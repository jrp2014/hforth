module HForth where

import Control.Concurrent {- base -}
import Control.Monad {- base -}
import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}
import Data.Char {- base -}
import Data.Hashable {- hashable -}
import Data.List {- base -}
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import System.Directory {- directory -}
import System.Environment {- base -}
import System.Exit {- base -}
import System.FilePath {- filepath -}
import System.IO {- base -}
import qualified System.Posix.Signals as P {- unix -}

-- * Virtual Machine

-- | A dictionary is a map of named instructions ('Forth's).
type Dict w a = M.Map String (Forth w a ())

-- | Class of values that can constitute a 'Forth'.
class Forth_Type a where
    ty_show :: a -> String -- ^ String representation of /a/, pretty printer.
    ty_to_int :: a -> Maybe Int -- ^ Coercion, ie. for loop counters.
    ty_from_int :: Int -> a -- ^ Coercion
    ty_from_bool :: Bool -> a -- ^ Boolean value represented in /a/, by convention @-1@ and @0@.

ty_to_int' :: Forth_Type a => String -> a -> Int
ty_to_int' msg = fromMaybe (error ("NOT-INTEGER: " ++ msg)) . ty_to_int

instance Forth_Type Integer where
    ty_show = show
    ty_to_int = Just . fromInteger
    ty_from_int = fromIntegral
    ty_from_bool t = if t then -1 else 0

-- | A data cell, for the data stacks.
data DC a = DC a | DC_String String | DC_XT String

instance Forth_Type a => Show (DC a) where
    show dc =
        case dc of
          DC a -> ty_show a
          DC_String str -> "STRING:" ++ tick_quotes str
          DC_XT str -> "XT:" ++ str

-- | Extract plain value from 'DC', else error.
dc_plain :: DC a -> Forth w a a
dc_plain dc =
    case dc of
      DC a -> return a
      _ -> throw_error "DC-NOT-VALUE-CELL"

-- | A compilation cell, for the compilation stack.
data CC w a = CC_Word String | CC_Forth (Forth w a ())

-- | Predicate to see if 'CC' is a particular 'CC_Word'.
cc_is_word :: String -> CC w a -> Bool
cc_is_word w cw =
    case cw of
      CC_Word w' -> w == w'
      _ -> False

-- | The machine is either interpreting or compiling.
data VM_Mode = Interpret | Compile deriving (Eq,Show)

-- | The machine, /w/ is the type of the world, /a/ is the type of the stack elements.
data VM w a =
    VM {stack :: [DC a] -- ^ The data stack, /the/ stack.
       ,rstack :: [DC a] -- ^ The return stack.
       ,cstack :: [CC w a] -- ^ The compilation stack.
       ,threads :: M.Map Int ThreadId
       ,dict :: Dict w a -- ^ The dictionary.
       ,locals :: [Dict w a] -- ^ The stack of locals dictionaries.
       ,buffer :: String -- ^ The current line of input text.
       ,mode :: VM_Mode -- ^ Basic state of the machine.
       ,world :: w -- ^ The world, instance state.
       ,literal :: String -> Maybe a -- ^ Read function for literal values.
       ,dynamic :: Maybe (String -> Forth w a ()) -- ^ Dynamic post-dictionary lookup.
       ,input_port :: Maybe Handle
       ,tracing :: Int
       ,sigint :: MVar Bool -- ^ True if a SIGINT signal (user interrupt) has been received.
       }

instance Forth_Type a => Show (VM w a) where
    show vm = concat
              ["\n DATA STACK: ",unwords (map show (stack vm))
              ,"\n RETURN STACK: ",unwords (map show (rstack vm))
              ,"\n COMPILE STACK DEPTH: ",show (length (cstack vm))
              ,"\n THREADS: ",intercalate "," (map show (M.keys (threads vm)))
              ,"\n DICT: ",unwords (M.keys (dict vm))
              ,"\n LOCALS: ",intercalate "," (map (unwords . M.keys) (locals vm))
              ,"\n BUFFER: ",buffer vm
              ,"\n MODE: ",show (mode vm)
              ,"\n DYMAMIC: ",maybe "NO" (const "YES") (dynamic vm)
              ,"\n INPUT PORT: ",maybe "NO" (const "YES") (input_port vm)
              ,"\n TRACING: ",show (tracing vm)
              ]

-- | Signals (exceptions) from 'VM'.
data VM_Signal = VM_EOF | VM_No_Input | VM_Error String deriving (Eq,Show)

-- | An instruction, the implementation of a /word/.
type Forth w a r = ExceptT VM_Signal (StateT (VM w a) IO) r

-- | Make an empty (initial) machine.
empty_vm :: w -> (String -> Maybe a) -> MVar Bool -> VM w a
empty_vm w lit sig =
    VM {stack = []
       ,rstack = []
       ,cstack = []
       ,threads = M.empty
       ,buffer = ""
       ,mode = Interpret
       ,dict = M.empty
       ,locals = []
       ,world = w
       ,literal = lit
       ,dynamic = Nothing
       ,input_port = Nothing
       ,tracing = -1
       ,sigint = sig}

-- | Reset 'VM', on error.
vm_reset :: VM w a -> VM w a
vm_reset vm =
    vm {stack = []
       ,rstack = []
       ,cstack = []
       ,buffer = ""
       ,mode = Interpret
       ,locals = []}

-- | Type specialised variant of 'get' that checks SIGINT handler.
get_vm :: Forth w a (VM w a)
get_vm = do
  vm <- get
  sig <- liftIO (modifyMVar (sigint vm) (\s -> return (False,s)))
  when sig (throw_error "VM: SIGINT")
  return vm

-- | Function with 'VM'.
with_vm :: (VM w a -> (VM w a,r)) -> Forth w a r
with_vm f = get_vm >>= \vm -> let (vm',r) = f vm in put vm' >> return r

-- | Procedure with 'VM'.
do_with_vm :: (VM w a -> Forth w a (VM w a)) -> Forth w a ()
do_with_vm f = get_vm >>= \vm -> f vm >>= put

-- | Change the world.
vm_modify_world :: (w -> w) -> Forth w a ()
vm_modify_world f = modify (\vm -> vm {world = f (world vm)})

-- * Error

-- | Tracer, levels are 0 = HIGH, 1 = MEDIUM, 2 = LOW
trace :: Int -> String -> Forth w a ()
trace k msg = do
  vm <- get_vm
  when (k <= tracing vm) (write_ln msg)

throw_error :: String -> Forth w a r
throw_error = throwError . VM_Error

-- | Reader that raises an /unknown word/ error.
unknown_error :: String -> Forth w a r
unknown_error s = throw_error ("UNKNOWN WORD: " ++ tick_quotes s)

-- * Stack

push' :: DC a -> Forth w a ()
push' x = modify (\vm -> vm {stack = x : stack vm})

-- | Push value onto 'stack'.
push :: a -> Forth w a ()
push = push' . DC

pushr' :: DC a -> Forth w a ()
pushr' x = modify (\vm -> vm {rstack = x : rstack vm})

-- | Push value onto 'rstack'.
pushr :: a -> Forth w a ()
pushr = pushr' . DC

-- | Push value onto 'cstack'.
pushc :: CC w a -> Forth w a ()
pushc x = modify (\vm -> vm {cstack = x : cstack vm})

-- | Pop indicated 'VM' stack.
pop_vm_stack :: String -> (VM w a -> [r]) -> (VM w a -> [r] -> VM w a) -> Forth w a r
pop_vm_stack nm f g = do
  vm <- get_vm
  case f vm of
    [] -> throw_error (nm ++ ": STACK UNDERFLOW")
    x:xs -> put (g vm xs) >> return x

pop' :: Forth w a (DC a)
pop' = pop_vm_stack "DATA" stack (\vm s -> vm {stack = s})

-- | Remove value from 'stack'.
pop :: Forth w a a
pop = pop' >>= dc_plain

pop_int :: Forth_Type a => String -> Forth w a Int
pop_int msg = pop >>= return . ty_to_int' msg

popr' :: Forth w a (DC a)
popr' = pop_vm_stack "RETURN" rstack (\vm s -> vm {rstack = s})

-- | Remove value from 'rstack'.
popr :: Forth w a a
popr = popr' >>= dc_plain

-- | Remove value from 'cstack'.
popc :: Forth w a (CC w a)
popc = pop_vm_stack "COMPILE" cstack (\vm s -> vm {cstack = s})

-- | ( id len -- )
pop_string :: String -> Forth w a String
pop_string msg = do
  vm <- get_vm
  case stack vm of
    DC _ : DC_String str : s' -> put vm {stack = s'} >> return str
    _ -> throw_error ("NOT-STRING?" ++ msg)

-- * Token / Expr

-- | Expressions are either literals or words.
data Expr a = Literal a | Word String deriving (Show,Eq)

-- | Pretty print 'Expr'.
expr_pp :: Forth_Type a => Expr a -> String
expr_pp e =
    case e of
      Literal a -> ty_show a
      Word nm -> nm

-- | Dictionary lookup, word should be lower case.
lookup_word :: String -> VM w a -> Maybe (Forth w a ())
lookup_word k vm =
    case locals vm of
      [] -> M.lookup k (dict vm)
      l:_ -> case M.lookup k l of
               Nothing -> M.lookup k (dict vm)
               r -> r

-- | Parse a token string to an expression.
parse_token :: String -> Forth w a (Expr a)
parse_token s = do
  vm <- get_vm
  case lookup_word s vm of
    Just _  -> return (Word s)
    Nothing ->
        case literal vm s of
          Just l  -> return (Literal l)
          Nothing ->
              case dynamic vm of
                Just _ -> return (Word s) -- if there is a dynamic reader, defer...
                Nothing -> unknown_error s

-- | Read buffer until predicate holds, if /pre/ delete preceding white space.
read_until :: Bool -> (Char -> Bool) -> Forth w a (String,String)
read_until pre cf = do
  vm <- get_vm
  let f = if pre then dropWhile isSpace else id
      r = break_on cf (f (buffer vm))
  trace 2 (show ("READ_UNTIL",mode vm,fst r,length (snd r)))
  put vm {buffer = snd r}
  return r

scan_until :: (Char -> Bool) -> Forth w a String
scan_until = fmap fst . read_until False

-- | Scan a token from 'buffer', ANS Forth type comments are
-- discarded.  Although 'buffer' is filled by 'hGetLine' it may
-- contain newline characters because we may include a file.
scan_token :: Forth w a (Maybe String)
scan_token = do
  r <- read_until True isSpace
  case r of
    ([],[]) -> write_ln " OK" >> return Nothing
    ([],rhs) -> throw_error ("SCAN_TOKEN: NULL: " ++ rhs)
    ("\\",_) -> scan_until (== '\n') >> scan_token
    ("(",_) -> scan_until (== ')') >> scan_token
    (e,_) -> return (Just e)

-- | Read line from 'input_port' to 'buffer'.  There are two
-- /exceptions/ thrown here, 'VM_EOF' if an input port is given but
-- returns EOF, and 'VM_No_Input' if there is no input port.
fw_refill :: Forth w a ()
fw_refill = do
  vm <- get_vm
  case input_port vm of
    Nothing -> throwError VM_No_Input
    Just h -> do
      eof <- liftIO (hIsEOF h)
      when eof (throwError VM_EOF)
      trace 2 "REFILL"
      x <- liftIO (hGetLine h)
      put (vm {buffer = x})

-- | If 'scan_token' is 'Nothing', then 'fw_refill' and retry.  Tokens are lower case.
read_token :: Forth w a String
read_token = do
  r <- scan_token
  case r of
    Just str -> return (map toLower str)
    Nothing -> fw_refill >> read_token

-- | 'parse_token' of 'read_token'.
read_expr :: Forth w a (Expr a)
read_expr = parse_token =<< read_token

-- * Interpret

-- | 'lookup_word' in the dictionary, if unknown try 'dynamic', if
-- dynamic gives a word then add it to the dictionary.
interpret_word :: String -> Forth w a ()
interpret_word w = do
  vm <- get_vm
  case lookup_word w vm of
    Just r -> r
    Nothing ->
        case dynamic vm of
          Just f -> let d_r = f w in put vm {dict = M.insert w d_r (dict vm)} >> d_r
          Nothing -> throw_error ("UNKNOWN WORD: " ++ tick_quotes w)

-- | Either 'interpret_word' or 'push' literal.
interpret_expr :: Expr a -> Forth w a ()
interpret_expr e =
    case e of
      Word w -> interpret_word w
      Literal a -> push a

-- | 'interpret_expr' of 'read_expr'.
vm_interpret :: Forth w a ()
vm_interpret = read_expr >>= interpret_expr

-- * Compile

-- | Define word and add to dictionary.  The only control structures are /if/ and /do/.
vm_compile :: (Eq a,Forth_Type a) => Forth w a ()
vm_compile = do
  expr <- read_expr
  trace 2 ("COMPILE: " ++ expr_pp expr)
  case expr of
    Word ";" -> fw_semi_colon
    Word ":" -> throw_error ": IN COMPILE CONTEXT"
    Word "do" -> pushc (CC_Word "do")
    Word "i" -> pushc (CC_Forth fw_i)
    Word "j" -> pushc (CC_Forth fw_j)
    Word "loop" -> fw_loop
    Word "if" -> pushc (CC_Word "if")
    Word "else" -> pushc (CC_Word "else")
    Word "then" -> fw_then
    Word "{" -> fw_open_brace
    Word "s\"" -> fw_s_quote_compiler
    e -> pushc (CC_Forth (interpret_expr e))

-- | Get instruction at 'CC' or raise an error.
cw_instr :: CC w a -> Forth w a ()
cw_instr cw =
    case cw of
      CC_Word w -> throw_error ("cw_instr: WORD: " ++ w)
      CC_Forth f -> f

-- | Type specialised 'foldl1' of '>>'.
forth_block :: [Forth w a ()] -> Forth w a ()
forth_block = foldl1 (>>)

-- | Add a 'locals' frame.
begin_locals :: Forth w a ()
begin_locals = with_vm (\vm -> (vm {locals = M.empty : locals vm},()))

-- | Remove a 'locals' frame.
end_locals :: Forth w a ()
end_locals = with_vm (\vm -> (vm {locals = tail (locals vm)},()))

-- | Unwind the 'cstack' to the indicated control word.  The result is
-- the code block, in sequence.  The control word is also removed from
-- the cstack.
unwind_cstack_to :: String -> Forth w a [CC w a]
unwind_cstack_to w = do
  with_vm (\vm -> let (r,c) = break (cc_is_word w) (cstack vm)
                  in (vm {cstack = tail c},reverse r))

-- | Either 'vm_interpret' or 'vm_compile', depending on 'mode'.
vm_execute :: (Eq a,Forth_Type a) => Forth w a ()
vm_execute = do
  vm <- get_vm
  case mode vm of
    Interpret -> vm_interpret
    Compile -> vm_compile

vm_execute_buffer :: (Forth_Type a, Eq a) => VM w a -> IO (VM w a)
vm_execute_buffer vm = do
  (r,vm') <- runStateT (runExceptT vm_execute) vm
  case r of
    Left err -> case err of
                  VM_No_Input -> return vm'
                  _ -> error ("VM_EXECUTE_BUFFER: " ++ show err)
    Right () -> vm_execute_buffer vm'

-- * DO LOOP

-- | A loop ends when the two elements at the top of the rstack are equal.
loop_end :: Eq a => Forth w a Bool
loop_end = do
  vm <- get_vm
  case rstack vm of
    DC p : DC q : _ -> return (p == q)
    _ -> throw_error "LOOP-END: ILLEGAL RSTACK"

-- | /code/ is the expressions between @do@ and @loop@.
interpret_do_loop :: (Forth_Type a,Eq a) => Forth w a () -> Forth w a ()
interpret_do_loop code = do
  start <- pop
  end <- pop
  pushr end
  pushr start
  let step = do
        code
        i <- popr
        let i' = ty_from_int (ty_to_int' "DO-LOOP: I" i + 1)
        pushr i'
  let loop = do
        r <- loop_end
        if not r then step >> loop else popr >> popr >> return ()
  loop

-- | Compile @loop@ statement, end of do block.
fw_loop :: (Eq a,Forth_Type a) => Forth w a ()
fw_loop = do
  cw <- unwind_cstack_to "do"
  let w = forth_block (map cw_instr cw)
  pushc (CC_Forth (interpret_do_loop w))

-- * IF ELSE THEN

-- | Consult stack and select either /true/ or /false/.
interpret_if :: (Eq a,Forth_Type a) => (Forth w a (),Forth w a ()) -> Forth w a ()
interpret_if (t,f) = pop >>= \x -> if x /= ty_from_bool False then t else f

-- | Compile @then@ statement, end of @if@ block.
fw_then :: (Eq a,Forth_Type a) => Forth w a ()
fw_then = do
  cw <- unwind_cstack_to "if"
  let f = forth_block . map cw_instr
  case break (cc_is_word "else") cw of
    (tb,[]) -> pushc (CC_Forth (interpret_if (f tb,return ())))
    (tb,fb) -> pushc (CC_Forth (interpret_if (f tb,f (tail fb))))

-- * LOCALS

-- | Variant on @(local)@, argument not on stack.
fw_local' :: String -> Forth w a ()
fw_local' nm = do
  vm <- get_vm
  case stack vm of
    e : s' -> put vm {stack = s'
                     ,locals = case locals vm of
                                 [] -> error "NO LOCALS FRAME"
                                 l : l' -> M.insert nm (push' e) l : l'}
    _ -> throw_error ("(LOCAL): STACK UNDERFLOW: " ++ nm)

-- | Function over current locals 'Dict'.
at_current_locals :: (Dict w a -> Dict w a) -> VM w a -> VM w a
at_current_locals f vm =
    case locals vm of
      l : l' -> vm {locals = f l : l'}
      _ -> error "AT_CURRENT_LOCALS"

-- | 'locals' is used both during compilation and interpretation.  In
-- compilation the RHS is undefined, it is used for name lookup and to
-- know if an interpreter 'locals' frame must be made.  In
-- interpretation, if required, it is a secondary dictionary,
-- consulted first.
fw_open_brace :: Forth_Type a => Forth w a ()
fw_open_brace = do
  let get_names r = do
               w <- read_token
               if w == "}" then return r else get_names (w : r)
  nm <- get_names []
  when (any is_reserved_word nm) (throw_error ("FW_OPEN_BRACE: RESERVED WORD: " ++ unwords nm))
  trace 0 ("DEFINE-LOCALS: " ++ intercalate " " nm)
  let locals' = M.fromList (zip nm (repeat undefined))
  with_vm (\vm -> (at_current_locals (M.union locals') vm,()))
  pushc (CC_Forth (forth_block (map fw_local' nm)))

-- * Compiler

-- | ":". Enter compile phase, the word name is pushed onto the
-- /empty/ 'cstack', and a 'locals' frame is added.
fw_colon :: Forth w a ()
fw_colon = do
  nm <- read_token
  trace 0 ("DEFINE: " ++ nm)
  let edit vm = do
        when (is_reserved_word nm) (throw_error ("':' RESERVED NAME: " ++ nm))
        when (not (null (cstack vm))) (throw_error ("':' CSTACK NOT EMPTY: " ++ nm))
        return (vm {mode = Compile
                   ,cstack = [CC_Word nm]
                   ,locals = M.empty : locals vm})
  do_with_vm edit

-- | ";".  End compile phase.  There is always a compile 'locals'
-- frame to be removed.
fw_semi_colon :: Forth w a ()
fw_semi_colon = do
  vm <- get_vm
  case reverse (cstack vm) of
    CC_Word nm : cw ->
        let instr = (map cw_instr cw)
            instr' = if M.null (head (locals vm))
                     then instr
                     else bracketed (begin_locals,end_locals) instr
            w = forth_block instr'
        in do trace 2 ("END DEFINITION: " ++ nm)
              when (M.member nm (dict vm)) (write_sp ("REDEFINED " ++ nm))
              put (vm {cstack = []
                      ,locals = tail (locals vm)
                      ,dict = M.insert nm w (dict vm)
                      ,mode = Interpret})
    _ -> throw_error "CSTACK"
  return ()

-- * STRINGS

fw_s_quote_compiler :: Forth_Type a => Forth w a ()
fw_s_quote_compiler = do
  str <- scan_until (== '"')
  trace 2 ("COMPILE: S\": \"" ++ str ++ "\"")
  pushc (CC_Forth (push_str str))

fw_s_quote_interpet :: Forth_Type a => Forth w a ()
fw_s_quote_interpet = scan_until (== '"') >>=  push_str

fw_type :: Forth w a ()
fw_type = pop_string "TYPE" >>= write

-- * Forth words

-- | Store current buffer & input port, place input string on buffer
-- with no input port, 'vm_execute_buffer', restore buffer & port.
fw_evaluate' :: (Eq a,Forth_Type a) => String -> Forth w a ()
fw_evaluate' str = do
  vm <- get_vm
  let buf = buffer vm
      ip = input_port vm
  vm' <- liftIO (vm_execute_buffer (vm {buffer = str, input_port = Nothing}))
  put (vm' {buffer = buf, input_port = ip})

-- | Variant on @included@, argument not on stack.
fw_included' :: (Eq a,Forth_Type a) => FilePath -> Forth w a ()
fw_included' nm = do
  trace 0 ("INCLUDED: " ++ nm)
  x <- liftIO (doesFileExist nm)
  when (not x) (throw_error ("INCLUDED': FILE MISSING: " ++ tick_quotes nm))
  liftIO (readFile nm) >>= fw_evaluate'

fw_included :: (Eq a,Forth_Type a) => Forth w a ()
fw_included = pop_string "INCLUDED" >>= fw_included'

fw_i :: Forth w a ()
fw_i = popr >>= \x -> pushr x >> push x

-- | Forth word @j@.
fw_j :: Forth w a ()
fw_j = do {x <- popr; y <- popr; z <- popr
          ;pushr z; pushr y; pushr x
          ;push z}

-- | dup : ( p -- p p ) swap : ( p q -- q p ) drop : ( p -- ) over : (
-- p q -- p q p ) rot : ( p q r -- q r p ) 2dup : ( p q -- p q p q )
fw_dup,fw_swap,fw_drop,fw_over,fw_rot,fw_2dup :: Forth w a ()
fw_dup = pop' >>= \e -> push' e >> push' e
fw_swap = pop' >>= \p -> pop' >>= \q -> push' p >> push' q
fw_drop = pop' >> return ()
fw_over = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q
fw_rot = pop' >>= \p -> pop' >>= \q -> pop' >>= \r -> push' q >> push' p >> push' r
fw_2dup = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q >> push' p

-- | ( xu ... x1 x0 u -- xu ... x1 x0 xu )
fw_pick :: Forth_Type a => Forth w a ()
fw_pick = do
  vm <- get_vm
  case stack vm of
    DC n : s' -> let n' = ty_to_int' "PICK" n
                     e = s' !! n'
                 in put vm {stack = e : s'}
    _ -> throw_error "PICK"

write,write_ln,write_sp :: String -> Forth w a ()
write = liftIO . putStr
write_ln = write . (++ "\n")
write_sp = write . (++ " ")

fw_emit,fw_dot :: Forth_Type a => Forth w a ()
fw_emit = write . return . toEnum =<< pop_int "EMIT"
fw_dot = write_sp . show =<< pop'

fw_dot_s :: Forth_Type a => Forth w a ()
fw_dot_s = do
  vm <- get_vm
  let l = map show (reverse (stack vm))
      n = "<" ++ show (length l) ++ "> "
  write (n ++ concatMap (++ " ") l)

fw_bye :: Forth w a ()
fw_bye = liftIO exitSuccess

push_str :: Forth_Type a => String -> Forth w a ()
push_str str =
    let f vm = (vm {stack = DC (ty_from_int (length str)) : DC_String str : stack vm},())
    in with_vm f

fw_vmstat :: Forth_Type a => Forth w a ()
fw_vmstat = get_vm >>= write_ln . show

fw_fork :: Forth_Type a => Forth w a ()
fw_fork = do
  nm <- read_token
  vm <- get_vm
  case lookup_word nm vm of
    Just fw -> do th <- liftIO (forkIO (exec_err vm fw >> return ()))
                  let k = hash th :: Int
                  put vm {stack = DC (ty_from_int k) : stack vm
                         ,threads = M.insert k th (threads vm)}
    Nothing -> throw_error ("FORK: UNKNOWN WORD: " ++ nm)

fw_kill :: Forth_Type a => Forth w a ()
fw_kill = do
  k <- pop_int "KILL: PID?"
  vm <- get_vm
  let threads' = threads vm
  case M.lookup k threads' of
    Nothing -> throw_error ("KILL: UNKNOWN THREAD: " ++ show k)
    Just th -> liftIO (killThread th) >> put vm {threads = M.delete k threads'}

fw_kill_all :: Forth w a ()
fw_kill_all = do
  vm <- get_vm
  let th = M.elems (threads vm)
  liftIO (mapM_ killThread th)
  put vm {threads = M.empty}

fw_quote :: Forth w a ()
fw_quote = do
  tok <- read_token
  push' (DC_XT tok)

fw_execute :: Forth w a ()
fw_execute = do
  c <- pop'
  case c of
    DC_XT xt -> interpret_word xt
    _ -> throw_error "EXECUTE: NOT EXECUTION TOKEN"

-- * Dictionaries

core_dict :: (Eq a,Forth_Type a) => Dict w a
core_dict =
    let err nm = throw_error (concat [tick_quotes nm,": compiler word in interpeter context"])
    in M.fromList
    [(":",fw_colon)
    ,(";",err ";")
    ,("s\"",fw_s_quote_interpet)
    ,("included",fw_included)
    ,("type",fw_type)
    ,("do",err "do")
    ,("i",err "i")
    ,("j",err "j")
    ,("loop",err "loop")
    ,("if",err "if")
    ,("else",err "else")
    ,("then",err "then")
    ,("{",err "{")
    ,("}",err "}")
    ,("'",fw_quote)
    ,("execute",fw_execute)
    ,("fork",fw_fork)
    ,("kill",fw_kill)
    ,("killall",fw_kill_all)
    ,("bye",fw_bye)
    -- STACK
    ,("drop",fw_drop)
    ,("dup",fw_dup)
    ,("over",fw_over)
    ,("pick",fw_pick)
    ,("rot",fw_rot)
    ,("swap",fw_swap)
    ,("2dup",fw_2dup)
    ,(">r",pop' >>= pushr')
    ,("r>",popr' >>= push')
     -- IO
    ,("emit",fw_emit)
    ,(".",fw_dot)
    ,(".s",fw_dot_s)
    ,("key",liftIO getChar >>= \c -> push (ty_from_int (fromEnum c)))
    -- DEBUG
    ,("vmstat",fw_vmstat)
    ,("trace",pop >>= \k -> with_vm (\vm -> (vm {tracing = ty_to_int' "TRACE" k},())))]

core_words :: [String]
core_words = M.keys (core_dict :: Dict w Integer)

is_reserved_word :: String -> Bool
is_reserved_word nm = nm `elem` core_words

-- * Operation

exec_err :: VM w a -> Forth w a () -> IO (VM w a)
exec_err vm fw = do
  (r,vm') <- runStateT (runExceptT fw) vm
  case r of
    Left err -> error ("EXEC_ERR: " ++ show err)
    Right () -> return vm'

-- | Read, evaluate, print, loop.  Prints @OK@ at end of line.  Prints
-- error message and runs 'vm_reset' on error.
repl' :: (Eq a,Forth_Type a) => VM w a -> IO ()
repl' vm = do
  (r,vm') <- runStateT (runExceptT vm_execute) vm
  case r of
    Left err -> case err of
                  VM_EOF -> putStrLn "BYE" >> liftIO exitSuccess
                  VM_No_Input -> liftIO exitSuccess
                  VM_Error msg -> putStrLn (" ERROR: " ++ msg) >> repl' (vm_reset vm)
    Right () -> repl' vm'

catch_sigint :: VM w a -> IO ()
catch_sigint vm = do
  let h = modifyMVar_ (sigint vm) (return . const True)
  _ <- P.installHandler P.sigINT (P.Catch h) Nothing
  _ <- P.installHandler P.sigTERM (P.Catch h) Nothing
  return ()

-- | 'repl'' but with 'catch_sigint'.
repl :: (Forth_Type a, Eq a) => VM w a -> Forth w a () -> IO ()
repl vm init_f = do
  catch_sigint vm
  (_,vm') <- runStateT (runExceptT init_f) vm
  repl' vm'

load_files :: (Eq a,Forth_Type a) => [String] -> Forth w a ()
load_files nm = do
  trace 0 ("LOAD-FILES: " ++ intercalate "," nm)
  r <- liftIO (lookupEnv "HSC3_FORTH_DIR")
  case r of
    Nothing -> throw_error "HSC3_FORTH_DIR NOT SET"
    Just dir -> mapM_ fw_included' (map (dir </>) nm)

-- * List functions

-- | Read until /f/ is 'True', discarding /x/, RHS may be @[]@.
--
-- > break_on isSpace "" == ([],[])
-- > break_on (== ')') "comment ) WORD" == ("comment "," WORD")
-- > break_on (== '\n') " comment\n\n" == (" comment","\n")
break_on :: (a -> Bool) -> [a] -> ([a],[a])
break_on f l =
    case break f l of
      (lhs,[]) -> (lhs,[])
      (lhs,_ : rhs) -> (lhs,rhs)

-- | 'snd' of 'break_on'.
delete_until :: (a -> Bool) -> [a] -> [a]
delete_until f = snd . break_on f

bracketed :: (a,a) -> [a] -> [a]
bracketed (l,r) x = l : x ++ [r]

tick_quotes :: String -> String
tick_quotes = bracketed ('\'','\'')
