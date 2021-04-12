{-# language DerivingStrategies #-}
module HForth where

import Control.Concurrent
    ( modifyMVar, modifyMVar_, forkIO, killThread, ThreadId, MVar ) {- base -}
import Control.Monad ( unless, void, when, (>=>) ) {- base -}
import Control.Monad.State
    ( modify,
      MonadIO(liftIO),
      MonadState(put, get),
      StateT(runStateT) ) {- mtl -}
import qualified Control.Monad.Except as CME {- mtl -}
import Data.Char ( isSpace, toLower ) {- base -}
import Data.Hashable ( Hashable(hash) ) {- hashable -}
import Data.List ( intercalate ) {- base -}
import qualified Data.Map as M {- containers -}
import Data.Maybe ( fromMaybe ) {- base -}
import System.Directory ( doesFileExist ) {- directory -}
import System.Environment ( lookupEnv ) {- base -}
import System.Exit ( exitSuccess ) {- base -}
import System.FilePath ( (</>) ) {- filepath -}
import System.IO ( Handle, hIsEOF, hGetLine ) {- base -}
import qualified System.Posix.Signals as P {- unix -}

-- * Virtual Machine

-- | A dictionary is a map of named instructions ('Forth's).
type Dict w a = M.Map String (Forth w a ())

-- | Class of values that can constitute a 'Forth'.
class ForthType a where
    tyShow :: a -> String -- ^ String representation of /a/, pretty printer.
    tyToInt :: a -> Maybe Int -- ^ Coercion, ie. for loop counters.
    tyFromInt :: Int -> a -- ^ Coercion
    tyFromBool :: Bool -> a -- ^ Boolean value represented in /a/, by convention @-1@ and @0@.

tyToInt' :: ForthType a => String -> a -> Int
tyToInt' msg = fromMaybe (error ("NOT-INTEGER: " ++ msg)) . tyToInt

instance ForthType Integer where
    tyShow = show
    tyToInt = Just . fromInteger
    tyFromInt = fromIntegral
    tyFromBool t = if t then -1 else 0

-- | A data cell, for the data stacks.
data DC a = DC a | DCString String | DCXT String

instance ForthType a => Show (DC a) where
    show dc =
        case dc of
          DC a -> tyShow a
          DCString str -> "STRING:" ++ tickQuotes str
          DCXT str -> "XT:" ++ str

-- | Extract plain value from 'DC', else error.
dcPlain :: DC a -> Forth w a a
dcPlain dc =
    case dc of
      DC a -> return a
      _ -> throwError "DC-NOT-VALUE-CELL"

-- | A compilation cell, for the compilation stack.
data CC w a = CCWord String | CCForth (Forth w a ())

-- | Predicate to see if 'CC' is a particular 'CCWord'.
ccIsWord :: String -> CC w a -> Bool
ccIsWord w cw =
    case cw of
      CCWord w' -> w == w'
      _ -> False

-- | The machine is either interpreting or compiling.
data VMMode = Interpret | Compile deriving stock (Eq,Show)

-- | The machine, /w/ is the type of the world, /a/ is the type of the stack elements.
data VM w a =
    VM {stack :: [DC a] -- ^ The data stack, /the/ stack.
       ,rstack :: [DC a] -- ^ The return stack.
       ,cstack :: [CC w a] -- ^ The compilation stack.
       ,threads :: M.Map Int ThreadId
       ,dict :: Dict w a -- ^ The dictionary.
       ,locals :: [Dict w a] -- ^ The stack of locals dictionaries.
       ,buffer :: String -- ^ The current line of input text.
       ,mode :: VMMode -- ^ Basic state of the machine.
       ,world :: w -- ^ The world, instance state.
       ,literal :: String -> Maybe a -- ^ Read function for literal values.
       ,dynamic :: Maybe (String -> Forth w a ()) -- ^ Dynamic post-dictionary lookup.
       ,inputPort :: Maybe Handle
       ,tracing :: Int
       ,sigint :: MVar Bool -- ^ True if a SIGINT signal (user interrupt) has been received.
       }

instance ForthType a => Show (VM w a) where
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
              ,"\n INPUT PORT: ",maybe "NO" (const "YES") (inputPort vm)
              ,"\n TRACING: ",show (tracing vm)
              ]

-- | Signals (exceptions) from 'VM'.
data VMSignal = VMEOF | VMNoInput | VMError String deriving stock (Eq,Show)

-- | An instruction, the implementation of a /word/.
type Forth w a r = CME.ExceptT VMSignal (StateT (VM w a) IO) r

-- | Make an empty (initial) machine.
emptyVm :: w -> (String -> Maybe a) -> MVar Bool -> VM w a
emptyVm w lit sig =
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
       ,inputPort = Nothing
       ,tracing = -1
       ,sigint = sig}

-- | Reset 'VM', on error.
vmReset :: VM w a -> VM w a
vmReset vm =
    vm {stack = []
       ,rstack = []
       ,cstack = []
       ,buffer = ""
       ,mode = Interpret
       ,locals = []}

-- | Type specialised variant of 'get' that checks SIGINT handler.
getVm :: Forth w a (VM w a)
getVm = do
  vm <- get
  sig <- liftIO (modifyMVar (sigint vm) (\s -> return (False,s)))
  when sig (throwError "VM: SIGINT")
  return vm

-- | Function with 'VM'.
withVm :: (VM w a -> (VM w a,r)) -> Forth w a r
withVm f = getVm >>= \vm -> let (vm',r) = f vm in put vm' >> return r

-- | Procedure with 'VM'.
doWithVm :: (VM w a -> Forth w a (VM w a)) -> Forth w a ()
doWithVm f = getVm >>= (f >=> put)

-- | Change the world.
vmModifyWorld :: (w -> w) -> Forth w a ()
vmModifyWorld f = modify (\vm -> vm {world = f (world vm)})

-- * Error

-- | Tracer, levels are 0 = HIGH, 1 = MEDIUM, 2 = LOW
trace :: Int -> String -> Forth w a ()
trace k msg = do
  vm <- getVm
  when (k <= tracing vm) (writeLn msg)

throwError :: String -> Forth w a r
throwError = CME.throwError . VMError

-- | Reader that raises an /unknown word/ error.
unknownError :: String -> Forth w a r
unknownError s = throwError ("UNKNOWN WORD: " ++ tickQuotes s)

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
popVmStack :: String -> (VM w a -> [r]) -> (VM w a -> [r] -> VM w a) -> Forth w a r
popVmStack nm f g = do
  vm <- getVm
  case f vm of
    [] -> throwError (nm ++ ": STACK UNDERFLOW")
    x:xs -> put (g vm xs) >> return x

pop' :: Forth w a (DC a)
pop' = popVmStack "DATA" stack (\vm s -> vm {stack = s})

-- | Remove value from 'stack'.
pop :: Forth w a a
pop = pop' >>= dcPlain

popInt :: ForthType a => String -> Forth w a Int
--popInt msg =  pop <&> tyToInt' msg
popInt msg =  tyToInt' msg <$> pop

popr' :: Forth w a (DC a)
popr' = popVmStack "RETURN" rstack (\vm s -> vm {rstack = s})

-- | Remove value from 'rstack'.
popr :: Forth w a a
popr = popr' >>= dcPlain

-- | Remove value from 'cstack'.
popc :: Forth w a (CC w a)
popc = popVmStack "COMPILE" cstack (\vm s -> vm {cstack = s})

-- | ( id len -- )
popString :: String -> Forth w a String
popString msg = do
  vm <- getVm
  case stack vm of
    DC _ : DCString str : s' -> put vm {stack = s'} >> return str
    _ -> throwError ("NOT-STRING?" ++ msg)

-- * Token / Expr

-- | Expressions are either literals or words.
data Expr a = Literal a | Word String deriving stock (Show,Eq)

-- | Pretty print 'Expr'.
exprPp :: ForthType a => Expr a -> String
exprPp e =
    case e of
      Literal a -> tyShow a
      Word nm -> nm

-- | Dictionary lookup, word should be lower case.
lookupWord :: String -> VM w a -> Maybe (Forth w a ())
lookupWord k vm =
    case locals vm of
      [] -> M.lookup k (dict vm)
      l:_ -> case M.lookup k l of
               Nothing -> M.lookup k (dict vm)
               r -> r

-- | Parse a token string to an expression.
parseToken :: String -> Forth w a (Expr a)
parseToken s = do
  vm <- getVm
  case lookupWord s vm of
    Just _  -> return (Word s)
    Nothing ->
        case literal vm s of
          Just l  -> return (Literal l)
          Nothing ->
              case dynamic vm of
                Just _ -> return (Word s) -- if there is a dynamic reader, defer...
                Nothing -> unknownError s

-- | Read buffer until predicate holds, if /pre/ delete preceding white space.
readUntil :: Bool -> (Char -> Bool) -> Forth w a (String,String)
readUntil pre cf = do
  vm <- getVm
  let f = if pre then dropWhile isSpace else id
      r = breakOn cf (f (buffer vm))
  trace 2 (show ("READUNTIL",mode vm,fst r,length (snd r)))
  put vm {buffer = snd r}
  return r

scanUntil :: (Char -> Bool) -> Forth w a String
scanUntil = fmap fst . readUntil False

-- | Scan a token from 'buffer', ANS Forth type comments are
-- discarded.  Although 'buffer' is filled by 'hGetLine' it may
-- contain newline characters because we may include a file.
scanToken :: Forth w a (Maybe String)
scanToken = do
  r <- readUntil True isSpace
  case r of
    ([],[]) -> writeLn " OK" >> return Nothing
    ([],rhs) -> throwError ("SCANTOKEN: NULL: " ++ rhs)
    ("\\",_) -> scanUntil (== '\n') >> scanToken
    ("(",_) -> scanUntil (== ')') >> scanToken
    (e,_) -> return (Just e)

-- | Read line from 'inputPort' to 'buffer'.  There are two
-- /exceptions/ thrown here, 'VMEOF' if an input port is given but
-- returns EOF, and 'VMNoInput' if there is no input port.
fwRefill :: Forth w a ()
fwRefill = do
  vm <- getVm
  case inputPort vm of
    Nothing -> CME.throwError VMNoInput
    Just h -> do
      eof <- liftIO (hIsEOF h)
      when eof (CME.throwError VMEOF)
      trace 2 "REFILL"
      x <- liftIO (hGetLine h)
      put (vm {buffer = x})

-- | If 'scanToken' is 'Nothing', then 'fwRefill' and retry.  Tokens are lower case.
readToken :: Forth w a String
readToken = do
  r <- scanToken
  case r of
    Just str -> return (map toLower str)
    Nothing -> fwRefill >> readToken

-- | 'parseToken' of 'readToken'.
readExpr :: Forth w a (Expr a)
readExpr = parseToken =<< readToken

-- * Interpret

-- | 'lookupWord' in the dictionary, if unknown try 'dynamic', if
-- dynamic gives a word then add it to the dictionary.
interpretWord :: String -> Forth w a ()
interpretWord w = do
  vm <- getVm
  case lookupWord w vm of
    Just r -> r
    Nothing ->
        case dynamic vm of
          Just f -> let dR = f w in put vm {dict = M.insert w dR (dict vm)} >> dR
          Nothing -> throwError ("UNKNOWN WORD: " ++ tickQuotes w)

-- | Either 'interpretWord' or 'push' literal.
interpretExpr :: Expr a -> Forth w a ()
interpretExpr e =
    case e of
      Word w -> interpretWord w
      Literal a -> push a

-- | 'interpretExpr' of 'readExpr'.
vmInterpret :: Forth w a ()
vmInterpret = readExpr >>= interpretExpr

-- * Compile

-- | Define word and add to dictionary.  The only control structures are /if/ and /do/.
vmCompile :: (Eq a,ForthType a) => Forth w a ()
vmCompile = do
  expr <- readExpr
  trace 2 ("COMPILE: " ++ exprPp expr)
  case expr of
    Word ";" -> fwSemiColon
    Word ":" -> throwError ": IN COMPILE CONTEXT"
    Word "do" -> pushc (CCWord "do")
    Word "i" -> pushc (CCForth fwI)
    Word "j" -> pushc (CCForth fwJ)
    Word "loop" -> fwLoop
    Word "if" -> pushc (CCWord "if")
    Word "else" -> pushc (CCWord "else")
    Word "then" -> fwThen
    Word "{" -> fwOpenBrace
    Word "s\"" -> fwSQuoteCompiler
    e -> pushc (CCForth (interpretExpr e))

-- | Get instruction at 'CC' or raise an error.
cwInstr :: CC w a -> Forth w a ()
cwInstr cw =
    case cw of
      CCWord w -> throwError ("cwInstr: WORD: " ++ w)
      CCForth f -> f

-- | Type specialised 'foldl1' of '>>'.
forthBlock :: [Forth w a ()] -> Forth w a ()
forthBlock = foldl1 (>>)

-- | Add a 'locals' frame.
beginLocals :: Forth w a ()
beginLocals = withVm (\vm -> (vm {locals = M.empty : locals vm},()))

-- | Remove a 'locals' frame.
endLocals :: Forth w a ()
endLocals = withVm (\vm -> (vm {locals = tail (locals vm)},()))

-- | Unwind the 'cstack' to the indicated control word.  The result is
-- the code block, in sequence.  The control word is also removed from
-- the cstack.
unwindCstackTo :: String -> Forth w a [CC w a]
unwindCstackTo w = do
  withVm (\vm -> let (r,c) = break (ccIsWord w) (cstack vm)
                  in (vm {cstack = tail c},reverse r))

-- | Either 'vmInterpret' or 'vmCompile', depending on 'mode'.
vmExecute :: (Eq a,ForthType a) => Forth w a ()
vmExecute = do
  vm <- getVm
  case mode vm of
    Interpret -> vmInterpret
    Compile -> vmCompile

vmExecuteBuffer :: (ForthType a, Eq a) => VM w a -> IO (VM w a)
vmExecuteBuffer vm = do
  (r,vm') <- runStateT (CME.runExceptT vmExecute) vm
  case r of
    Left err -> case err of
                  VMNoInput -> return vm'
                  _ -> error ("VMEXECUTEBUFFER: " ++ show err)
    Right () -> vmExecuteBuffer vm'

-- * DO LOOP

-- | A loop ends when the two elements at the top of the rstack are equal.
loopEnd :: Eq a => Forth w a Bool
loopEnd = do
  vm <- getVm
  case rstack vm of
    DC p : DC q : _ -> return (p == q)
    _ -> throwError "LOOP-END: ILLEGAL RSTACK"

-- | /code/ is the expressions between @do@ and @loop@.
interpretDoLoop :: (ForthType a,Eq a) => Forth w a () -> Forth w a ()
interpretDoLoop code = do
  start <- pop
  end <- pop
  pushr end
  pushr start
  let step = do
        code
        i <- popr
        let i' = tyFromInt (tyToInt' "DO-LOOP: I" i + 1)
        pushr i'
  let loop = do
        r <- loopEnd
        if not r then step >> loop else popr >> popr >> return ()
  loop

-- | Compile @loop@ statement, end of do block.
fwLoop :: (Eq a,ForthType a) => Forth w a ()
fwLoop = do
  cw <- unwindCstackTo "do"
  let w = forthBlock (map cwInstr cw)
  pushc (CCForth (interpretDoLoop w))

-- * IF ELSE THEN

-- | Consult stack and select either /true/ or /false/.
interpretIf :: (Eq a,ForthType a) => (Forth w a (),Forth w a ()) -> Forth w a ()
interpretIf (t,f) = pop >>= \x -> if x /= tyFromBool False then t else f

-- | Compile @then@ statement, end of @if@ block.
fwThen :: (Eq a,ForthType a) => Forth w a ()
fwThen = do
  cw <- unwindCstackTo "if"
  let f = forthBlock . map cwInstr
  case break (ccIsWord "else") cw of
    (tb,[]) -> pushc (CCForth (interpretIf (f tb,return ())))
    (tb,fb) -> pushc (CCForth (interpretIf (f tb,f (tail fb))))

-- * LOCALS

-- | Variant on @(local)@, argument not on stack.
fwLocal' :: String -> Forth w a ()
fwLocal' nm = do
  vm <- getVm
  case stack vm of
    e : s' -> put vm {stack = s'
                     ,locals = case locals vm of
                                 [] -> error "NO LOCALS FRAME"
                                 l : l' -> M.insert nm (push' e) l : l'}
    _ -> throwError ("(LOCAL): STACK UNDERFLOW: " ++ nm)

-- | Function over current locals 'Dict'.
atCurrentLocals :: (Dict w a -> Dict w a) -> VM w a -> VM w a
atCurrentLocals f vm =
    case locals vm of
      l : l' -> vm {locals = f l : l'}
      _ -> error "ATCURRENTLOCALS"

-- | 'locals' is used both during compilation and interpretation.  In
-- compilation the RHS is undefined, it is used for name lookup and to
-- know if an interpreter 'locals' frame must be made.  In
-- interpretation, if required, it is a secondary dictionary,
-- consulted first.
--fwOpenBrace :: ForthType a => Forth w a ()
fwOpenBrace :: Forth w a ()
fwOpenBrace = do
  let getNames r = do
               w <- readToken
               if w == "}" then return r else getNames (w : r)
  nm <- getNames []
  when (any isReservedWord nm) (throwError ("FWOPENBRACE: RESERVED WORD: " ++ unwords nm))
  trace 0 ("DEFINE-LOCALS: " ++ unwords nm)
  let locals' = M.fromList (zip nm (repeat undefined))
  withVm (\vm -> (atCurrentLocals (M.union locals') vm,()))
  pushc (CCForth (forthBlock (map fwLocal' nm)))

-- * Compiler

-- | ":". Enter compile phase, the word name is pushed onto the
-- /empty/ 'cstack', and a 'locals' frame is added.
fwColon :: Forth w a ()
fwColon = do
  nm <- readToken
  trace 0 ("DEFINE: " ++ nm)
  let edit vm = do
        when (isReservedWord nm) (throwError ("':' RESERVED NAME: " ++ nm))
        unless (null (cstack vm)) (throwError ("':' CSTACK NOT EMPTY: " ++ nm))
        return (vm {mode = Compile
                   ,cstack = [CCWord nm]
                   ,locals = M.empty : locals vm})
  doWithVm edit

-- | ";".  End compile phase.  There is always a compile 'locals'
-- frame to be removed.
fwSemiColon :: Forth w a ()
fwSemiColon = do
  vm <- getVm
  case reverse (cstack vm) of
    CCWord nm : cw ->
        let instr = map cwInstr cw
            instr' = if M.null (head (locals vm))
                     then instr
                     else bracketed (beginLocals,endLocals) instr
            w = forthBlock instr'
        in do trace 2 ("END DEFINITION: " ++ nm)
              when (M.member nm (dict vm)) (writeSp ("REDEFINED " ++ nm))
              put (vm {cstack = []
                      ,locals = tail (locals vm)
                      ,dict = M.insert nm w (dict vm)
                      ,mode = Interpret})
    _ -> throwError "CSTACK"
  return ()

-- * STRINGS

fwSQuoteCompiler :: ForthType a => Forth w a ()
fwSQuoteCompiler = do
  str <- scanUntil (== '"')
  trace 2 ("COMPILE: S\": \"" ++ str ++ "\"")
  pushc (CCForth (pushStr str))

fwSQuoteInterpet :: ForthType a => Forth w a ()
fwSQuoteInterpet = scanUntil (== '"') >>=  pushStr

fwType :: Forth w a ()
fwType = popString "TYPE" >>= write

-- * Forth words

-- | Store current buffer & input port, place input string on buffer
-- with no input port, 'vmExecuteBuffer', restore buffer & port.
fwEvaluate' :: (Eq a,ForthType a) => String -> Forth w a ()
fwEvaluate' str = do
  vm <- getVm
  let buf = buffer vm
      ip = inputPort vm
  vm' <- liftIO (vmExecuteBuffer (vm {buffer = str, inputPort = Nothing}))
  put (vm' {buffer = buf, inputPort = ip})

-- | Variant on @included@, argument not on stack.
fwIncluded' :: (Eq a,ForthType a) => FilePath -> Forth w a ()
fwIncluded' nm = do
  trace 0 ("INCLUDED: " ++ nm)
  x <- liftIO (doesFileExist nm)
  unless x (throwError ("INCLUDED': FILE MISSING: " ++ tickQuotes nm))
  liftIO (readFile nm) >>= fwEvaluate'

fwIncluded :: (Eq a,ForthType a) => Forth w a ()
fwIncluded = popString "INCLUDED" >>= fwIncluded'

fwI :: Forth w a ()
fwI = popr >>= \x -> pushr x >> push x

-- | Forth word @j@.
fwJ :: Forth w a ()
fwJ = do {x <- popr; y <- popr; z <- popr
          ;pushr z; pushr y; pushr x
          ;push z}

-- | dup : ( p -- p p ) swap : ( p q -- q p ) drop : ( p -- ) over : (
-- p q -- p q p ) rot : ( p q r -- q r p ) 2dup : ( p q -- p q p q )
fwDup,fwSwap,fwDrop,fwOver,fwRot,fw2dup :: Forth w a ()
fwDup = pop' >>= \e -> push' e >> push' e
fwSwap = pop' >>= \p -> pop' >>= \q -> push' p >> push' q
fwDrop = void pop'
fwOver = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q
fwRot = pop' >>= \p -> pop' >>= \q -> pop' >>= \r -> push' q >> push' p >> push' r
fw2dup = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q >> push' p

-- | ( xu ... x1 x0 u -- xu ... x1 x0 xu )
fwPick :: ForthType a => Forth w a ()
fwPick = do
  vm <- getVm
  case stack vm of
    DC n : s' -> let n' = tyToInt' "PICK" n
                     e = s' !! n'
                 in put vm {stack = e : s'}
    _ -> throwError "PICK"

write,writeLn,writeSp :: String -> Forth w a ()
write = liftIO . putStr
writeLn = write . (++ "\n")
writeSp = write . (++ " ")

fwEmit,fwDot :: ForthType a => Forth w a ()
fwEmit = write . return . toEnum =<< popInt "EMIT"
fwDot = writeSp . show =<< pop'

fwDotS :: ForthType a => Forth w a ()
fwDotS = do
  vm <- getVm
  let l = map show (reverse (stack vm))
      n = "<" ++ show (length l) ++ "> "
  write (n ++ concatMap (++ " ") l)

fwBye :: Forth w a ()
fwBye = liftIO exitSuccess

pushStr :: ForthType a => String -> Forth w a ()
pushStr str =
    let f vm = (vm {stack = DC (tyFromInt (length str)) : DCString str : stack vm},())
    in withVm f

fwVmstat :: ForthType a => Forth w a ()
fwVmstat = getVm >>= writeLn . show

fwFork :: ForthType a => Forth w a ()
fwFork = do
  nm <- readToken
  vm <- getVm
  case lookupWord nm vm of
    Just fw -> do th <- liftIO (forkIO (void (execErr vm fw)))
                  let k = hash th :: Int
                  put vm {stack = DC (tyFromInt k) : stack vm
                         ,threads = M.insert k th (threads vm)}
    Nothing -> throwError ("FORK: UNKNOWN WORD: " ++ nm)

fwKill :: ForthType a => Forth w a ()
fwKill = do
  k <- popInt "KILL: PID?"
  vm <- getVm
  let threads' = threads vm
  case M.lookup k threads' of
    Nothing -> throwError ("KILL: UNKNOWN THREAD: " ++ show k)
    Just th -> liftIO (killThread th) >> put vm {threads = M.delete k threads'}

fwKillAll :: Forth w a ()
fwKillAll = do
  vm <- getVm
  let th = M.elems (threads vm)
  liftIO (mapM_ killThread th)
  put vm {threads = M.empty}

fwQuote :: Forth w a ()
fwQuote = do
  tok <- readToken
  push' (DCXT tok)

fwExecute :: Forth w a ()
fwExecute = do
  c <- pop'
  case c of
    DCXT xt -> interpretWord xt
    _ -> throwError "EXECUTE: NOT EXECUTION TOKEN"

-- * Dictionaries

coreDict :: (Eq a,ForthType a) => Dict w a
coreDict =
    let err nm = throwError (tickQuotes nm ++ ": compiler word in interpeter context")
    in M.fromList
    [(":",fwColon)
    ,(";",err ";")
    ,("s\"",fwSQuoteInterpet)
    ,("included",fwIncluded)
    ,("type",fwType)
    ,("do",err "do")
    ,("i",err "i")
    ,("j",err "j")
    ,("loop",err "loop")
    ,("if",err "if")
    ,("else",err "else")
    ,("then",err "then")
    ,("{",err "{")
    ,("}",err "}")
    ,("'",fwQuote)
    ,("execute",fwExecute)
    ,("fork",fwFork)
    ,("kill",fwKill)
    ,("killall",fwKillAll)
    ,("bye",fwBye)
    -- STACK
    ,("drop",fwDrop)
    ,("dup",fwDup)
    ,("over",fwOver)
    ,("pick",fwPick)
    ,("rot",fwRot)
    ,("swap",fwSwap)
    ,("2dup",fw2dup)
    ,(">r",pop' >>= pushr')
    ,("r>",popr' >>= push')
     -- IO
    ,("emit",fwEmit)
    ,(".",fwDot)
    ,(".s",fwDotS)
    ,("key",liftIO getChar >>= \c -> push (tyFromInt (fromEnum c)))
    -- DEBUG
    ,("vmstat",fwVmstat)
    ,("trace",pop >>= \k -> withVm (\vm -> (vm {tracing = tyToInt' "TRACE" k},())))]

coreWords :: [String]
coreWords = M.keys (coreDict :: Dict w Integer)

isReservedWord :: String -> Bool
isReservedWord nm = nm `elem` coreWords

-- * Operation

execErr :: VM w a -> Forth w a () -> IO (VM w a)
execErr vm fw = do
  (r,vm') <- runStateT (CME.runExceptT fw) vm
  case r of
    Left err -> error ("EXECERR: " ++ show err)
    Right () -> return vm'

-- | Read, evaluate, print, loop.  Prints @OK@ at end of line.  Prints
-- error message and runs 'vmReset' on error.
repl' :: (Eq a,ForthType a) => VM w a -> IO ()
repl' vm = do
  (r,vm') <- runStateT (CME.runExceptT vmExecute) vm
  case r of
    Left err -> case err of
                  VMEOF -> putStrLn "BYE" >> liftIO exitSuccess
                  VMNoInput -> liftIO exitSuccess
                  VMError msg -> putStrLn (" ERROR: " ++ msg) >> repl' (vmReset vm)
    Right () -> repl' vm'

catchSigint :: VM w a -> IO ()
catchSigint vm = do
  let h = modifyMVar_ (sigint vm) (return . const True)
  _ <- P.installHandler P.sigINT (P.Catch h) Nothing
  _ <- P.installHandler P.sigTERM (P.Catch h) Nothing
  return ()

-- | 'repl'' but with 'catchSigint'.
repl :: (ForthType a, Eq a) => VM w a -> Forth w a () -> IO ()
repl vm initF = do
  catchSigint vm
  (_,vm') <- runStateT (CME.runExceptT initF) vm
  repl' vm'

loadFiles :: (Eq a,ForthType a) => [String] -> Forth w a ()
loadFiles nm = do
  trace 0 ("LOAD-FILES: " ++ intercalate "," nm)
  r <- liftIO (lookupEnv "HSC3FORTHDIR")
  case r of
    Nothing -> throwError "HSC3FORTHDIR NOT SET"
    Just dir -> mapM_ (fwIncluded' . (dir </>)) nm

-- * List functions

-- | Read until /f/ is 'True', discarding /x/, RHS may be @[]@.
--
-- > breakOn isSpace "" == ([],[])
-- > breakOn (== ')') "comment ) WORD" == ("comment "," WORD")
-- > breakOn (== '\n') " comment\n\n" == (" comment","\n")
breakOn :: (a -> Bool) -> [a] -> ([a],[a])
breakOn f l =
    case break f l of
      (lhs,[]) -> (lhs,[])
      (lhs,_ : rhs) -> (lhs,rhs)

-- | 'snd' of 'breakOn'.
deleteUntil :: (a -> Bool) -> [a] -> [a]
deleteUntil f = snd . breakOn f

bracketed :: (a,a) -> [a] -> [a]
bracketed (l,r) x = l : x ++ [r]

tickQuotes :: String -> String
tickQuotes = bracketed ('\'','\'')
