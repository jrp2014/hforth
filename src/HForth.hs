{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE AllowAmbiguousTypes         #-}
module HForth where

import           Control.Concurrent             ( MVar
                                                , ThreadId
                                                , forkIO
                                                , killThread
                                                , modifyMVar
                                                , modifyMVar_
                                                , threadDelay
                                                )
import           Control.Monad                  ( (>=>)
                                                , unless
                                                , void
                                                , when
                                                )
import qualified Control.Monad.Except          as CME
import           Control.Monad.State            ( MonadIO(liftIO)
                                                , MonadState(get, put)
                                                , StateT(runStateT)
                                                , modify
                                                )
import           Data.Array
import           Data.Char                      ( isSpace
                                                , toLower
                                                )
import           Data.Hashable                  ( Hashable(hash) )
import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           System.Directory               ( doesFileExist )
import           System.Exit                    ( die
                                                , exitSuccess
                                                )
import           System.IO                      ( Handle
                                                , hGetLine
                                                , hIsEOF
                                                )
import qualified System.Posix.Signals          as P

-- * Virtual Machine

-- | A dictionary is a map of named instructions ('Forth's).
type Dict w a m = M.Map String (ForthStep w a m)

-- | Class of values that can constitute a 'Forth'.
class ForthType a where
    tyShow :: a -> String -- ^ String representation of /a/, pretty printer.
    tyToInt :: a -> Maybe Int -- ^ Coercion, ie. for loop counters.
    tyFromInt :: Int -> a -- ^ Coercion
    tyFromBool :: Bool -> a -- ^ Boolean value represented in /a/, by convention @-1@ and @0@.

tyToInt' :: ForthType a => String -> a -> Int
tyToInt' msg = fromMaybe (error ("NOT-INT: " ++ msg)) . tyToInt

instance ForthType Int where
  tyShow    = show
  tyToInt   = Just
  tyFromInt = id
  tyFromBool t = if t then -1 else 0

instance ForthType Integer where
  tyShow    = show
  tyToInt   = Just . fromInteger
  tyFromInt = fromIntegral
  tyFromBool t = if t then -1 else 0

-- | A data cell, for the data stacks.
data DC a = DC a | DCString String | DCXT String

instance ForthType a => Show (DC a) where
  show dc = case dc of
    DC       a   -> tyShow a
    DCString str -> "STRING:" ++ tickQuotes str
    DCXT     str -> "XT:" ++ str

-- | Extract plain value from 'DC', else error.
dcPlain :: DC a -> Forth w a m  a
dcPlain dc = case dc of
  DC a -> return a
  _    -> throwError "DC-NOT-VALUE-CELL"

-- | A compilation cell, for the compilation stack.
data CC w a m = CCWord String | CCForth (ForthStep w a m)

-- | Predicate to see if 'CC' is a particular 'CCWord'.
ccIsWord :: String -> CC w a m -> Bool
ccIsWord w cw = case cw of
  CCWord w' -> w == w'
  _         -> False

-- | Int-indexed memory, type constructor @m@, index type @i@, and elememt type @e@
class Memory m e where
  fetch :: ForthType i => m e -> i -> e
  store :: ForthType i => m e -> i -> e -> m e

instance Memory [] e where
  fetch mem ix = mem !! tyToInt' "FETCH" ix
  store mem ix e = take ix' mem ++ e : drop (ix' + 1) mem
    where ix' = tyToInt' "STORE" ix

instance Memory (Array Int) e where
  fetch mem ix = mem ! tyToInt' "FETCH" ix -- TODO should have a tyToIx
  store mem ix e = mem // [(tyToInt' "STORE" ix, e)]

type VMMemory m e = m e


-- | The machine is either interpreting or compiling.
data VMMode = Interpret | Compile deriving stock (Eq,Show)

-- | The machine, /w/ is the type of the world, /a/ is the type of the stack elements,
-- /m/ is the memory container type.
data VM w a m = VM
  { stack     :: [DC a] -- ^ The data stack, /the/ stack.
  , rstack    :: [DC a] -- ^ The return stack.
  , cstack    :: [CC w a m] -- ^ The compilation stack.
  , threads   :: M.Map Int ThreadId
  , dict      :: Dict w a m -- ^ The dictionary.
  , locals    :: [Dict w a m] -- ^ The stack of locals dictionaries.
  , buffer    :: String -- ^ The current line of input text.
  , mode      :: VMMode -- ^ Basic state of the machine - compiling or interpreting.
  , world     :: w -- ^ The world, instance state.
  , literal   :: String -> Maybe a -- ^ Read function for literal values.
  , recursive :: Maybe (String -> ForthStep w a m) -- ^ Allow recursive word definitions
  , inputPort :: Maybe Handle -- ^ eg, @Just stdin@
  , tracing   :: Int -- ^ for debugging
  , sigint    :: MVar Bool -- ^ True if a SIGINT signal (user interrupt) has been received.
  , vMMemory  :: Maybe (VMMemory m a)
  }

instance (ForthType a) => Show (VM w a m) where
  show vm = concat
    [ "\n DATA STACK: "
    , unwords (map show (reverse $ stack vm))
    , "\n RETURN STACK: "
    , unwords (map show (reverse $ rstack vm))
    , "\n COMPILE STACK DEPTH: "
    , show (length (cstack vm))
    , "\n THREADS: "
    , intercalate "," (map show (M.keys (threads vm)))
    , "\n DICT: "
    , unwords (M.keys (dict vm))
    , "\n LOCALS: "
    , intercalate "," (map (unwords . M.keys) (locals vm))
    , "\n BUFFER: "
    , buffer vm
    , "\n MODE: "
    , show (mode vm)
    , "\n RECURSIVE: "
    , maybe "NO" (const "YES") (recursive vm)
    , "\n INPUT PORT: "
    , maybe "NO" (const "YES") (inputPort vm)
    , "\n TRACING: "
    , show (tracing vm)
    , "\n MEMORY: "
    , maybe "NO" (const "YES") (vMMemory vm)
    ]

-- | Signals (exceptions) from 'VM'.
data VMSignal = VMEOF | VMNoInput | VMError String deriving stock (Eq,Show)

-- | An instruction, the implementation of a /word/.
type Forth w a m r = CME.ExceptT VMSignal (StateT (VM w a m) IO) r

data Continue = Next | Exit deriving stock Show

-- | The result of intpreting an instruction: Left = exit, Right () = continue
type ForthStep w a m  = Forth w a m Continue

next :: ForthStep w a m
next = return Next

exit :: ForthStep w a m
exit = return Exit

-- | Make an empty (initial) machine.
emptyVm :: w -> (String -> Maybe a) -> MVar Bool -> VM w a m
emptyVm w lit sig = VM { stack     = []
                       , rstack    = []
                       , cstack    = []
                       , threads   = M.empty
                       , buffer    = ""
                       , mode      = Interpret
                       , dict      = M.empty
                       , locals    = []
                       , world     = w
                       , literal   = lit
                       , recursive = Nothing
                       , inputPort = Nothing
                       , tracing   = -1
                       , sigint    = sig
                       , vMMemory  = Nothing
                       }

-- | Reset 'VM', on error.
vmReset :: VM w a m  -> VM w a m
vmReset vm = vm { stack  = []
                , rstack = []
                , cstack = []
                , buffer = ""
                , mode   = Interpret
                , locals = []
                , vMMemory = error "Memory reset unimplemented"
                }

-- | Type specialised variant of 'get' that checks SIGINT handler.
getVm :: Forth w a m (VM w a m)
getVm = do
  vm  <- get
  sig <- liftIO (modifyMVar (sigint vm) (\s -> return (False, s)))
  when sig (throwError "VM: SIGINT")
  return vm

-- | Function with 'VM'.
withVm :: (VM w a m -> (VM w a m, r)) -> Forth w a m r
withVm f = getVm >>= \vm -> let (vm', r) = f vm in put vm' >> return r

-- | Procedure with 'VM'.
doWithVm :: (VM w a m -> Forth w a m (VM w a m)) -> Forth w a m ()
doWithVm f = getVm >>= (f >=> put)

-- | Change the world.
vmModifyWorld :: (w -> w) -> Forth w a m ()
vmModifyWorld f = modify (\vm -> vm { world = f (world vm) })

-- * Error

-- | Tracer, levels are 2 = HIGH, 1 = MEDIUM, 0 = LOW
trace :: Int -> String -> Forth w a m ()
trace k msg = do
  vm <- getVm
  when (k <= tracing vm) (void $ writeLn msg)

throwError :: String -> Forth w a m r
throwError = CME.throwError . VMError

-- | Reader that raises an /unknown word/ error.
unknownError :: String -> Forth w a m r
unknownError s = throwError ("UNKNOWN WORD: " ++ tickQuotes s)

-- | Reader that raises an /unimplemented word/ error.
notImplementedError :: String -> Forth w a m r
notImplementedError s = throwError ("UNIMPLEMENTED WORD: " ++ tickQuotes s)

-- * Stack

push' :: DC a -> ForthStep w a m
push' x = do
  modify (\vm -> vm { stack = x : stack vm })
  next

-- | Push value onto 'stack'.
push :: a -> ForthStep w a m
push = push' . DC

pushr' :: DC a -> ForthStep w a m
pushr' x = do
  modify (\vm -> vm { rstack = x : rstack vm })
  next

-- | Push value onto 'rstack'.
pushr :: a -> ForthStep w a m
pushr = pushr' . DC

-- | Push value onto 'cstack'.
pushc :: CC w a m -> ForthStep w a m
pushc x = do
  modify (\vm -> vm { cstack = x : cstack vm })
  next

-- | Pop indicated 'VM' stack.
popVmStack
  :: String -> (VM w a m -> [r]) -> (VM w a m -> [r] -> VM w a m) -> Forth w a m r
popVmStack nm f g = do
  vm <- getVm
  case f vm of
    []     -> throwError (nm ++ ": STACK UNDERFLOW")
    x : xs -> put (g vm xs) >> return x

pop' :: Forth w a m (DC a)
pop' = popVmStack "DATA" stack (\vm s -> vm { stack = s })

-- | Remove value from 'stack'.
pop :: Forth w a m a
pop = pop' >>= dcPlain

popInt :: ForthType a => String -> Forth w a m Int
popInt msg = tyToInt' msg <$> pop

popr' :: Forth w a m (DC a)
popr' = popVmStack "RETURN" rstack (\vm s -> vm { rstack = s })

-- | Remove value from 'rstack'.
popr :: Forth w a m a
popr = popr' >>= dcPlain

-- | Remove value from 'cstack'.
popc :: Forth w a m (CC w a m)
popc = popVmStack "COMPILE" cstack (\vm s -> vm { cstack = s })

-- | ( id len -- )
popString :: String -> Forth w a m String
popString msg = do
  vm <- getVm
  case stack vm of
    DC _ : DCString str : s' -> put vm { stack = s' } >> return str
    _                        -> throwError ("NOT-STRING?" ++ msg)

-- * Token / Expr

-- | Expressions are either literals or words.
data Expr a = Literal a | Word String deriving stock (Show,Eq)

-- | Pretty print 'Expr'.
exprPp :: ForthType a => Expr a -> String
exprPp e = case e of
  Literal a  -> tyShow a
  Word    nm -> nm

-- | Dictionary lookup, word should be lower case.
lookupWord :: String -> VM w a m -> Maybe (ForthStep w a m)
lookupWord k vm = case locals vm of
  []    -> M.lookup k (dict vm)
  l : _ -> case M.lookup k l of
    Nothing -> M.lookup k (dict vm)
    r       -> r

-- | Parse a token string to an expression.
parseToken :: String -> Forth w a m (Expr a)
parseToken s = do
  vm <- getVm
  case lookupWord s vm of
    Just _  -> return (Word s)
    Nothing -> case literal vm s of
      Just l  -> return (Literal l)
      Nothing -> case recursive vm of
        Just _ -> do
          case reverse $ cstack vm of
            (CCWord cw : _) | cw == s -> return (Word s) -- if there is a recursive placeholder, defer...
            _                         -> unknownError s
        Nothing -> unknownError s

-- | Read buffer until predicate holds, if /pre/ delete preceding white space.
readUntil :: Bool -> (Char -> Bool) -> Forth w a m (String, String)
readUntil pre cf = do
  vm <- getVm
  let f = if pre then dropWhile isSpace else id
      r = breakOn cf (f (buffer vm))
  trace 2 (show ("READUNTIL", mode vm, fst r, length (snd r)))
  put vm { buffer = snd r }
  return r

scanUntil :: (Char -> Bool) -> Forth w a m String
scanUntil = fmap fst . readUntil False

-- | Scan a token from 'buffer', ANS Forth type comments are
-- discarded.  Although 'buffer' is filled by 'hGetLine' it may
-- contain newline characters because we may include a file.
scanToken :: Forth w a m (Maybe String)
scanToken = do
  r <- readUntil True isSpace
  case r of
    ("", "") -> do
      vm <- getVm
      let sl = length (stack vm)
      writeLn (" OK " ++ if sl == 0 then "" else '<' : show sl ++ ">")
        >> return Nothing
    (""  , rhs) -> throwError ("SCANTOKEN: NULL: " ++ rhs)
    ("\\", _  ) -> scanUntil (== '\n') >> scanToken
    ("(" , _  ) -> scanUntil (== ')') >> scanToken
    (e   , _  ) -> return (Just e)

-- | Read line from 'inputPort' to 'buffer'.  There are two
-- /exceptions/ thrown here, 'VMEOF' if an input port is given but
-- returns EOF, and 'VMNoInput' if there is no input port.
fwRefill :: ForthStep w a m
fwRefill = do
  vm <- getVm
  case inputPort vm of
    Nothing -> CME.throwError VMNoInput
    Just h  -> do
      eof <- liftIO (hIsEOF h)
      when eof (CME.throwError VMEOF)
      trace 2 "REFILL"
      x <- liftIO (hGetLine h)
      put (vm { buffer = x })
      next

-- | If 'scanToken' is 'Nothing', then 'fwRefill' and retry.  Tokens are lower case.
readToken :: Forth w a m String
readToken = do
  r <- scanToken
  case r of
    Just str -> return (map toLower str)
    Nothing  -> fwRefill >> readToken

-- | 'parseToken' of 'readToken'.
readExpr :: Forth w a m (Expr a)
readExpr = parseToken =<< readToken

-- * Interpret

-- | 'lookupWord' in the dictionary
interpretWord :: String -> ForthStep w a m
interpretWord w = do
  vm <- getVm
  trace 3 ("INTERPRETWORD: " ++ w)
  case lookupWord w vm of
    Just r  -> r
    Nothing -> unknownError w

{-
    case recursive vm of
      Just f ->
        let dR = f w in put vm { dict = M.insert w dR (dict vm) } >> dR
      Nothing -> unknownError w
-}


-- | Either 'interpretWord' or 'push' literal.
interpretExpr :: Expr a -> ForthStep w a m
interpretExpr e = case e of
  Word    w -> interpretWord w
  Literal a -> push a

-- | 'interpretExpr' of 'readExpr'.
vmInterpret :: ForthStep w a m
vmInterpret = readExpr >>= interpretExpr

-- * Compile

-- | Define word and add to dictionary.  The only control structures are /if/ and /do/.
vmCompile :: (Eq a, ForthType a) => ForthStep w a m
vmCompile = do
  expr <- readExpr
  trace 2 ("COMPILE: " ++ exprPp expr)
  case expr of
    Word ";"     -> fwSemiColon
    Word ":"     -> throwError ": IN COMPILE CONTEXT"
    Word "do"    -> pushc (CCWord "do")
    Word "i"     -> pushc (CCForth fwI)
    Word "j"     -> pushc (CCForth fwJ)
    Word "loop"  -> fwLoop
    Word "if"    -> pushc (CCWord "if")
    Word "else"  -> pushc (CCWord "else")
    Word "then"  -> fwThen
    Word "{"     -> fwOpenBrace
    Word "s\""   -> fwSQuoteCompiler
    Word "exit"  -> pushc (CCForth fwExit)
    Word "?exit" -> pushc (CCForth fwQExit)
    e            -> pushc (CCForth (interpretExpr e))

-- | Get instruction at 'CC' or raise an error.
cwInstr :: CC w a m -> ForthStep w a m
cwInstr cw = case cw of
  CCWord  w -> throwError ("cwInstr: WORD: " ++ w)
  CCForth f -> f

-- | Type specialised 'foldl1' of '>>'.
forthBlock :: [ForthStep w a m] -> ForthStep w a m
forthBlock []       = next
forthBlock (i : is) = i >>= \case
  Exit -> next
  Next -> forthBlock is

--forthBlock = foldl1 (>>)

-- | Add a 'locals' frame.
beginLocals :: ForthStep w a m
beginLocals = withVm (\vm -> (vm { locals = M.empty : locals vm }, Next))

-- | Remove a 'locals' frame.
endLocals :: ForthStep w a m
endLocals = withVm (\vm -> (vm { locals = tail (locals vm) }, Next))

-- | Unwind the 'cstack' to the indicated control word.  The result is
-- the code block, in sequence.  The control word is also removed from
-- the cstack.
unwindCstackTo :: String -> Forth w a m [CC w a m]
unwindCstackTo w = do
  withVm
    (\vm ->
      let (r, c) = break (ccIsWord w) (cstack vm)
      in  (vm { cstack = tail c }, reverse r)
    )

-- | Either 'vmInterpret' or 'vmCompile', depending on 'mode'.
vmExecute :: (Eq a, ForthType a) => ForthStep w a m
vmExecute = do
  vm <- getVm
  case mode vm of
    Interpret -> vmInterpret
    Compile   -> vmCompile

vmExecuteBuffer :: (ForthType a, Eq a) => VM w a m -> IO (VM w a m)
vmExecuteBuffer vm = do
  (r, vm') <- runStateT (CME.runExceptT vmExecute) vm
  case r of
    Left err -> case err of
      VMNoInput   -> return vm'
      VMEOF       -> die "VMEXECUTEBUFFER: VMEOF"
      VMError msg -> die
        (  "VMEXECUTEBUFFER: "
        ++ msg
        ++ " before '"
        ++ buffer vm'
        ++ "'"
        )
    Right Next -> vmExecuteBuffer vm'
    Right Exit -> vmExecuteBuffer vm' -- TODO don't stop buffer execution?

-- * DO LOOP

-- | A loop ends when the two elements at the top of the rstack are equal.
loopEnd :: Eq a => Forth w a m Bool
loopEnd = do
  vm <- getVm
  case rstack vm of
    DC p : DC q : _ -> return (p == q)
    _               -> throwError "LOOP-END: ILLEGAL RSTACK"

-- | /code/ is the expressions between @do@ and @loop@.
interpretDoLoop :: (ForthType a, Eq a) => ForthStep w a m -> ForthStep w a m
interpretDoLoop code = do
  start <- pop
  end   <- pop
  _next <- pushr end
  _next <- pushr start
  let step = do
        shortCircuit <- code
        case shortCircuit of
          Exit -> exit
          Next -> do
            i <- popr
            let i' = tyFromInt (tyToInt' "DO-LOOP: I" i + 1)
            pushr i'
  let loop = do
        r <- loopEnd
        if not r then step >> loop else popr >> popr >> next
  loop

-- | Compile @loop@ statement, end of do block.
fwLoop :: (Eq a, ForthType a) => ForthStep w a m
fwLoop = do
  cw <- unwindCstackTo "do"
  let w = forthBlock (map cwInstr cw)
  pushc (CCForth (interpretDoLoop w))

-- | placeholder for deferred definitions
fwUndefined :: Maybe (String -> ForthStep w a m)
fwUndefined = Just u
  where u s = forthBlock [writeLn $ s ++ " is undfined", fwExit]

-- | @exit@ statement
fwExit :: ForthStep w a m
fwExit = exit -- short circuit

-- | @?exit@ statement
fwQExit :: (Eq a, ForthType a) => ForthStep w a m
fwQExit = interpretIf (fwExit, next)

-- * IF ELSE THEN

-- | Consult stack and select either /true/ or /false/.
interpretIf
  :: (Eq a, ForthType a) => (ForthStep w a m, ForthStep w a m) -> ForthStep w a m
interpretIf (t, f) = pop >>= \x -> do
  trace 3 ("INTERPRETIF: " ++ tyShow x)
  if x /= tyFromBool False then t else f

-- | Compile @then@ statement, end of @if@ block.
fwThen :: (Eq a, ForthType a) => ForthStep w a m
fwThen = do
  cw <- unwindCstackTo "if"
  let f = forthBlock . map cwInstr
  case break (ccIsWord "else") cw of
    (tb, []) -> pushc (CCForth (interpretIf (f tb, next)))
    (tb, fb) -> pushc (CCForth (interpretIf (f tb, f (tail fb))))

-- * LOCALS

-- | Variant on @(local)@, argument not on stack.
fwLocal' :: String -> ForthStep w a m
fwLocal' nm = do
  vm <- getVm
  case stack vm of
    e : s' -> do
      put vm
        { stack  = s'
        , locals = case locals vm of
                     []     -> error "NO LOCALS FRAME"
                     l : l' -> M.insert nm (push' e) l : l'
        }
      next
    _ -> throwError ("(LOCAL): STACK UNDERFLOW: " ++ nm)

-- | Function over current locals 'Dict'.
atCurrentLocals :: (Dict w a m -> Dict w a m) -> VM w a m -> VM w a m
atCurrentLocals f vm = case locals vm of
  l : l' -> vm { locals = f l : l' }
  _      -> error "ATCURRENTLOCALS"

-- | 'locals' is used both during compilation and interpretation.  In
-- compilation the RHS is undefined, it is used for name lookup and to
-- know if an interpreter 'locals' frame must be made.  In
-- interpretation, if required, it is a secondary dictionary,
-- consulted first.
--fwOpenBrace :: ForthType a => ForthStep w a m
fwOpenBrace :: ForthStep w a m
fwOpenBrace = do
  let getNames r = do
        w <- readToken
        if w == "}" then return r else getNames (w : r)
  nm <- getNames []
  when (any isReservedWord nm)
       (throwError ("FWOPENBRACE: RESERVED WORD: " ++ unwords nm))
  trace 0 ("DEFINE-LOCALS: " ++ unwords nm)
  let locals' = M.fromList (zip nm (repeat undefined))
  withVm (\vm -> (atCurrentLocals (M.union locals') vm, ()))
  pushc (CCForth (forthBlock (map fwLocal' nm)))

-- * Compiler

-- | ":". Enter compile phase, the word name is pushed onto the
-- /empty/ 'cstack', and a 'locals' frame is added.
fwColon :: ForthStep w a m
fwColon = do
  nm <- readToken
  trace 0 ("DEFINE: " ++ nm)
  let
    edit vm = do
      when (isReservedWord nm) (throwError ("':' RESERVED NAME: " ++ nm))
      unless (null (cstack vm)) (throwError ("':' CSTACK NOT EMPTY: " ++ nm))
      return
        (vm { mode   = Compile
            , cstack = [CCWord nm]
            , locals = M.empty : locals vm
            }
        )
  doWithVm edit
  next

-- | ";".  End compile phase.  There is always a compile 'locals'
-- frame to be removed.
fwSemiColon :: ForthStep w a m
fwSemiColon = do
  vm <- getVm
  case reverse (cstack vm) of
    CCWord nm : cw ->
      let instr  = map cwInstr cw
          instr' = if M.null (head (locals vm))
            then instr
            else bracketed (beginLocals, endLocals) instr
          w = forthBlock instr'
      in  do
            trace 2 ("END DEFINITION: " ++ nm)
            when (M.member nm (dict vm)) (void $ writeLn ("REDEFINED " ++ nm))
            put
              (vm { cstack = []
                  , locals = tail (locals vm)
                  , dict   = M.insert nm w (dict vm)
                  , mode   = Interpret
                  }
              )
            next
    _ -> throwError "CSTACK UNDERFLOW"

-- * STRINGS

fwSQuoteCompiler :: ForthType a => ForthStep w a m
fwSQuoteCompiler = do
  str <- scanUntil (== '"')
  trace 2 ("COMPILE: S\": \"" ++ str ++ "\"")
  pushc (CCForth (pushStr str))

fwSQuoteInterpet :: ForthType a => ForthStep w a m
fwSQuoteInterpet = scanUntil (== '"') >>= pushStr

fwType :: ForthStep w a m
fwType = (popString "TYPE" >>= write) >> next

-- * Forth words

-- | Store current buffer & input port, place input string on buffer
-- with no input port, 'vmExecuteBuffer', restore buffer & port.
fwEvaluate' :: (Eq a, ForthType a) => String -> ForthStep w a m
fwEvaluate' str = do
  vm <- getVm
  let buf = buffer vm
      ip  = inputPort vm
  vm' <- liftIO (vmExecuteBuffer (vm { buffer = str, inputPort = Nothing }))
  put (vm' { buffer = buf, inputPort = ip })
  next

-- | Variant on @included@, argument not on stack.
fwIncluded' :: (Eq a, ForthType a) => FilePath -> ForthStep w a m
fwIncluded' nm = do
  trace 0 ("INCLUDED: " ++ nm)
  x <- liftIO (doesFileExist nm)
  unless x (throwError ("INCLUDED': FILE MISSING: " ++ tickQuotes nm))
  liftIO (readFile nm) >>= fwEvaluate'

fwIncluded :: (Eq a, ForthType a) => ForthStep w a m
fwIncluded = popString "INCLUDED" >>= fwIncluded'

fwI :: ForthStep w a m
fwI = popr >>= \x -> pushr x >> push x

-- | Forth word @j@.
fwJ :: ForthStep w a m
fwJ = do
  x     <- popr
  y     <- popr
  z     <- popr
  _next <- pushr z
  _next <- pushr y
  _next <- pushr x
  push z

-- Apply comparison with top of stack
-- (optimized version of comparisonOp)  Not used
comparison :: ForthType a => (a -> Bool) -> ForthStep w a m
comparison cmp = do
  vm <- getVm
  case stack vm of
    DC n : s' -> do
      let flag = tyFromBool $ cmp n in put vm { stack = DC flag : s' }
      next
    _ -> throwError "comparison"

-- | Apply comparison with top of stack
binOp :: (a -> a -> a) -> ForthStep w a m
binOp op = do
  vm <- getVm
  case stack vm of
    DC x : DC y : s' -> do
      put vm { stack = DC (y `op` x) : s' }
      next
    _ -> throwError "binop"

-- | Binary stack operation.  The first value on the stack is the RHS.
binaryOp :: (a -> a -> a) -> ForthStep w a m
binaryOp f = pop >>= \y -> pop >>= \x -> push (f x y)

-- | Unary stack operation.
unaryOp :: (a -> a) -> ForthStep w a m
unaryOp f = pop >>= push . f

-- | comparison operation
comparisonOp :: ForthType a => (a -> a -> Bool) -> ForthStep w a m
comparisonOp f = binaryOp (\x y -> tyFromBool (f x y))

predicateOp :: ForthType a => (a -> Bool) -> ForthStep w a m
predicateOp f = unaryOp (tyFromBool . f)

-- | dup : ( p -- p p ) swap : ( p q -- q p ) drop : ( p -- ) over : (
-- p q -- p q p ) rot : ( p q r -- q r p ) 2dup : ( p q -- p q p q )
fwDup, fwSwap, fwDrop, fwOver, fwRot, fw2dup :: ForthStep w a m
fwDup = pop' >>= \e -> push' e >> push' e
fwSwap = pop' >>= \p -> pop' >>= \q -> push' p >> push' q
fwDrop = do
  trace 3 "FWDROP"
  _dropped <- pop'
  next
fwOver = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q
fwRot =
  pop' >>= \p -> pop' >>= \q -> pop' >>= \r -> push' q >> push' p >> push' r
fw2dup = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q >> push' p

-- | ( xu ... x1 x0 u -- xu ... x1 x0 xu )
fwPick :: ForthType a => ForthStep w a m
fwPick = do
  vm <- getVm
  case stack vm of
    DC n : s' ->
      let n' = tyToInt' "PICK" n
          e  = s' !! n'
      in  do
            put vm { stack = e : s' }
            next
    _ -> throwError "PICK"

-- | fwGTR (>r)
fwGTR :: ForthStep w a m
fwGTR = pop' >>= pushr'

-- | fwRGT (r>)
fwRGT :: ForthStep w a m
fwRGT = popr' >>= push'

-- | 0<
fw0LT :: (Ord a, ForthType a) => ForthStep w a m
fw0LT = predicateOp (< tyFromInt 0)

-- | -
fwMinus :: (Num a) => ForthStep w a m
fwMinus = binaryOp (-)

write, writeLn, writeSp :: String -> ForthStep w a m
write s = do
  (liftIO . putStr) s
  next
writeLn s = do
  (write . (++ "\n")) s
writeSp s = do
  (write . (++ " ")) s

fwEmit, fwDot :: ForthType a => ForthStep w a m
fwEmit = write . return . toEnum =<< popInt "EMIT"
fwDot = writeSp . show =<< pop'

fwDotS :: ForthType a => ForthStep w a m
fwDotS = do
  vm <- getVm
  let l = map show (reverse (stack vm))
      n = "<" ++ show (length l) ++ "> "
  write (n ++ concatMap (++ " ") l)

fwBye :: ForthStep w a m
fwBye = liftIO exitSuccess

pushStr :: ForthType a => String -> ForthStep w a m
pushStr str =
  let f vm =
        ( vm { stack = DC (tyFromInt (length str)) : DCString str : stack vm }
        , Next
        )
  in  withVm f

fwVmStat :: ForthType a => ForthStep w a m
fwVmStat = getVm >>= writeLn . show

fwFork :: ForthType a => ForthStep w a m
fwFork = do
  nm <- readToken
  vm <- getVm
  case lookupWord nm vm of
    Just fw -> do
      th <- liftIO (forkIO (void (execErr vm fw)))
      let k = hash th :: Int
      put vm { stack   = DC (tyFromInt k) : stack vm
             , threads = M.insert k th (threads vm)
             }
      next
    Nothing -> throwError ("FORK: UNKNOWN WORD: " ++ nm)

fwKill :: ForthType a => ForthStep w a m
fwKill = do
  k  <- popInt "KILL: PID?"
  vm <- getVm
  let threads' = threads vm
  case M.lookup k threads' of
    Nothing -> throwError ("KILL: UNKNOWN THREAD: " ++ show k)
    Just th -> do
      liftIO (killThread th) >> put vm { threads = M.delete k threads' }
      next

fwKillAll :: ForthStep w a m
fwKillAll = do
  vm <- getVm
  let th = M.elems (threads vm)
  liftIO (mapM_ killThread th)
  put vm { threads = M.empty }
  next

fwQuote :: ForthStep w a m
fwQuote = do
  tok <- readToken
  push' (DCXT tok)

fwExecute :: ForthStep w a m
fwExecute = do
  c <- pop'
  case c of
    DCXT xt -> interpretWord xt >> next
    _       -> throwError "EXECUTE: NOT EXECUTION TOKEN"

-- | Pause the current thread (seconds)
fwPause :: (ForthType a) => ForthStep w a m
fwPause = popInt "PAUSE" >>= pauseThread >> next
  where pauseThread n = when (n > 0) (liftIO (threadDelay (n * 1000000)))

fwFetch :: (ForthType a, Memory m a)  => ForthStep w a m
fwFetch = do
  vm <- getVm
  addr <- popInt "FWFETCH"
  let mem = fromMaybe (error "NO MEMORY FROM WHICH TO FETCH")
  push (fetch (mem (vMMemory vm)) addr)

fwStore :: (ForthType a, Memory m a) => ForthStep w a m
fwStore = do
  addr <- popInt "FWSTOR ADDRESS"
  value <- pop
  let mem = fromMaybe (error "NO MEMORY TO WHICH TO STORE")
  let f vm =
        ( vm {vMMemory = Just $ store (mem (vMMemory vm)) addr value},
          Next
        )
   in withVm f


-- * Dictionaries

coreDict :: (Ord a, Num a, ForthType a, Memory m a) => Dict w a m
coreDict =
  let err nm =
        throwError (tickQuotes nm ++ ": compiler word in interpeter context")
  in
    M.fromList
      [ (":"       , fwColon)
      , (";"       , err ";")
      , ("s\""     , fwSQuoteInterpet)
      , ("included", fwIncluded)
      , ("type"    , fwType)
      , ("do"      , err "do")
      , ("i"       , err "i")
      , ("j"       , err "j")
      , ("loop"    , err "loop")
      , ("if"      , err "if")
      , ("else"    , err "else")
      , ("then"    , err "then")
      , ("{"       , err "{")
      , ("}"       , err "}")
      , ("'"       , fwQuote)
      , ("execute" , fwExecute)
      , ("fork"    , fwFork)
      , ("kill"    , fwKill)
      , ("killall" , fwKillAll)
      , ( "recursive"
        , pop >>= \r -> withVm
          (\vm ->
            ( vm
              { recursive = if r /= tyFromBool False
                              then fwUndefined
                              else Nothing
              }
            , Next
            )
          )
        )
      , ("bye"   , fwBye)
      , ("exit"  , err "exit")
      , ("?exit" , err "?exit")
      , ("0<"    , fw0LT)
      , ("-"     , fwMinus)
      , ("pause" , fwPause)
  -- STACK
      , ("drop"  , fwDrop)
      , ("dup"   , fwDup)
      , ("over"  , fwOver)
      , ("pick"  , fwPick)
      , ("rot"   , fwRot)
      , ("swap"  , fwSwap)
      , ("2dup"  , fw2dup)
      , (">r"    , fwGTR)
      , ("r>"    , fwRGT)
   -- IO
      , ("emit"  , fwEmit)
      , ("."     , fwDot)
      , (".s"    , fwDotS)
      , ("key", liftIO getChar >>= \c -> push (tyFromInt (fromEnum c)))
  -- MEMORY
      , ("!"     , fwStore)
      , ("@"     , fwFetch)
  -- DEBUG
      , ("vmstat", fwVmStat)
      , ( "trace"
        , pop >>= \k ->
          withVm (\vm -> (vm { tracing = tyToInt' "TRACE" k }, Next))
        )
      ]

coreWords :: [String]
coreWords = M.keys (coreDict ::  Dict w Integer [])

isReservedWord :: String -> Bool
isReservedWord nm = nm `elem` coreWords

-- * Operation

execErr :: VM w a m -> ForthStep w a m -> IO (VM w a m)
execErr vm fw = do
  (r, vm') <- runStateT (CME.runExceptT fw) vm
  case r of
    Left  err  -> error ("EXECERR: " ++ show err)
    Right Next -> return vm'
    Right Exit -> return vm'

-- | Read, evaluate, print, loop.  Prints @OK@ at end of line.  Prints
-- error message and runs 'vmReset' on error.
repl' :: (Eq a, ForthType a) => VM w a m -> IO ()
repl' vm = do
  (r, vm') <- runStateT (CME.runExceptT vmExecute) vm
  case r of
    Left err -> case err of
      VMEOF       -> putStrLn "BYE" >> liftIO exitSuccess
      VMNoInput   -> liftIO exitSuccess
      VMError msg -> putStrLn (" ERROR: " ++ msg) >> repl' (vmReset vm)
    Right Next -> repl' vm'
    Right Exit -> repl' vm'

catchSigint :: VM w a m -> IO ()
catchSigint vm = do
  let h = modifyMVar_ (sigint vm) (return . const True)
  _ <- P.installHandler P.sigINT (P.Catch h) Nothing
  _ <- P.installHandler P.sigTERM (P.Catch h) Nothing
  return ()

-- | 'repl'' but with 'catchSigint'.
repl :: (ForthType a, Eq a) => VM w a m -> ForthStep w a m -> IO ()
repl vm initF = do
  catchSigint vm
  (r, vm') <- runStateT (CME.runExceptT initF) vm
  case r of
    Left err -> case err of
      VMEOF       -> putStrLn "BYE" >> liftIO exitSuccess
      VMNoInput   -> liftIO exitSuccess
      VMError msg -> putStrLn (" ERROR: " ++ msg) >> repl' (vmReset vm)
    Right Next -> repl' vm'
    Right Exit -> repl' vm'

loadFiles :: (Eq a, ForthType a) => [String] -> ForthStep w a m
loadFiles nm = do
  trace 0 ("LOAD-FILES: " ++ intercalate "," nm)
  mapM_ fwIncluded' nm
  next

-- * List functions

-- | Read until /f/ is 'True', discarding /x/, RHS may be @[]@.
--
-- > breakOn isSpace "" == ([],[])
-- > breakOn (== ')') "comment ) WORD" == ("comment "," WORD")
-- > breakOn (== '\n') " comment\n\n" == (" comment","\n")
breakOn :: (a -> Bool) -> [a] -> ([a], [a])
breakOn f l = case break f l of
  (lhs, []     ) -> (lhs, [])
  (lhs, _ : rhs) -> (lhs, rhs)

-- | 'snd' of 'breakOn'.
deleteUntil :: (a -> Bool) -> [a] -> [a]
deleteUntil f = snd . breakOn f

bracketed :: (a, a) -> [a] -> [a]
bracketed (l, r) x = l : x ++ [r]

tickQuotes :: String -> String
tickQuotes = bracketed ('\'', '\'')
