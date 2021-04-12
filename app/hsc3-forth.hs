import Control.Concurrent {- base -}
import Control.Monad {- base -}
import Control.Monad.Except {- mtl -}
import Control.Monad.State {- mtl -}
import Data.Char {- hash -}
import Data.Hashable {- hash -}
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import System.IO {- base -}
import System.Process {- process -}
import Text.Printf {- base -}

import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}
import Sound.SC3.Common.Base {- hsc3 -}
import Sound.SC3.UGen.Plain {- hsc3 -}
import Sound.SC3.UGen.PP {- hsc3 -}

import Sound.SC3.UGen.Protect {- hsc3-rw -}

import qualified Sound.SC3.Lang.Help as Help {- hsc3-lang -}

import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.PP as DB {- hsc3-db -}

import Sound.SC3.UGen.Dot {- hsc3-dot -}

import HForth

-- * Forth

-- | hsc3-forth word.
type U_Forth r = Forth Int UGen r

-- | 'replicateM' of 'pop'.
pop_n :: Int -> Forth w a [a]
pop_n n = replicateM n pop

-- | 'mapM_' of 'push'.
push_l :: [a] -> Forth w a ()
push_l = mapM_ push

pop_double :: String -> Forth w UGen Double
pop_double msg =
    let f u = case constant_opt u of
                Nothing -> throw_error ("POP_DOUBLE: " ++ msg ++ ": " ++ show u)
                Just n -> return n
    in pop >>= f

-- | Get UId counter.
get_uid :: Forth Int a Int
get_uid = with_vm (\vm -> (vm,world vm))

-- | Store UId counter.
set_uid :: Int -> Forth Int a ()
set_uid k = with_vm (\vm -> (vm {world = k},()))

-- | Get UId counter and store increment.
incr_uid :: Forth Int a Int
incr_uid = with_vm (\vm -> let k = world vm in (vm {world = k + 1},k))

-- | Assert that the stack is empty.
fw_assert_empty :: Forth_Type a => Forth w a ()
fw_assert_empty = do
  vm <- get
  case stack vm of
    [] -> return ()
    l -> throw_error ("STACK NOT EMPTY: " ++ unwords (map show l))

ugen_sep :: String -> Forth w a (String,Maybe Rate)
ugen_sep = maybe (throw_error "UGEN NAME RATE SEPARATOR FAILED") return . sc3_ugen_name_sep

-- * UForth

-- DEMAND makes this two stage, since inputs are known until we know if to pop NC.
get_nc :: DB.U -> Maybe Int -> U_Forth (Maybe Int)
get_nc u nc =
    case nc of
      Just n -> return (Just n)
      Nothing -> case DB.ugen_nc_mce u of
                   Just _ -> return Nothing
                   Nothing -> fmap Just (pop_int "GET_NC")

-- > fmap ugen_io (DB.u_lookup CI "DSEQ")
-- > fmap ugen_io (DB.u_lookup CI "DEMAND")
ugen_io :: DB.U -> (Int,Maybe Int)
ugen_io u = (length (DB.ugen_inputs u),DB.u_fixed_outputs u)

gen_plain :: String -> U_Forth ()
gen_plain w = do
  (nm,rt) <- ugen_sep w
  let (nm',sp) = case rt of
                   Nothing -> resolve_operator CI nm
                   _ -> (nm,Nothing)
      sp' = Special (fromMaybe 0 sp)
  u <- case DB.u_lookup CI nm' of
         Nothing -> throw_error ("DYNAMIC FAILED: UNKNOWN UGEN: " ++ tick_quotes nm')
         Just r -> return r
  when (isNothing rt && isNothing (DB.ugen_filter u))
       (throw_error ("OSC: NO RATE?: " ++ tick_quotes nm'))
  let (inp,nc) = ugen_io u
  z <- if DB.ugen_nondet u then fmap UId incr_uid else return NoId
  nc' <- get_nc u nc
  i <- fmap reverse (pop_n inp)
  let nc'' = fromMaybe (length (mceChannels (last i))) nc'
  let rt' = fromMaybe (maximum (map rateOf i)) rt
      i' = (if DB.ugen_std_mce u  > 0 then halt_mce_transform else id) i
  push (ugen_optimise_const_operator (mk_plain rt' (DB.ugen_name u) i' nc'' sp' z))

gen_nm :: UGen -> String
gen_nm = show . hash . show

sched :: Time -> UGen -> IO ()
sched t u =
    let nm = gen_nm u
        sy = synthdef nm (out 0 u)
        b0 = bundle immediately [d_recv sy]
        b1 = bundle t [s_new nm (-1) AddToHead 1 []]
    in withSC3 (sendBundle b0 >> sendBundle b1)

fw_help :: Forth w a ()
fw_help = do
  (nm,_) <- ugen_sep =<< pop_string "HELP: NAME"
  case DB.ugen_summary_maybe CI nm of
    Nothing -> throw_error ("?: NO HELP: " ++ nm)
    Just (_,h) -> liftIO (putStrLn h)

fw_manual :: Forth w a ()
fw_manual = do
  (nm,_) <- ugen_sep =<< pop_string "MANUAL: NAME"
  case DB.u_lookup CI nm of
    Nothing -> throw_error ("MANUAL: NO ENTRY: " ++ nm)
    Just u -> liftIO (Help.viewSC3Help (DB.ugen_name u))

dpans_id_to_url :: String -> String
dpans_id_to_url dpans_id =
  let dpans_sec = read (takeWhile isDigit dpans_id) :: Int
  in printf "http://forth.sourceforge.net/std/dpans/dpans%d.htm#%s" dpans_sec dpans_id

fw_dpans :: Forth w a ()
fw_dpans = do
  dpans_id <- pop_string "DPANS: ID"
  liftIO (void (rawSystem "x-www-browser" [dpans_id_to_url dpans_id]))

fw_play_at :: U_Forth ()
fw_play_at = do
  grp <- pop_int "PLAY-AT: GRP"
  act <- pop_int "PLAY-AT: ACT"
  nid <- pop_int "PLAY-AT: NID"
  u <- pop
  fw_assert_empty
  liftIO (audition_at (nid,toEnum act,grp,[]) u)

fw_write_synthdef :: U_Forth ()
fw_write_synthdef = do
  fn <- pop_string "WRITE-SYNTHDEF: FILE-NAME"
  nm <- pop_string "WRITE-SYNTHDEF: SYNTHDEF-NAME"
  u <- pop
  fw_assert_empty
  liftIO (synthdefWrite fn (synthdef nm (out (control KR "out" 0) u)))

fw_pretty_print :: U_Forth ()
fw_pretty_print = do
  k <- pop_int "PRETTY-PRINT"
  u <- pop
  liftIO (putStrLn (DB.ugen_graph_forth_pp (toEnum k) u))

fw_load_datum :: Char -> U_Forth Datum
fw_load_datum c =
    case c of
      'i' -> pop_int "LOAD-DATUM: I" >>= return . int32
      'f' -> pop_double "LOAD-DATUM: F" >>= return . float
      's' -> pop_string "LOAD-DATUM: S" >>= return . string
      _ -> throw_error ("LOAD-DATUM: UNKNOWN TYPE: " ++ tick_quotes [c])

fw_async :: U_Forth ()
fw_async = do
  nm <- pop_string "ASYNC: MESSAGE-NAME"
  ty <- pop_string "ASYNC: OSC-TYPE-SIG"
  param <- mapM fw_load_datum (reverse (tail ty))
  _ <- liftIO (withSC3 (async (message nm (reverse param))))
  return ()

ugen_dict :: Dict Int UGen
ugen_dict =
    M.fromList $ map (\(nm,en) -> (map toLower nm,en))
    [("clone",pop_int "CLONE" >>= \n -> pop >>= \u -> incr_uid >>= \z -> push (uclone_all z n u))
    ,("draw",pop >>= \u -> fw_assert_empty >> liftIO (draw (out 0 u)))
    ,("mce",pop_int "MCE" >>= \n -> pop_n n >>= push . mce . reverse)
    ,("mix",pop >>= push . mix) -- here rather hsc3.fs to get sum_opt for graph comparisons...
    ,("mrg",pop_int "MRG" >>= \n -> pop_n n >>= push . mrg . reverse)
    ,("play-at",fw_play_at)
    ,("write-synthdef",fw_write_synthdef)
    ,("sched",pop_double "SCHED" >>= \t -> pop >>= \u -> fw_assert_empty >> liftIO (sched t u))
    ,("stop",liftIO (withSC3 reset))
    ,("unmce",pop >>= push_l . mceChannels)
    ,("async",fw_async)
    ,("pause",pop_double "PAUSE" >>= pauseThread)
    ,("time",liftIO time >>= push . constant)
    ,("label",pop_string "LABEL" >>= push . label)
    ,("get-uid",get_uid >>= push . constant)
    ,("set-uid",pop_int "SET-UID" >>= set_uid)
    ,("unrand",pop >>= push . ugen_optimise_ir_rand)
    ,("chan",pop >>= push . constant . length . mceChannels)
    ,("sc3-status",liftIO (withSC3 serverStatus >>= mapM_ putStrLn))
    ,("pretty-print",fw_pretty_print)
    ,("?",fw_help)
    ,("manual",fw_manual)
    ,("dpans",fw_dpans)]

instance Forth_Type UGen where
    ty_show = ugen_concise_pp
    ty_to_int = fmap floor . u_constant
    ty_from_int = fromIntegral
    ty_from_bool t = if t then -1 else 0

main :: IO ()
main = do
  sig <- newMVar False
  let d :: Dict Int UGen
      d = M.unions [core_dict,ugen_dict]
      vm = (empty_vm 0 parse_constant sig) {dynamic = Just gen_plain
                                           ,dict = d
                                           ,input_port = Just stdin}
      init_f = load_files ["stdlib.fs","hsc3.fs","overlap-texture.fs","sapf.fs"]
  putStrLn "HSC3-FORTH"
  repl vm init_f
