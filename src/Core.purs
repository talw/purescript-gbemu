module Core where

import Prelude
import Data.Int.Bits
import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Foldable
import Control.Monad.Eff
import Control.Monad
import Control.Bind

import Types
import MainMem
import Ops
import OpCodeMap
import Gpu
import Utils
import Control.Monad.Rec.Class

import Debug

reset :: forall e. Array I8 -> Eff (ma :: MemAccess, canvas :: Canvas | e) Z80State
reset rom = do
  resetScreen
  cs <- cleanState
  return
    cs {
      mem = cs.mem {
        mainMem = setRom rom cs.mem.mainMem
      }
    }


run :: forall e. Int -> Z80State -> Eff (ma :: MemAccess, canvas :: Canvas | e) Z80State
run interval state = tailRecM go { intr : interval, st : state }
 where
  go { st = st@{stop = true} } = return $ Right st
  go { intr, st } | intr <= 0 = return $ Right st
  go { intr, st } = do
    {--if (shldTrc st) then traceA "-------step-------" else return unit--}
    st' <- step st
    {--st' <- stepTime st--}
    let elapsed = st'.totalM - st.totalM
    return $ Left { intr : (intr - elapsed), st : st' }

step :: forall e. Z80State -> Eff (ma :: MemAccess, canvas :: Canvas | e) Z80State
step state@{ mem = oldMem@{regs = oldRegs} } = do
  opt <- opTiming
  handleGpu opt
    <=< (return <<< updImeCntInMem)
    <=< incTime
    <=< (return <<< incPc oldRegs.pc)
    <=< op
    {--<<< traceState--}
     $  state
 where
  op st = if checkShldHndlIntrr st
    then interruptOp st
    else regularOp st
  regularOp st = do
    coc <- getCurrOpCode st
    getCpuOp coc basicOps st
  getCurrOpCode st = rd8 st.mem.regs.pc st.mem.mainMem
  incTime st@{totalM} = do
    opt <- opTiming
    return st { totalM = st.totalM + opt }
  opTiming = getCurrOpTiming oldRegs.pc state
  updImeCntInMem st = st { mem = st.mem { mainMem = updImeCnt st.mem.mainMem } }

getCurrOpTiming :: forall e. I16 -> Z80State -> Eff (ma :: MemAccess | e) Int
getCurrOpTiming oldPc st = do
  opCode <- rd8 oldPc st.mem.mainMem
  let isExtOp = opCode == 0xCB
  addr <- if isExtOp
    then rd8 (oldPc+1) st.mem.mainMem
    else return opCode
  let opTimingTable = if isExtOp
        then cbOpTimings 
        else
          if st.mem.regs.brTkn
            then branchBasicOpTimings
            else basicOpTimings
  return $ getOpTiming addr opTimingTable

--NOTE is there a better way to set nested properties?
--consider checking the lens-equivalent in purescript.
incPc :: I8 -> Z80State -> Z80State
incPc oldPc st =
  if oldPc /= st.mem.regs.pc
    then st
    else
      st {
        mem = st.mem {
          regs = st.mem.regs {
            pc = 65535 .&. (st.mem.regs.pc + 1)
          }
        }
      }


--NOTE change handleGpu to get the op timing itself, instead of reacquiring it here.
handleGpu :: forall e. Int -> Z80State
          -> Eff (ma :: MemAccess, canvas :: Canvas | e) Z80State
handleGpu opTiming st = do
  gpu' <- gpuStep opTiming (getGpu st.mem.mainMem)
  let intF' = (if gpu'.vblIntrr then --(0x01 .|. _)
                                ({-trace ("vbl-totalM: " ++ show st.totalM) \_ ->-} (0x01 .|. _))
                                else id)
                $ getIntF st.mem.mainMem

  setGpu gpu' st.mem.mainMem
  setVblIntrr false st.mem.mainMem 
  setIntF intF' st.mem.mainMem
  return st

interruptOp :: forall e. Z80State -> Eff (ma :: MemAccess | e) Z80State
interruptOp st = do
  setIntF (fst res $ getIntF st.mem.mainMem) st.mem.mainMem 
  setIme false st.mem.mainMem
  snd res st
--    st { mem = st.mem {
 --     mainMem = setIme2 false
  --           =<< setIntF (fst res $ getIntF st.mem.mainMem) $ st.mem.mainMem } }
 where
  res :: Tuple (Int -> Int) (Z80State -> Eff (ma :: MemAccess | e) Z80State)
  res = fromMaybe (Tuple id return) mRes
  mRes = getOnBit (interruptBits st) >>= \x -> case x of
           1  -> Just $ Tuple (0xFE .&. _) (mme2op $ callRoutine true 0x40 )
           2  -> Just $ Tuple (0xFD .&. _) (mme2op $ callRoutine true 0x48 )
           4  -> Just $ Tuple (0xFB .&. _) (mme2op $ callRoutine true 0x50 )
           8  -> Just $ Tuple (0xF7 .&. _) (mme2op $ callRoutine true 0x58 )
           16 -> Just $ Tuple (0xEF .&. _) (mme2op $ callRoutine true 0x60 )
           otherwise -> Nothing -- NOTE log this

interruptBits :: Z80State -> I8
interruptBits st = getIntE st.mem.mainMem .&. getIntF st.mem.mainMem

checkShldHndlIntrr :: Z80State -> Boolean
checkShldHndlIntrr st = getIme st.mem.mainMem && interruptBits st /= 0 


cleanState :: forall e. Eff (ma :: MemAccess | e) Z80State
cleanState = 
  { mem : _
  , totalM : 1
  , halt : false
  , stop : false
  } <$> cleanMem

cleanMem :: forall e. Eff (ma :: MemAccess | e) Mem
cleanMem =
  { regs : cleanRegs
  , svdRegs : cleanSavedRegs
  , mainMem : _
  } <$> cleanMainMem

cleanRegs :: Regs
cleanRegs =
  { pc    : 0x0101
  , sp    : 0xFFFE
  , a     : 0x01
  , b     : 0
  , c     : 0x13
  , d     : 0
  , e     : 0xD8
  , h     : 0x01
  , l     : 0x4D
  , f     : 0xB0
  , brTkn : false
  }

cleanSavedRegs :: SavedRegs
cleanSavedRegs =
  { a  : 0
  , b  : 0
  , c  : 0
  , d  : 0
  , e  : 0
  , h  : 0
  , l  : 0
  , f  : 0
  }

--Debug functions

{--stepTime :: forall e. Z80State--}
         {---> Eff (ma :: MemAccess, canvas :: Canvas, timer :: Timer | e) Z80State--}
{--stepTime state = do--}
  {--currOpCode <- rd8 state.mem.regs.pc state.mem.mainMem--}
  {--currImm <- rd8 (state.mem.regs.pc+1) state.mem.mainMem--}
  {--let timerIx = if (currOpCode `elem` memModifyOps)--}
                {--|| (currOpCode == 0xCB && (currImm `elem` cbMemModifyOps))--}
                  {--then 1 else 0--}
  {--timeIt timerIx (\_ -> step state)--}
 {--where--}
  {--memModifyOps =--}
    {--[ 0x02 ,0x08 ,0x12 ,0x22 ,0x32 ,0x34 ,0x35 ,0x36 ,0x70 ,0x71--}
    {--, 0x72 ,0x73 ,0x74 ,0x75 ,0x77 ,0xc4 ,0xc5 ,0xc7 ,0xcc ,0xcd ,0xcf ,0xd4--}
    {--, 0xd5 ,0xd7 ,0xd9 ,0xdc ,0xdf ,0xe0 ,0xe2 ,0xe5 ,0xe7 ,0xea ,0xef ,0xf3--}
    {--, 0xf5 ,0xf7 ,0xfb ,0xff--}
    {--]--}
  {--cbMemModifyOps = --}
    {--[ 0x06, 0x0e, 0x16, 0x1e, 0x26, 0x2e, 0x36, 0x3e, 0x86, 0x8e, 0x96, 0x9e--}
    {--, 0xa6, 0xae, 0xb6, 0xbe, 0xc6, 0xce, 0xd6, 0xde, 0xe6, 0xee, 0xf6, 0xfe--}
    {--]--}

traceState :: Z80State -> Z80State
traceState state@{ mem = oldMem@{regs = oldRegs} } =
  condTr (shldTrc state) (\_ -> "totalM: "++show (state.totalM)) $
  {--condTr (shldTrc state) (\_ -> "totalM: "++show (state.totalM - 123958)) $--}

  {--condTr (shldTrc state) (\_ -> tileMap0Str state.mem) $--}
  {--condTr (shldTrc state) (\_ -> tileSet0Str state.mem) $--}

  {--condTr (shldTrc state) (\_ -> regsStr oldRegs) $--}
  {--condTr (shldTrc state) (\_ -> pcArea oldMem) $--}
  {--condTr (shldTrc state) (\_ -> spArea oldMem) $--}
  {--condTr shldTrc (\_ -> ioArea oldMem) $--}

  condTr (shldTrc state) (\_ -> "IntF: " ++ toHexStr 2 (getIntF state.mem.mainMem)) $
  condTr (shldTrc state) (\_ -> "IntE: " ++ toHexStr 2 (getIntE state.mem.mainMem)) $
  condTr (shldTrc state) (\_ -> "Ime: " ++ show (getIme state.mem.mainMem)) $

  {--condTr shldTrc (\_ -> "IntBits: " ++ toHexStr 2 intBits) $--}
  {--condTr shldTrc (\_ -> "getOnBit: " ++ show (getOnBit intBits)) $--}
  {--condTr shldTrc (\_ -> "shldHndlIntrr: " ++ show (shldHndlIntrr state)) $--}
  {--condTr shldTrc (\_ -> "intF after interruptop: " ++ toHexStr 2 (getIntF (interruptOp state).mem.mainMem)) $--}

  {--condTr (shldTrc state) (\_ -> "enbCnt: " ++ show (getImeEnableCnt state.mem.mainMem)) $--}

  {--condTr (shldTrc state) (\_ -> rd8RangeStr "FF40" 0xFF40 0xFF40 state.mem) $--}
  {--condTr (shldTrc state) (\_ -> rd8RangeStr "FF44" 0xFF44 0xFF44 state.mem) $--}
  state

shldTrc :: Z80State -> Boolean
{--shldTrc state = state.totalM == 233644--}
shldTrc state = false
{--shldTrc state = state.mem.regs.pc == 0x0405 || state.mem.regs.pc == 0x0371--}

