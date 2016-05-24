module Core where

import Prelude
import Data.Int.Bits
import Data.Tuple
import Data.Maybe
import Data.Either
import Control.Monad.Eff

import Types
import MainMem
import Ops
import OpCodeMap
import Gpu
import Utils
import Control.Monad.Rec.Class

import Debug

reset :: forall e. Array I8 -> Eff (canvas :: Canvas | e) Z80State
reset rom = do
  resetScreen
  return 
    cleanState {
      mem = cleanMem {
        mainMem = setRom rom cleanMainMem
      }
    }


run :: forall e. Int -> Z80State -> Eff (canvas :: Canvas | e) Z80State
run interval state = tailRecM go { intr : interval, st : state }
 where
  go { st = st@{stop = true} } = return $ Right state
  go { intr, st } | intr <= 0 = return $ Right state
  go { intr, st } = do
    st' <- (if shldTrc st then trace "-------step-------" \_ -> step st else step st)
    let elapsed = st'.totalM - st.totalM
    return $ Left { intr : (intr - elapsed), st : st' }


--NOTE Reg r should be increased by 1?
step :: forall e. Z80State -> Eff (canvas :: Canvas | e) Z80State
step state@{ mem = oldMem@{regs = oldRegs} } =
  handleGpu <<< incTime oldRegs.pc <<< incPc oldRegs.pc <<< op $
    traceState $ state
 where

  op st = if shldHndlIntrr st
    then interruptOp st
    else regularOp st
  regularOp st = getCpuOp (getCurrOpCode st) basicOps $ st
  getCurrOpCode st = rd8 st.mem.regs.pc st.mem.mainMem

incTime :: I8 -> Z80State -> Z80State
incTime oldPc st@{totalM} = st
  { totalM = st.totalM + opTiming }
 where
  opTiming = getOpTiming addr opTimingTable
  addr = if opCode == 0xCB
    then rd8 (oldPc+1) st.mem.mainMem
    else opCode
  opTimingTable = if opCode == 0xCB
    then cbOpTimings 
    else
      if st.mem.regs.brTkn
        then branchBasicOpTimings
        else basicOpTimings
  opCode = rd8 oldPc st.mem.mainMem

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


handleGpu :: forall e. Z80State -> Eff (canvas :: Canvas | e) Z80State
handleGpu st = do
  gpu' <- gpuStep st.mem.regs.m (getGpu st.mem.mainMem)
  let intF' = (if gpu'.vblIntrr then (0x01 .|. _) else id) $ getIntF st.mem.mainMem
  return $ st { mem = st.mem { mainMem =
        setIntF intF'
    <<< setGpu (gpu' { vblIntrr = false })
     $  st.mem.mainMem } }

interruptOp :: Z80State -> Z80State
interruptOp st = snd res $
  st { mem = st.mem {
    mainMem = setIme false <<< setIntF (fst res $ getIntF st.mem.mainMem) $ st.mem.mainMem } }
 where
  res :: Tuple (Int -> Int) (Z80State -> Z80State)
  res = fromMaybe (Tuple id id) mRes
  mRes = getOnBit (interruptBits st) >>= \x -> case x of
           1  -> Just $ Tuple (0xFE .&. _) (mm2op $ callRoutine 0x40)
           2  -> Just $ Tuple (0xFD .&. _) (mm2op $ callRoutine 0x48)
           4  -> Just $ Tuple (0xFB .&. _) (mm2op $ callRoutine 0x50)
           8  -> Just $ Tuple (0xF7 .&. _) (mm2op $ callRoutine 0x58)
           16 -> Just $ Tuple (0xEF .&. _) (mm2op $ callRoutine 0x60)
           otherwise -> Nothing -- NOTE log this

interruptBits :: Z80State -> I8
interruptBits st = getIntE st.mem.mainMem .&. getIntF st.mem.mainMem

shldHndlIntrr :: Z80State -> Boolean
shldHndlIntrr st = getIme st.mem.mainMem && interruptBits st /= 0


cleanState :: Z80State
cleanState = 
  { mem : cleanMem
  , totalM : 1
  , halt : false
  , stop : false
  }

cleanMem :: Mem
cleanMem =
  { regs : cleanRegs
  , svdRegs : cleanSavedRegs
  , mainMem : cleanMainMem
  }

cleanRegs :: Regs
cleanRegs =
  { pc    : 0x0101
  , sp    : 0xFFFE
  , m     : 0
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

traceState :: Z80State -> Z80State
traceState state@{ mem = oldMem@{regs = oldRegs} } =
  condTr shldTrc (\_ -> "-----------") $
  condTr shldTrc (\_ -> "totalM: "++show state.totalM) $

  {--condTr shldTrc (\_ -> tileMap0Str mem) $--}

  condTr shldTrc (\_ -> ioArea oldMem) $
  condTr shldTrc (\_ -> pcArea oldMem) $
  condTr shldTrc (\_ -> spArea oldMem) $

  {--condTr shldTrc (\_ -> "IntF: " ++ toHexStr 2 intF) $--}
  {--condTr shldTrc (\_ -> "IntE: " ++ toHexStr 2 intE) $--}
  {--condTr shldTrc (\_ -> "Ime: " ++ show (getIme state.mem.mainMem)) $--}
  {--condTr shldTrc (\_ -> "IntBits: " ++ toHexStr 2 intBits) $--}
  {--condTr shldTrc (\_ -> "getOnBit: " ++ show (getOnBit intBits)) $--}
  {--condTr shldTrc (\_ -> "shldHndlIntrr: " ++ show (shldHndlIntrr state)) $--}
  {--condTr shldTrc (\_ -> "intF after interruptop: " ++ toHexStr 2 (getIntF (interruptOp state).mem.mainMem)) $--}
  {--condTr (shldTrc state) (\_ -> regsStr oldRegs) $--}
  condTr (shldTrc state) (\_ -> rd8RangeStr "FF44" 0xFF44 0xFF44 state.mem) $
  state

shldTrc :: Z80State -> Boolean
{--shldTrc state = state.totalM >= 123000--}
shldTrc state = true
{--shldTrc state = state.mem.regs.pc == 0x0200--}
{--shldTrc = oldRegs.pc == 0x0371 || oldRegs.pc == 0x0200--}

regsStr :: Regs -> String
regsStr regs = "af: "    ++ af
          ++ "\nbc: "    ++ bc
          ++ "\nde: "    ++ de
          ++ "\nhl: "    ++ hl
          ++ "\nsp: "    ++ sp
          ++ "\npc: "    ++ pc
          ++ "\nbrTkn: " ++ show regs.brTkn
 where
  af = toHexStr 4 $ joinRegs a f regs
  bc = toHexStr 4 $ joinRegs b c regs
  de = toHexStr 4 $ joinRegs d e regs
  hl = toHexStr 4 $ joinRegs h l regs
  sp = toHexStr 4 $ regs.sp
  pc = toHexStr 4 $ regs.pc

