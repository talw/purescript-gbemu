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
    st' <- step st
    let elapsed = st'.totalM - st.totalM
    return $ Left { intr : (intr - elapsed), st : st' }

--NOTE Reg r should be increased by 1?
step :: forall e. Z80State -> Eff (canvas :: Canvas | e) Z80State
step state@{ mem=mem@{regs} } =
  hndlGpu <<< incTime <<< incPc (regs.pc) <<< op $
    {--trace (gpuRegsStr mem) \_ ->--}
    {--trace (pcArea mem) \_ ->--}
    {--trace (spArea mem) \_ ->--}
    {--trace (regsStr regs) \_ ->--}
    state
 where
  -- NOTE: change fromMaybe with something that will log exceptional cases.

  incTime st@{totalM} = st
    { totalM = st.totalM + regs.m }

  hndlGpu st = do
    gpu' <- gpuStep regs.m (getGpu st.mem.mainMem)
    return $ st { mem = st.mem { mainMem = setGpu gpu' st.mem.mainMem } }

  op = if shldHndlIntrr
    then interruptOp
    else regularOp

  regularOp = getOpcode opCode basicOps
  opCode = rd8 regs.pc mem.mainMem

  --disable ime
  interruptOp :: Z80State -> Z80State
  interruptOp st = snd res $
    st { mem = st.mem {
      mainMem = setIme false <<< setIntF (fst res) $ st.mem.mainMem } }
   where
    res :: Tuple Int (Z80State -> Z80State)
    res = fromMaybe (Tuple intF id) mRes
    mRes = getOnBit intBits >>= \x -> case x of
             1  -> Just $ Tuple (0xFE .&. intF) (mm2op $ callRoutine 0x40)
             2  -> Just $ Tuple (0xFD .&. intF) (mm2op $ callRoutine 0x48)
             4  -> Just $ Tuple (0xFB .&. intF) (mm2op $ callRoutine 0x50)
             8  -> Just $ Tuple (0xF7 .&. intF) (mm2op $ callRoutine 0x58)
             16 -> Just $ Tuple (0xEF .&. intF) (mm2op $ callRoutine 0x60)
             otherwise -> Nothing -- NOTE log this

  shldHndlIntrr = getIme state.mem.mainMem && intBits /= 0
  intBits = intE .&. intF
  intE = getIntE state.mem.mainMem
  intF = getIntF state.mem.mainMem
       

  --NOTE is there a better way to set nested properties?
  --consider checking the lens-equivalent in purescript.
  incPc oldPc newState =
    if oldPc /= newState.mem.regs.pc
      then newState
      else
        newState {
          mem = newState.mem {
            regs = newState.mem.regs {
              pc = 65535 .&. (newState.mem.regs.pc + 1)
            }
          }
        }

cleanState :: Z80State
cleanState = 
  { mem : cleanMem
  , totalM : 0
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
  { pc : 0x0200
  , sp : 0xCFF7
  , ime : true
  , m  : 0
  , a  : 0
  , b  : 0
  , c  : 0
  , d  : 0
  , e  : 0xD8
  , h  : 0x97
  , l  : 0xFF
  , f  : 0x80
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
