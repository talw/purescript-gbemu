module Core where

import Prelude
import Data.Int.Bits
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
  op = getOpcode opCode {-(trh "opCode" opCode)-} basicOps
  opCode = rd8 regs.pc mem.mainMem

  incTime st@{totalM} = st
    { totalM = st.totalM + regs.m }

  hndlGpu st = do
    gpu' <- gpuStep regs.m (getGpu st.mem.mainMem)
    return $ st { mem = st.mem { mainMem = setGpu gpu' st.mem.mainMem } }

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
  { pc : 0
  , sp : 0
  , ime : false
  , m  : 0
  , a  : 0
  , b  : 0
  , c  : 0
  , d  : 0
  , e  : 0
  , h  : 0
  , l  : 0
  , f  : 0
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
