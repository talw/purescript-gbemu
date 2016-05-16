module Core where

import Prelude
import Data.Int.Bits
import Data.Maybe
import Control.Monad.Eff

import Types
import MainMem
import Ops
import OpCodeMap
import Gpu
import Utils

reset :: Z80State -> Z80State
reset state =
  cleanState {
    mem = cleanMem {
      mainMem = state.mem.mainMem 
    }
  }

--NOTE is there a better way to set nested properties?
--consider checking the lens-equivalent in purescript
step :: Z80State -> Z80State
step state = incTime <<< incPc (state.mem.regs.pc) <<< op $ trace (mainMemStr state.mem.mainMem) \_ -> trace (regsStr state.mem.regs) \_ -> state
  where
    -- NOTE: change fromMaybe with something that will log exceptional cases.
    op = getOpcode (trh "opCode" opCode) basicOps
    opCode = rd8 state.mem.regs.pc state.mem.mainMem
    incTime state@{totalM} = state
      { totalM = state.totalM + state.mem.regs.m }
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
