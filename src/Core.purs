module Core where

import Prelude
import Data.Int.Bits

import Types
import MainMem
import Ops

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
step = incTime <<< incPc <<< op 
  where
    op :: Z80State -> Z80State
    op = id -- TODO: undefined
    incTime state@{ totalM } = state
      { totalM = totalM + state.mem.regs.m }
    incPc state@{ mem }=
      state {
        mem = mem {
          regs = mem.regs {
            pc = (mem.regs.pc + 1) .&. 255
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
