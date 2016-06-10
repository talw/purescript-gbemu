module Regs where

import Prelude
import Control.Monad.Eff

import Types
import Utils
import Data.Int.Bits
import MemSection as M

import Debug

foreign import data RegsAccess :: !

foreign import getNewRegs :: forall e. Eff (ma :: MemAccess | e) Regs

foreign import pc    :: Regs -> {-Eff (ra :: RegsAccess | e)-} I16
foreign import sp    :: Regs -> {-Eff (ra :: RegsAccess | e)-} I16
foreign import a     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import b     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import c     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import d     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import e     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import h     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import l     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import f     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import af     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import bc     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import de     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import hl     :: Regs -> {-Eff (ra :: RegsAccess | e)-} I8
foreign import brTkn     :: Regs -> {-Eff (ra :: RegsAccess | e)-} Boolean

foreign import setPC    :: forall e. I16 -> Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setSP    :: forall e. I16 -> Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setA     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setF     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setB     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setC     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setD     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setE     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setH     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setL     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setBrTkn :: forall e. Boolean ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setAF     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setBC     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setDE     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
foreign import setHL     :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs

cleanRegs :: forall e. Eff (ma :: MemAccess | e) Regs
cleanRegs = do
  regs <- getNewRegs
  setPC 0x0101 regs
  setSP 0xFFFE regs
  setA  0x01   regs
  setB  0      regs
  setC  0x13   regs
  setD  0      regs
  setE  0xD8   regs
  setH  0x01   regs
  setL  0x4D   regs
  setF  0xB0   regs
  setBrTkn false regs
  {--return $ trace (regsStr regs) \_ -> regs--}
  return regs

{--Regs--}
  {--{ pc    : 0x0101--}
  {--, sp    : 0xFFFE--}
  {--, a     : 0x01--}
  {--, b     : 0--}
  {--, c     : 0x13--}
  {--, d     : 0--}
  {--, e     : 0xD8--}
  {--, h     : 0x01--}
  {--, l     : 0x4D--}
  {--, f     : 0xB0--}
  {--, brTkn : false--}
  {--}--}

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

regsStr :: Regs -> String
regsStr regs = "af: "    ++ af'
          ++ "\nbc: "    ++ bc'
          ++ "\nde: "    ++ de'
          ++ "\nhl: "    ++ hl'
          ++ "\nsp: "    ++ sp'
          ++ "\npc: "    ++ pc'
          ++ "\nbrTkn: " ++ show (brTkn regs)
 where
  af' = toHexStr 4 $ af regs
  {--af = toHexStr 4 $ joinRegs a f regs--}
  bc' = toHexStr 4 $ bc regs
  de' = toHexStr 4 $ de regs
  hl' = toHexStr 4 $ hl regs
  sp' = toHexStr 4 $ sp regs
  pc' = toHexStr 4 $ pc regs
  {--joinRegs msByteReg lsByteReg regs = (msByteReg regs `shl` 8) + lsByteReg regs--}
