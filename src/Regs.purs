module Regs where

import Prelude
import Control.Monad.Eff

import Types
import MemSection as M

foreign import data RegsAccess :: !

--{--foreign import innerPC    :: forall e. RegsObj -> {-Eff (ra :: RegsAccess | e)-} I16-}
--{--foreign import innerSP    :: forall e. RegsObj -> {-Eff (ra :: RegsAccess | e)-} I16-}
--{--foreign import innerA     :: forall e. RegsObj -> {-Eff (ra :: RegsAccess | e)-} I8-}
--{--foreign import innerB     :: forall e. RegsObj -> {-Eff (ra :: RegsAccess | e)-} I8-}
--{--foreign import innerC     :: forall e. RegsObj -> {-Eff (ra :: RegsAccess | e)-} I8-}
--{--foreign import innerD     :: forall e. RegsObj -> {-Eff (ra :: RegsAccess | e)-} I8-}
--{--foreign import innerE     :: forall e. RegsObj -> {-Eff (ra :: RegsAccess | e)-} I8-}
--{--foreign import innerH     :: forall e. RegsObj -> {-Eff (ra :: RegsAccess | e)-} I8-}
--{--foreign import innerL     :: forall e. RegsObj -> {-Eff (ra :: RegsAccess | e)-} I8-}
--{--foreign import innerF     :: forall e. RegsObj -> {-Eff (ra :: RegsAccess | e)-} I8-}
--{--foreign import innerBrTkn :: forall e. RegsObj -> {-Eff (ra :: RegsAccess | e)-} I8-}

foreign import innerSetPC    :: forall e. I16 -> RegsObj -> Eff (ma :: MemAccess | e) RegsObj
foreign import innerSetSP    :: forall e. I16 -> RegsObj -> Eff (ma :: MemAccess | e) RegsObj
foreign import innerSetA     :: forall e. I8 ->  RegsObj -> Eff (ma :: MemAccess | e) RegsObj
foreign import innerSetB     :: forall e. I8 ->  RegsObj -> Eff (ma :: MemAccess | e) RegsObj
foreign import innerSetC     :: forall e. I8 ->  RegsObj -> Eff (ma :: MemAccess | e) RegsObj
foreign import innerSetD     :: forall e. I8 ->  RegsObj -> Eff (ma :: MemAccess | e) RegsObj
foreign import innerSetE     :: forall e. I8 ->  RegsObj -> Eff (ma :: MemAccess | e) RegsObj
foreign import innerSetH     :: forall e. I8 ->  RegsObj -> Eff (ma :: MemAccess | e) RegsObj
foreign import innerSetL     :: forall e. I8 ->  RegsObj -> Eff (ma :: MemAccess | e) RegsObj
foreign import innerSetF     :: forall e. I8 ->  RegsObj -> Eff (ma :: MemAccess | e) RegsObj
foreign import innerSetBrTkn :: forall e. Boolean ->  RegsObj -> Eff (ma :: MemAccess | e) RegsObj

cleanRegs :: Regs
cleanRegs = Regs
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

--NOTE are these wrappers necessary? Or can you pass Regs straight in to the js functions?
pc :: forall e. Regs -> {-Eff (ra :: RegsAccess | e)-} I16
pc (Regs rs) = rs.pc

sp :: forall e. Regs -> {-Eff (ra :: RegsAccess | e)-} I16
sp (Regs rs) = rs.sp

a :: forall e. Regs -> {-Eff (ra :: RegsAccess | e)-} I8
a (Regs rs) = rs.a

b :: forall e. Regs -> {-Eff (ra :: RegsAccess | e)-} I8
b (Regs rs) = rs.b

c :: forall e. Regs -> {-Eff (ra :: RegsAccess | e)-} I8
c (Regs rs) = rs.c

d :: forall e. Regs -> {-Eff (ra :: RegsAccess | e)-} I8
d (Regs rs) = rs.d

e :: forall e. Regs -> {-Eff (ra :: RegsAccess | e)-} I8
e (Regs rs) = rs.e

h :: forall e. Regs -> {-Eff (ra :: RegsAccess | e)-} I8
h (Regs rs) = rs.h

l :: forall e. Regs -> {-Eff (ra :: RegsAccess | e)-} I8
l (Regs rs) = rs.l

f :: forall e. Regs -> {-Eff (ra :: RegsAccess | e)-} I8
f (Regs rs) = rs.f

brTkn :: forall e. Regs -> {-Eff (ra :: RegsAccess | e)-} Boolean
brTkn (Regs rs) = rs.brTkn

setPC :: forall e. I16 ->  Regs -> Eff (ma :: MemAccess | e) Regs
setPC val (Regs rs) = Regs <$> innerSetPC val rs

setSP :: forall e. I16 ->  Regs -> Eff (ma :: MemAccess | e) Regs
setSP val (Regs rs) = Regs <$> innerSetSP val rs

setA :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
setA val (Regs rs) = Regs <$> innerSetA val rs

setB :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
setB val (Regs rs) = Regs <$> innerSetB val rs

setC :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
setC val (Regs rs) = Regs <$> innerSetC val rs

setD :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
setD val (Regs rs) = Regs <$> innerSetD val rs

setE :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
setE val (Regs rs) = Regs <$> innerSetE val rs

setH :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
setH val (Regs rs) = Regs <$> innerSetH val rs

setL :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
setL val (Regs rs) = Regs <$> innerSetL val rs

setF :: forall e. I8 ->  Regs -> Eff (ma :: MemAccess | e) Regs
setF val (Regs rs) = Regs <$> innerSetF val rs

setBrTkn :: forall e. Boolean ->  Regs -> Eff (ma :: MemAccess | e) Regs
setBrTkn val (Regs rs) = Regs <$> innerSetBrTkn val rs
