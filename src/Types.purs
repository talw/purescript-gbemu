module Types where

import Data.Sequence

type Z80State =
  { mem :: Mem
  , totalM :: I8
  }

type Mem =
  { regs :: Regs
  , mainMem :: MainMem
  }

newtype MainMem = MainMem (Seq I16)

type Regs =
  { pc :: I16
  , sp :: I16
  , m :: I8
  , a :: I8
  , b :: I8
  , c :: I8
  , d :: I8
  , e :: I8
  , h :: I8
  , l :: I8
  , f :: I8
  }

{--pc, sp, a, b, c, d, e, h, l, f :: Regs -> Number--}
pc { pc } = pc
sp { sp } = sp
a { a } = a
b { b } = b
c { c } = c
d { d } = d
e { e } = e
h { h } = h
l { l } = l
f { f } = f

{--setA, setB, setC, setD, setE, setH, setL, setF :: Number -> Regs -> Regs--}
setA x = _ { a = x }
setB x = _ { b = x }
setC x = _ { c = x }
setD x = _ { d = x }
setE x = _ { e = x }
setH x = _ { h = x }
setL x = _ { l = x }
setF x = _ { f = x }

--NOTE:: If you make these types 'newtype's you gain compile-time safety
--at the cost of verbosity. Consider doing the change
type I8 = Int
type I16 = Int
