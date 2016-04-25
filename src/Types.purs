module Types where

type Z80State =
  { mem :: Mem
  , totalM :: I8
  }

cleanState :: Z80State
cleanState = 
  { mem = cleanMem
  , totalM = 0
  }

type Mem =
  { regs :: Regs
  , mainMem :: MainMem
  }

cleanMem :: Mem
cleanMem =
  { regs = cleanRegs
  , mainMem = cleanMainMem
  }

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

cleanRegs :: Regs
cleanRegs =
  { pc = 0
  , sp = 0
  , m = 0
  , t = 0
  , a = 0
  , b = 0
  , c = 0
  , d = 0
  , e = 0
  , h = 0
  , l = 0
  , f = 0
  }

--NOTE:: If you make these types 'newtype's you gain compile-time safety
--at the cost of verbosity. Consider doing the change
type I8 = Number
type I16 = Number
