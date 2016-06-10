module Types where


import Prelude (class Eq, show, (++), (+), ($))
import Data.Int.Bits (shl)
import Control.Monad.Eff (Eff)

import Utils (toHexStr)


type Z80State =
  { mem :: Mem
  , totalM :: I8
  , halt :: Boolean
  , stop :: Boolean
  }

newtype OpCodeMap = OpCodeMap (Array (Z80State -> Z80State))

type Mem =
  { regs :: Regs
  , mainMem :: MainMem
  , svdRegs :: SavedRegs
  }

adjRegs :: (Regs -> Regs) -> Mem -> Mem
adjRegs func m = m { regs = func m.regs }

newtype MainMem = MainMem
  { biosMapped   :: Boolean
  , bios         :: Array I8
  , imeEnableCnt :: Int
  , ime          :: Boolean -- Interrupts master enable flag
  , intE         :: I8 -- Interrupt enable flags
  , intF         :: I8 -- Interrupt flags
  , gpu          :: Gpu
  , rom          :: MemSection
  , eram         :: MemSection
  , wram         :: MemSection
  , zram         :: MemSection
  }

--Effect for computations that read/write to 'MemSection's
foreign import data MemAccess :: !
--A java script array that is mutable
--through Eff computations with MemAccess effect
foreign import data MemSection :: *
--NOTE: make effect more granular, i.e. MemAccess(read) and MemModify(write)
foreign import data MemAccess :: !

type Gpu = 
  { mTimer    :: Int
  , dispOn    :: Boolean
  , bgOn      :: Boolean
  , bgMap1    :: Boolean
  , bgSet1    :: Boolean
  , vblFinish :: Boolean
  , currLine  :: Int
  , currPos   :: Int
  , yScroll   :: Int
  , xScroll   :: Int
  , vblIntrr  :: Boolean
  , palette   :: Palette
  , mode      :: GpuMode
  , tiles     :: Tiles
  , regs      :: MemSection
  , vram      :: MemSection
  , oam       :: MemSection
  }

type Color =
  { a :: I8
  , r :: I8
  , g :: I8
  , b :: I8
  }

newtype Tiles = Tiles MemSection

type Palette = MemSection

data Channel = Alpha
             | Red
             | Green
             | Blue

data GpuMode = HBlank
             | VBlank
             | OamScan
             | VramScan
derive instance eqGpuMode :: Eq GpuMode


foreign import data Regs :: *
foreign import data RegsAccess :: !
{--newtype Regs = Regs RegsObj--}

--NOTE; hide, RegsObj. Regs is a modifiable type.
--In order to ensure no one accesses it's content outside of an Eff (RegsAccess)
--computation, the inner object will be hidden
{--type RegsObj =--}
  {--{ pc  :: I16--}
  {--, sp  :: I16--}
  {--, a   :: I8--}
  {--, b   :: I8--}
  {--, c   :: I8--}
  {--, d   :: I8--}
  {--, e   :: I8--}
  {--, h   :: I8--}
  {--, l   :: I8--}
  {--, f   :: I8--}
  {--, brTkn :: Boolean--}
  {--}--}

type GetReg = Regs -> I8
type GetDblReg = Regs -> I16
type SetReg = forall e. I8 -> Regs -> Eff (ma :: MemAccess | e) Regs

type SavedRegs =
  { a :: I8
  , b :: I8
  , c :: I8
  , d :: I8
  , e :: I8
  , h :: I8
  , l :: I8
  , f :: I8
  }

{--pc :: Regs -> Int--}
{--pc = _.pc--}
{--sp :: Regs -> Int--}
{--sp = _.sp--}
{--a :: Regs -> Int--}
{--a  = _.a --}
{--b :: Regs -> Int--}
{--b  = _.b --}
{--c :: Regs -> Int--}
{--c  = _.c --}
{--d :: Regs -> Int--}
{--d  = _.d --}
{--e :: Regs -> Int--}
{--e  = _.e --}
{--h :: Regs -> Int--}
{--h  = _.h --}
{--l :: Regs -> Int--}
{--l  = _.l --}
{--f :: Regs -> Int--}
{--f  = _.f  --}

{--setA :: Int -> Regs -> Regs--}
{--setA x = _ { a = x }--}
{--setB :: Int -> Regs -> Regs--}
{--setB x = _ { b = x }--}
{--setC :: Int -> Regs -> Regs--}
{--setC x = _ { c = x }--}
{--setD :: Int -> Regs -> Regs--}
{--setD x = _ { d = x }--}
{--setE :: Int -> Regs -> Regs--}
{--setE x = _ { e = x }--}
{--setH :: Int -> Regs -> Regs--}
{--setH x = _ { h = x }--}
{--setL :: Int -> Regs -> Regs--}
{--setL x = _ { l = x }--}
{--setF :: Int -> Regs -> Regs--}
{--setF x = _ { f = x }--}

--NOTE:: Make these types 'newtype's so that you gain compile-time safety.
type I8 = Int
type I16 = Int
