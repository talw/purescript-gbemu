module MainMem where

import Prelude
import Math
import Data.Sequence as S
import Data.Maybe
import Data.Int.Bits
import Data.Array as A
import Data.Tuple as T
import Data.List as L
import Data.Foldable
import Control.Monad.Eff
import Control.Bind
import Data.Functor
import Data.Traversable

import Gpu
import Types
import Utils hiding((!!))
import MemSection as M
import Debug

--NOTE: optimization:
--Most of the memory is not writable, so consider
--saving it in a javascript array.
--But let that be transparent behind the reading/writing
--functions interface, i.e. decide where to read from depending
--on the address that is given as input.

-- 8 Bit version

foreign import setVblIntrr :: forall e. Boolean -> MainMem -> Eff (ma :: MemAccess | e) MainMem
foreign import setIme :: forall e. Boolean -> MainMem -> Eff (ma :: MemAccess | e) MainMem
foreign import setImeCntDwn :: forall e. MainMem -> Eff (ma :: MemAccess | e) MainMem
foreign import setGpu :: forall e. Gpu -> MainMem -> Eff (ma :: MemAccess | e) MainMem
foreign import setIntF :: forall e. I8 -> MainMem -> Eff (ma :: MemAccess | e) MainMem
foreign import setIntE :: forall e. I8 -> MainMem -> Eff (ma :: MemAccess | e) MainMem

setRom :: Array I8 -> MainMem -> MainMem
setRom rom (MainMem mem) = MainMem $ mem { rom = M.fromIntArray rom }

--NOTE: toggle biosMapped when pc == 0x0100
rd8 :: forall e. I16 -> MainMem -> Eff (ma :: MemAccess | e) I8
rd8 addr (MainMem mem@{biosMapped,bios,rom,eram,wram,zram, gpu = gpu@{vram,oam}}) =
  case 0xF000.&.addr of
    0x0000 -> if biosMapped && (addr < 0x0100)
             then return $ fromMaybe (-1) $ bios A.!! addr
             else rom M.!! addr
    n | 0x1000 <= n && n <= 0x7000 -> rom M.!! addr
    0x8000 -> vram M.!! (0x1FFF .&. addr)
    0x9000 -> vram M.!! (0x1FFF .&. addr)
    0xA000 -> eram M.!! (0x1FFF .&. addr)
    0xB000 -> eram M.!! (0x1FFF .&. addr)
    0xC000 -> wram M.!! (0x1FFF .&. addr)
    0xD000 -> wram M.!! (0x1FFF .&. addr)
    0xE000 -> wram M.!! (0x1FFF .&. addr)
    0xF000 ->
      case 0x0F00.&.addr of
        n | n < 0x0E00 -> wram M.!! (0x1FFF .&. addr)
        0x0E00 ->
          if addr < 0xFEA0
            then oam M.!! (0xFF .&. addr)
            else return $ -1 --NOTE log this
        0x0F00 ->
          case 0x00F0.&.addr of
            0x00 -> case addr of
              --TODO temporary until key input is implemented
              0xFF00 -> return 0xDF
              0xFF0F -> return mem.intF
              otherwise -> return $ -1 -- NOTE log this
            n | 0x0040 <= n && n <= 0x0070 -> gpuRd8 addr gpu
            n | n >= 0x0080 -> if addr /= 0xFFFF
              then zram M.!! (0x7F .&. addr)
              else return mem.intE
            otherwise -> return $ -1 --NOTE log this
        otherwise -> return $ -1 --NOTE log this
    otherwise -> return $ -1 --NOTE log this

--NOTE: make sure the significant byte part sits at a higher address
rd16 :: forall e. I16 -> MainMem -> Eff (ma :: MemAccess | e) I16
rd16 addr mem = do 
  l <- rd8 addr mem
  h <- rd8 (addr + 1) mem
  return $ (h `shl` 8) + l

--NOTE: temporary implementation. Writing to parts that are not writable
--memory during execution should be treated with an error.
--The initial writing to them should be ST array computations,
--and not wr8 calls
wr8 :: forall e. I8 -> I16 -> MainMem -> Eff (ma :: MemAccess | e) MainMem
wr8 i8 addr outerMM@(MainMem mem@{biosMapped,bios,rom,eram,wram,zram,gpu=gpu@{oam}}) =
  MainMem <$> eModifyMem
 where
  eModifyMem =
    case 0xF000.&.addr of
      n | 0 <= n && n <= 7 -> return mem -- NOTE error can't write to bios or rom
      --{ vram = M.replace i8 (0x1FFF .&. addr) vram }
      n | n == 0x8000 || n == 0x9000 -> mem <$ wrVRam i8 addr gpu 
      0xA000 -> mem <$ M.replace i8 (0x1FFF .&. addr) eram
      0xB000 -> mem <$ M.replace i8 (0x1FFF .&. addr) eram
      0xC000 -> mem <$ M.replace i8 (0x1FFF .&. addr) wram
      0xD000 -> mem <$ M.replace i8 (0x1FFF .&. addr) wram
      0xE000 -> mem <$ M.replace i8 (0x1FFF .&. addr) wram
      0xF000 ->
        case 0x0F00.&.addr of
          n | n < 0x0E00 -> mem <$ M.replace i8 (0x1FFF .&. addr) wram
          0x0E00 ->
            if addr < 0xFEA0
              then mem <$ M.replace i8 (0xFF .&. addr) oam 
              {--then _ { gpu = gpu { oam = M.replace i8 (0xFF .&. addr) oam } }--}
              else return mem --NOTE unwritable, log this
          0x0F00 ->
            case 0x00F0.&.addr of
              0x0000 -> if addr == 0xFF0F
                then mem <$ setIntF i8 outerMM --return mem { intF = i8 }
                else return mem
              n | 0x0040 <= n && n <= 0x0070 -> do
                gpu' <- gpuWr8 i8 addr gpu 
                mem <$ setGpu gpu' outerMM --return mem { gpu = gpu' }
              n | n >= 0x0080 -> if addr /= 0xFFFF
                then mem <$ M.replace i8 (0x7F .&. addr) zram
                else mem <$ setIntE i8 outerMM  --return mem { intE = i8 }
              otherwise -> return mem --NOTE log this
          otherwise -> return mem --NOTE log this
      otherwise -> return mem --NOTE log this

wr16 :: forall e. I16 -> I16 -> MainMem -> Eff (ma :: MemAccess | e) MainMem
wr16 i16 addr mem = do
      wr8 h (addr + 1)
  <=< wr8 l addr
   $  mem
 where
  h = 255 .&. (i16 `zshr` 8)
  l = 255 .&. i16

cleanMainMem :: forall e. Eff (ma :: MemAccess | e) MainMem
cleanMainMem =
  initIOArea $ MainMem
    { biosMapped : false
    , imeEnableCnt : 0
    , ime : true
    , intE : 0
    , intF : 0
    , rom  : M.fromIntArray $ A.singleton 0
    , eram : M.getNew 8192 0xFF --S.fromFoldable $ A.replicate 8192 0xFF
    , wram : M.getNew 8192 0 --S.fromFoldable $ A.replicate 8192 0
    , zram : M.getNew 128 0 --S.fromFoldable $ A.replicate 128 0
    , gpu  : cleanGpu
    , bios : A.singleton 0
    }
 where
  ioMem = A.zip (0xFF00 A... 0xFF7F) [
    0xCF,0x00,0x7E,0xFF,0xAF,0x00,0x00,0xF8,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xE1,
    0x80,0xBF,0xF3,0xFF,0xBF,0xFF,0x3F,0x00,0xFF,0xBF,0x7F,0xFF,0x9F,0xFF,0xBF,0xFF,
    0xFF,0x00,0x00,0xBF,0x77,0xF3,0xF1,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x06,0xFE,0x0E,0x7F,0x00,0xFF,0x58,0xDF,0x00,0xEC,0x00,0xBF,0x0C,0xED,0x03,0xF7,
    0x91,0x85,0x00,0x00,0x00,0x00,0x00,0xFC,0xFF,0xFF,0x00,0x00,0xFF,0x7E,0xFF,0xFE,
    0xFF,0x00,0x00,0x00,0x00,0xFF,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xC0,0x00,0xC1,0x00,0x00,0x00,0x00,0x00,
    0xF8,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
  ]
  initIOArea mm = A.foldM (\acc (T.Tuple addr b) -> wr8 b addr acc) mm ioMem
  {--initIOArea mm = mm <$ traverse (\(T.Tuple addr b) -> wr8 b addr mm) ioMem--}

getGpu :: MainMem -> Gpu
getGpu (MainMem mm) = mm.gpu

{--setGpu :: Gpu -> MainMem -> MainMem--}
{--setGpu gpu' (MainMem mm) = MainMem $ mm { gpu = gpu' }--}

getIntE :: MainMem -> I8
getIntE (MainMem mm) = mm.intE

getIntF :: MainMem -> I8
getIntF (MainMem mm) = mm.intF

getIme :: MainMem -> Boolean
getIme (MainMem mm) = mm.ime

getImeEnableCnt :: MainMem -> Int
getImeEnableCnt (MainMem mm) = mm.imeEnableCnt

{--setIntE :: I8 -> MainMem -> MainMem--}
{--setIntE val (MainMem mm) = MainMem $ mm { intE = val }--}

{--setIntF :: I8 -> MainMem -> MainMem--}
{--setIntF val (MainMem mm) = MainMem $ mm { intF = val }--}

{--setIme :: Boolean -> MainMem -> MainMem--}
{--setIme val (MainMem mm) = MainMem $ mm { ime = val }--}

--NOTE: imeEnableCnt should be 2 according to specs,
--but according to simulations I've run it looks like it's 3
{--imeCntDwn :: MainMem -> MainMem--}
{--imeCntDwn (MainMem mm@{ imeEnableCnt }) = MainMem $ mm { imeEnableCnt = 3 }--}

updImeCnt :: MainMem -> MainMem
updImeCnt (MainMem mm@{ imeEnableCnt,ime }) = MainMem $
  if imeEnableCnt > 0
    then mm { imeEnableCnt = imeEnableCnt'
            , ime = if imeEnableCnt' > 0 then ime else true
            }
    else mm
 where imeEnableCnt' = imeEnableCnt - 1

    {--, bios :  [--}
      {--0x31, 0xFE, 0xFF, 0xAF, 0x21, 0xFF, 0x9F, 0x32,--}
      {--0xCB, 0x7C, 0x20, 0xFB, 0x21, 0x26, 0xFF, 0x0E,--}
      {--0x11, 0x3E, 0x80, 0x32, 0xE2, 0x0C, 0x3E, 0xF3,--}
      {--0xE2, 0x32, 0x3E, 0x77, 0x77, 0x3E, 0xFC, 0xE0,--}
      {--0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1A,--}
      {--0xCD, 0x95, 0x00, 0xCD, 0x96, 0x00, 0x13, 0x7B,--}
      {--0xFE, 0x34, 0x20, 0xF3, 0x11, 0xD8, 0x00, 0x06,--}
      {--0x08, 0x1A, 0x13, 0x22, 0x23, 0x05, 0x20, 0xF9,--}
      {--0x3E, 0x19, 0xEA, 0x10, 0x99, 0x21, 0x2F, 0x99,--}
      {--0x0E, 0x0C, 0x3D, 0x28, 0x08, 0x32, 0x0D, 0x20,--}
      {--0xF9, 0x2E, 0x0F, 0x18, 0xF3, 0x67, 0x3E, 0x64,--}
      {--0x57, 0xE0, 0x42, 0x3E, 0x91, 0xE0, 0x40, 0x04,--}
      {--0x1E, 0x02, 0x0E, 0x0C, 0xF0, 0x44, 0xFE, 0x90,--}
      {--0x20, 0xFA, 0x0D, 0x20, 0xF7, 0x1D, 0x20, 0xF2,--}
      {--0x0E, 0x13, 0x24, 0x7C, 0x1E, 0x83, 0xFE, 0x62,--}
      {--0x28, 0x06, 0x1E, 0xC1, 0xFE, 0x64, 0x20, 0x06,--}
      {--0x7B, 0xE2, 0x0C, 0x3E, 0x87, 0xF2, 0xF0, 0x42,--}
      {--0x90, 0xE0, 0x42, 0x15, 0x20, 0xD2, 0x05, 0x20,--}
      {--0x4F, 0x16, 0x20, 0x18, 0xCB, 0x4F, 0x06, 0x04,--}
      {--0xC5, 0xCB, 0x11, 0x17, 0xC1, 0xCB, 0x11, 0x17,--}
      {--0x05, 0x20, 0xF5, 0x22, 0x23, 0x22, 0x23, 0xC9,--}
      {--0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,--}
      {--0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,--}
      {--0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,--}
      {--0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,--}
      {--0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,--}
      {--0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,--}
      {--0x3c, 0x42, 0xB9, 0xA5, 0xB9, 0xA5, 0x42, 0x4C,--}
      {--0x21, 0x04, 0x01, 0x11, 0xA8, 0x00, 0x1A, 0x13,--}
      {--0xBE, 0x20, 0xFE, 0x23, 0x7D, 0xFE, 0x34, 0x20,--}
      {--0xF5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20,--}
      {--0xFB, 0x86, 0x20, 0xFE, 0x3E, 0x01, 0xE0, 0x50--}
    {--]--}

--Debug functions

{--ioArea :: Mem -> String--}
{--ioArea = rd8RangeStr "IO FF00-FF7F" 0xFF00 0XFF7F--}

{--rd8RangeStr :: String -> Int -> Int -> Mem -> String--}
{--rd8RangeStr name from to { mainMem } = name ++ memStrRange 0 (to-from)--}
  {--(map (flip rd8 mainMem) $ S.fromFoldable (from A... to))--}

{--pcArea :: Mem -> String--}
{--pcArea { mainMem, regs } = "pcArea: " ++--}
  {--prt 0 ++ " " ++ prt 1 ++ " " ++ prt 2--}
 {--where--}
  {--prt d = toHexStr 2 $ rd8 (regs.pc + d) mainMem --}

{--spArea :: Mem -> String--}
{--spArea mem@{ mainMem=(MainMem mm), regs } =--}
  {--rd8RangeStr "spArea" regs.sp (regs.sp + 15) mem--}

{--gpuRegsStr :: Mem -> String--}
{--gpuRegsStr { mainMem = (MainMem { gpu })} = "gpu regs FF40-FF7F"--}
  {--++ show (map (toHexStr 2 <<< flip gpuRd8 gpu) $ 0xFF40 A... 0xFF47) ++ "\n"--}
  {--++ memStrRange 0 (S.length gpu.regs) gpu.regs--}

{--tileSet0Str :: Mem -> String--}
{--tileSet0Str { mainMem = (MainMem mm) } = "tile set 0" --}
  {--++ memStrRange (0x8000-0x8000) (0x97FF-0x8000) mm.gpu.vram--}

{--tileMap0Str :: Mem -> String--}
{--tileMap0Str { mainMem = (MainMem mm) } = "tile map 0" --}
  {--++ memStrRange (0x9800-0x8000) (0x9BFF-0x8000) mm.gpu.vram--}
