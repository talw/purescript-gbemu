module MainMem where

import Prelude ((>), ($), (-), (+), (<$>), return, (/=), (>=), bind, (<=)
               ,(&&), (==), (<), (||), negate)
import Data.Maybe (fromMaybe)
import Data.Int.Bits ((.&.), zshr, shl)
import Data.Array as A
import Data.Tuple as T
import Control.Monad.Eff (Eff)
import Control.Bind ((<=<))
import Data.Functor ((<$))


import Gpu (cleanGpu, gpuWr8, wrVRam, gpuRd8)
import Types (I8, Gpu, MemAccess, I16, MainMem(MainMem))
import MemSection as M

--Functions to set the different properties of MainMem, so that it oould be
--totally mutable. I didn't want to resort to these measures, but performance
--issues necessitated otherwise.
foreign import setVblIntrr :: forall e. Boolean -> MainMem -> Eff (ma :: MemAccess | e) MainMem
foreign import setIme :: forall e. Boolean -> MainMem -> Eff (ma :: MemAccess | e) MainMem
foreign import setImeCntDwn :: forall e. MainMem -> Eff (ma :: MemAccess | e) MainMem
foreign import setGpu :: forall e. Gpu -> MainMem -> Eff (ma :: MemAccess | e) MainMem
foreign import setIntF :: forall e. I8 -> MainMem -> Eff (ma :: MemAccess | e) MainMem
foreign import setIntE :: forall e. I8 -> MainMem -> Eff (ma :: MemAccess | e) MainMem

setRom :: Array I8 -> MainMem -> MainMem
setRom rom (MainMem mem) = MainMem $ mem { rom = M.fromIntArray rom }

--Writing a byte to memory, depending on the address. Delegating to other
--components if it is  needed.
--NOTE: for now, emulation starts after the bios stage,
--so effectively biosMapped will always be false.
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
              --TODO 0xDF is temporary until key input is implemented
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

--When reading 2 bytes the more significant byte part sits at the higher address
rd16 :: forall e. I16 -> MainMem -> Eff (ma :: MemAccess | e) I16
rd16 addr mem = do 
  l <- rd8 addr mem
  h <- rd8 (addr + 1) mem
  return $ (h `shl` 8) + l

--NOTE: Writing to parts that are not writable
--memory during execution should be treated with an error, unless they exist
--in regular roms and the Gameboy ignores them.
wr8 :: forall e. I8 -> I16 -> MainMem -> Eff (ma :: MemAccess | e) MainMem
wr8 i8 addr outerMM@(MainMem mem@{biosMapped,bios,rom,eram,wram,zram,gpu=gpu@{oam}}) =
  MainMem <$> eModifyMem
 where
  eModifyMem =
    case 0xF000.&.addr of
      n | 0 <= n && n <= 7 -> return mem -- NOTE error unwritable
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
              else return mem --NOTE unwritable
          0x0F00 ->
            case 0x00F0.&.addr of
              0x0000 -> if addr == 0xFF0F
                then mem <$ setIntF i8 outerMM
                else return mem
              n | 0x0040 <= n && n <= 0x0070 -> do
                gpu' <- gpuWr8 i8 addr gpu 
                mem <$ setGpu gpu' outerMM
              n | n >= 0x0080 -> if addr /= 0xFFFF
                then mem <$ M.replace i8 (0x7F .&. addr) zram
                else mem <$ setIntE i8 outerMM
              otherwise -> return mem --NOTE log this
          otherwise -> return mem --NOTE log this
      otherwise -> return mem --NOTE log this

--When writing 2 bytes the more significant byte part sits at the higher address
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
    , eram : M.getNew 8192 0xFF
    , wram : M.getNew 8192 0
    , zram : M.getNew 128 0
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

getGpu :: MainMem -> Gpu
getGpu (MainMem mm) = mm.gpu

getIntE :: MainMem -> I8
getIntE (MainMem mm) = mm.intE

getIntF :: MainMem -> I8
getIntF (MainMem mm) = mm.intF

getIme :: MainMem -> Boolean
getIme (MainMem mm) = mm.ime

getImeEnableCnt :: MainMem -> Int
getImeEnableCnt (MainMem mm) = mm.imeEnableCnt

updImeCnt :: MainMem -> MainMem
updImeCnt (MainMem mm@{ imeEnableCnt,ime }) = MainMem $
  if imeEnableCnt > 0
    then mm { imeEnableCnt = imeEnableCnt'
            , ime = if imeEnableCnt' > 0 then ime else true
            }
    else mm
 where imeEnableCnt' = imeEnableCnt - 1

--Debug functions
--NOTE: Change the debug functions to work with MemSections instead of 'Seq's

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
