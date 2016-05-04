module MainMem where

import Prelude
import Math
import Data.Sequence as S
import Data.Maybe
import Data.Int.Bits
import Data.Array as A

import Types

-- 8 Bit version

cleanMainMem :: MainMem
cleanMainMem = MainMem $ S.fromFoldable $ A.replicate 65536 0

--NOTE replace all 'fromMaybe's with an error mechanism that will make it easy
--to trace Nothing cases

rd8 :: I16 -> MainMem -> I8
rd8 addr (MainMem mem) =
  fromMaybe 0 $ S.index addr mem

--NOTE make sure the significant byte part sits at a higher address
rd16 :: I16 -> MainMem -> I16
rd16 addr (MainMem mem) = (h `shl` 8) + l
 where
  l = fromMaybe 0 $ S.index addr mem
  h = fromMaybe 0 $ S.index (addr + 1) mem

wr8 :: I8 -> I16 -> MainMem -> MainMem
wr8 i8 addr (MainMem mem) = MainMem $ S.replace i8 addr mem

wr16 :: I16 -> I16 -> MainMem -> MainMem
wr16 i16 addr (MainMem mem) = MainMem $
      S.replace h (addr + 1)
  <<< S.replace l addr
   $  mem
 where
  h = 255 .&. (i16 `zshr` 8)
  l = 255 .&. i16

--16 Bit version

--NOTE consider splitting the mems to a generic number of chunks N
--that you can easily change, and then profile the performance for different N
--Or consider using array ST, if in need of better performance

--memory address
{--0 1 2 3 4 ...--}
{--0 0 1 1 2 ...--}
--seq index

{--the quotient is the index--}
{--the remainder is the offset inside the chunk--}
--Chunk size in bytes
{--chkSz :: Int--}
{--chkSz = 2--}

{--cleanMainMem :: MainMem--}
{--cleanMainMem = MainMem $ S.fromFoldable $ A.replicate size 0--}
 {--where--}
  {--size = 65536 / chkSz--}

{--rd8 :: I16 -> MainMem -> I8--}
{--rd8 addr (MainMem mem) = lsByteOf $--}
  {--(if isShiftNeeded then shiftByte else id) i16--}
 {--where--}
  {--isShiftNeeded = addr `mod` 2 == 0--}
  {--i16 = fromMaybe 0 $ S.index (addr / chkSz) mem--}
  {--shiftByte x = x `zshr` 8--}
  {--lsByteOf x = x .&. 255--}

{----NOTE replace all 'fromMaybe's with an error mechanism that will make it easy--}
{----to trace Nothing cases--}
{--rd16 :: I16 -> MainMem -> I16--}
{--rd16 addr (MainMem mem) = fromMaybe 0 $ S.index (addr / chkSz) mem--}

{--wr8 :: I8 -> I16 -> MainMem -> MainMem--}
{--wr8 i8 addr (MainMem mem) = MainMem $ S.adjust adjByte (addr / chkSz) mem--}
 {--where--}
  {--adjByte = (_ .|. i8') <<<--}
    {--(_ .&. if isShiftNeeded then 255 else 65535 - 255)--}
  {--i8' = (if isShiftNeeded then shiftByte else id) i8--}
  {--isShiftNeeded = addr `mod` 2 == 0--}
  {--shiftByte x = x `shl` 8--}

{--wr16 :: I16 -> I16 -> MainMem -> MainMem--}
{--wr16 i16 addr (MainMem mem) = MainMem $ S.replace i16 (addr / chkSz) mem--}

