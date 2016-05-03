module Ops where

import Prelude
import Data.Int.Bits

import Types
import MainMem

--NOTE: consider moving the timings to 2D arrays.
--separate from the opcode's logic

-- Additions
-- =========

--ADD A,R
addRegToA :: GetReg -> Regs -> Regs
addRegToA getReg regs = addXToA (getReg regs) regs

--ADD A,(HL)
addHLMemToA :: Mem -> Regs
addHLMemToA { mainMem, regs } = (addXToA hlMem regs) { m = 2 }
 where hlMem = rd8 (joinRegs h l regs) mainMem

--ADD A,n
addImmToA :: Mem -> Regs
addImmToA { mainMem, regs } =
  (addXToA imm regs) { pc = regs.pc + 1, m = 2 }
 where imm = rd8 regs.pc mainMem

addXToA :: I8 -> Regs -> Regs
addXToA x regs =
  regs {a = sum.res, f = sum.flags, m = 1}
 where sum = addI8s regs.a x

--ADC A,R
addRegCarryToA :: GetReg -> Regs -> Regs
addRegCarryToA getReg regs = addXCarryToA (getReg regs) regs

--ADC A,(HL)
addHLMemCarryToA :: Mem -> Regs
addHLMemCarryToA { mainMem, regs } = (addXCarryToA hlMem regs) { m = 2 }
 where hlMem = rd8 (joinRegs h l regs) mainMem

--ADC A,n
addImmCarryToA :: Mem -> Regs
addImmCarryToA { mainMem, regs } =
  (addXCarryToA imm regs) { pc = regs.pc + 1, m = 2 }
 where imm = rd8 regs.pc mainMem

--NOTE: Test edge case of carry flag of 1 that causes a half-carry,
--Is the half-carry flag set correctly?
addXCarryToA :: I8 -> Regs -> Regs
addXCarryToA x regs =
  regs {a = sum.res, f = f', m = 1}
 where
  --Overriding addI8's half-carry flag because it used regs.a+carry
  --Which is essential for the other flags, but not for this one.
  f' = setHalfCarryFlag x regs.a sum.res sum.flags
  sum = addI8s (regs.a + carry) x
  carry = if regs.f .&. carryFlag /= 0 then 1 else 0

--ADD SP,|n|
addImmToSP :: Mem -> Regs
addImmToSP { regs, mainMem } =
  regs {sp = sp', pc = regs.pc + 1, m = 4}
 where
  sp' = (absI8 imm + regs.sp) .&. 0xFFFF
  imm = rd8 regs.pc mainMem

addI8s :: I8 -> I8 -> { res :: I8, flags :: I8 }
addI8s x1 x2 = { res , flags }
 where
  flags =  testZeroFlag res
       .|. testCarryFlag8 sum
       .|. testHalfCarryFlag x1 x2 res
  res = sum .&. 255
  sum = x1 + x2

--ADD HL,RR
add2RegsToHL :: GetReg -> GetReg -> Regs -> Regs
add2RegsToHL getReg1 getReg2 regs =
  regs { h = split.ms, l = split.ls, f = sum.flags, m = 3 } 
 where
  split = splitI16 sum.res
  sum = addI16s hl joinedRegs regs.f
  joinedRegs = joinRegs getReg1 getReg2 regs
  hl = joinRegs h l regs

--ADD HL,SP
addSPToHL :: Regs -> Regs
addSPToHL regs =
  regs { h = split.ms, l = split.ls, f = sum.flags, m = 3 } 
 where
  split = splitI16 sum.res
  sum = addI16s hl regs.sp regs.f
  hl = joinRegs h l regs

addI16s :: I8 -> I16 -> I16 -> { res :: I16, flags :: I8 }
addI16s oldFlags x1 x2 = { res, flags }
 where
  flags =  setCarryFlag16 sum
        $  oldFlags
       .|. testZeroFlag res
  res = sum .&. 0xFFFF
  sum = x1 + x2

-- Subtractions
-- ============

--SUB A,R
subRegFromA :: GetReg -> Regs -> Regs
subRegFromA getReg regs = subXFromA (getReg regs) regs

--SUB A,(HL)
subHLMemFromA :: Mem -> Regs
subHLMemFromA { mainMem, regs } = (subXFromA hlMem regs) { m = 2 }
 where hlMem = rd8 (joinRegs h l regs) mainMem

--SUB A,n
subImmFromA :: Mem -> Regs
subImmFromA { mainMem, regs } = (subXFromA imm regs) { pc = regs.pc + 1, m = 2 }
 where imm = rd8 regs.pc mainMem

--NOTE: Subtractions and additions are relatively similar. Consider
--Using same functions for both, if it doesn't obfuscate much.
subXFromA :: I8 -> Regs -> Regs
subXFromA x regs =
  regs {a = diff.res, f = diff.flags, m = 1}
 where diff = subI8s regs.a x

--SBC A,R
subRegCarryFromA :: GetReg -> Regs -> Regs
subRegCarryFromA getReg regs = subXCarryFromA (getReg regs) regs

--SBC A,(HL)
subHLMemCarryFromA :: Mem -> Regs
subHLMemCarryFromA { mainMem, regs } = (subXCarryFromA hlMem regs) { m = 2 }
 where hlMem = rd8 (joinRegs h l regs) mainMem

--SBC A,n
subImmCarryToA :: Mem -> Regs
subImmCarryToA { mainMem, regs } = (subXCarryFromA imm regs) { pc = regs.pc + 1, m = 2 }
 where imm = rd8 regs.pc mainMem

--NOTE: Test edge case of carry flag of 1 that causes a half-carry,
--Is the half-carry flag set correctly?
subXCarryFromA :: I8 -> Regs -> Regs
subXCarryFromA x regs =
  regs {a = diff.res, f = f', m = 1}
 where
  f' = setHalfCarryFlag x regs.a diff.res diff.flags
  diff = subI8s (regs.a - carry) x
  carry = if regs.f .&. carryFlag /= 0 then 1 else 0

subI8s :: I8 -> I8 -> { res :: I8, flags :: I8 }
subI8s x1 x2 = { res , flags }
 where
  flags =  testZeroFlag res
       .|. testNegCarryFlag diff
       .|. subtractionFlag
       .|. testHalfCarryFlag x1 x2 res
  res = diff .&. 255
  diff = x1 - x2

-- Boolean operations
-- ==================

--AND A,R
andOpRegIntoA :: GetReg -> Regs -> Regs
andOpRegIntoA = boolOpRegIntoA (.&.)

--OR A,R
orOpRegIntoA :: GetReg -> Regs -> Regs
orOpRegIntoA = boolOpRegIntoA (.|.)

--XOR A,R
xorOpRegIntoA :: GetReg -> Regs -> Regs
xorOpRegIntoA = boolOpRegIntoA (.^.)

boolOpRegIntoA :: (I8 -> I8 -> I8) -> GetReg -> Regs -> Regs
boolOpRegIntoA op getReg regs = boolOpXIntoA op (getReg regs) regs

--AND A,(HL)
andOpHLMemIntoA :: Mem -> Regs
andOpHLMemIntoA = boolOpHlMemIntoA (.&.)

--OR A,(HL)
orOpHLMemIntoA :: Mem -> Regs
orOpHLMemIntoA = boolOpHlMemIntoA (.|.)

--XOR A,(HL)
xorOpHLMemIntoA :: Mem -> Regs
xorOpHLMemIntoA = boolOpHlMemIntoA (.^.)

boolOpHlMemIntoA  :: (I8 -> I8 -> I8) -> Mem -> Regs
boolOpHlMemIntoA op { mainMem, regs } =
  (boolOpXIntoA op hlMem regs) { m = 2 }
 where hlMem = rd8 (joinRegs h l regs) mainMem

--AND A,Imm
andOpImmIntoA :: Mem -> Regs
andOpImmIntoA  = boolOpImmIntoA (.&.)

--OR A,Imm
orOpImmIntoA :: Mem -> Regs
orOpImmIntoA = boolOpImmIntoA (.|.)

--XOR A,Imm
xorOpImmIntoA :: Mem -> Regs
xorOpImmIntoA = boolOpImmIntoA (.^.)

boolOpImmIntoA :: (I8 -> I8 -> I8) -> Mem -> Regs
boolOpImmIntoA op { mainMem, regs } =
  (boolOpXIntoA op imm regs) {pc = regs.pc + 1, m = 2}
 where imm = rd8 regs.pc mainMem

boolOpXIntoA :: (I8 -> I8 -> I8) -> I8 -> Regs -> Regs
boolOpXIntoA op x regs =
  regs { a = a', f = testZeroFlag a', m = 1 }
 where a' = (regs.a `op` x) .&. 255

-- Helpers
-- =======

saveRegs :: Regs -> SavedRegs -> SavedRegs
saveRegs regs svdRegs =
  svdRegs { a = regs.a, b = regs.b, c = regs.c, d = regs.d
          , e = regs.e, f = regs.f, h = regs.h, l = regs.l
          }

restoreRegs :: SavedRegs -> Regs -> Regs
restoreRegs svdRegs regs =
  regs { a = svdRegs.a, b = svdRegs.b, c = svdRegs.c, d = svdRegs.d
       , e = svdRegs.e, f = svdRegs.f, h = svdRegs.h, l = svdRegs.l
       }

joinRegs :: (Regs -> I8) -> (Regs -> I8) -> Regs -> I16
joinRegs msByteReg lsByteReg regs = (msByteReg regs `shl` 8) + lsByteReg regs

absI8 :: I8 -> I8
absI8 i8 = if i8 > 127 then negI8 i8 else i8

negI8 :: I8 -> I8
negI8 i8 = (complement i8 + 1) .&. 255

splitI16 :: I16 -> { ms :: I8, ls :: I8 }
splitI16 x = { ms : (x `zshr` 8) .&. 255
             , ls : x .&. 255
             }

isSetFlag :: I8 -> I8 -> Boolean
isSetFlag f fs = f .&. fs /= 0

setFlag :: I8 -> I8 -> I8
setFlag  f = (f .|. _)

unsetFlag :: I8 -> I8 -> I8
unsetFlag f = ((complement f) .&. _)

flipFlag :: I8 -> I8 -> I8
flipFlag f = (f .^. _)

testZeroFlag :: Int -> Int
testZeroFlag x = if x == 0 then 0x80 else 0

setZeroFlag :: Int -> I8 -> I8
setZeroFlag x = if x == 0
  then (_ .|. zeroFlag)
  else (_ .&. cmplZeroFlag)

testCarryFlag8 :: Int -> I8
testCarryFlag8 x = if x > 255 then carryFlag else 0

setCarryFlag16 :: Int -> I8 -> I8
setCarryFlag16 x = if x > 0xFFFF
  then (_ .|. carryFlag)
  else (_ .&. cmplCarryFlag)

testNegCarryFlag :: Int -> I8
testNegCarryFlag reg = if reg < 0 then carryFlag else 0

testHalfCarryFlag :: I8 -> I8 -> I8 -> I8
testHalfCarryFlag reg1 reg2 sum = if res /= 0 then halfCarryFlag else 0
 where res = 0x10 .&. (reg1 .^. reg2 .^. sum)

setHalfCarryFlag :: I8 -> I8 -> I8 -> I8 -> I8
setHalfCarryFlag reg1 reg2 sum =
  if res /= 0 then (_ .|. halfCarryFlag) else (_ .&. cmplHalfCarryFlag)
 where res = 0x10 .&. (reg1 .^. reg2 .^. sum)

xor :: Boolean -> Boolean -> Boolean
xor true x = not x
xor false x = x

zeroFlag :: I8
zeroFlag = 0x80
cmplZeroFlag :: I8
cmplZeroFlag = 0x7F
carryFlag :: I8
carryFlag = 0x10
cmplCarryFlag :: I8
cmplCarryFlag = 0xEF
halfCarryFlag :: I8
halfCarryFlag = 0x20
cmplHalfCarryFlag :: I8
cmplHalfCarryFlag = 0xD0
subtractionFlag :: I8
subtractionFlag = 0x40

type GetReg = Regs -> I8
type SetReg = I8 -> Regs -> Regs
