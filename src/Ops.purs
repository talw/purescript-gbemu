module Ops where

import Prelude
import Data.Int.Bits

import Types
import MainMem

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
addImmToA { mainMem, regs } = (addXToA imm regs) { pc = regs.pc + 1, m = 2 }
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
addImmCarryToA { mainMem, regs } = (addXCarryToA imm regs) { pc = regs.pc + 1, m = 2 }
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
  carry = if regs.f .&. 0x10 /= 0 then 1 else 0

--ADD SP,|n|
addImmToSP :: Mem -> Regs
addImmToSP { regs, mainMem } =
  regs {sp = sp', pc = regs.pc + 1, m = 4}
 where
  sp' = (immAbs + regs.sp) .&. 0xFFFF
  immAbs = (if imm > 127 then negI8 else id) imm
  imm = rd8 regs.pc mainMem

addI8s :: I8 -> I8 -> { res :: I8, flags :: I8 }
addI8s x1 x2 = { res , flags }
 where
  flags =  zeroFlag res
       .|. carryFlag8 sum
       .|. halfCarryFlag x1 x2 res
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
       .|. zeroFlag res
  res = sum .&. 0xFFFF
  sum = x1 + x2

-- Helpers
-- =======

joinRegs :: (Regs -> I8) -> (Regs -> I8) -> Regs -> I16
joinRegs msByteReg lsByteReg regs = (msByteReg regs `shl` 8) + lsByteReg regs

negI8 :: I8 -> I8
negI8 i8 = (complement i8 + 1) .&. 255

splitI16 :: I16 -> { ms :: I8, ls :: I8 }
splitI16 x = { ms : (x `zshr` 8) .&. 255
             , ls : x .&. 255
             }

zeroFlag :: Int -> Int
zeroFlag x = if x == 0 then 0x80 else 0

carryFlag8 :: Int -> I8
carryFlag8 x = if x > 255 then 0x10 else 0

setCarryFlag16 :: Int -> I8 -> I8
setCarryFlag16 x = if x > 0xFFFF then (_ .|. 0x10) else (_ .&. 0xEF)

halfCarryFlag :: I8 -> I8 -> I8 -> I8
halfCarryFlag reg1 reg2 sum = if res /= 0 then 0x20 else 0
 where res = 0x10 .&. (reg1 .^. reg2 .^. sum)

setHalfCarryFlag :: I8 -> I8 -> I8 -> I8 -> I8
setHalfCarryFlag reg1 reg2 sum =
  if res /= 0 then (_ .|. 0x20) else (_ .&. 0xD0)
 where res = 0x10 .&. (reg1 .^. reg2 .^. sum)

negCarryFlag :: Int -> I8
negCarryFlag reg = if reg < 0 then 0x10 else 0

subtractionFlag :: I8
subtractionFlag = 0x40

type GetReg = Regs -> I8
type SetReg = I8 -> Regs -> Regs
