module Ops where

import Prelude
import Data.Int.Bits
import Data.Maybe
import Data.Array ((!!)) as A

import Types
import MainMem
import Utils

import Debug

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
  (addXToA imm regs) { pc = regs.pc + 2, m = 2 }
 where imm = rd8 (regs.pc+1) mainMem

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
  (addXCarryToA imm regs) { pc = regs.pc + 2, m = 2 }
 where imm = rd8 (regs.pc+1) mainMem

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
  regs {sp = sp', pc = regs.pc + 2, m = 4}
 where
  sp' = (absI8 imm + regs.sp) .&. 0xFFFF
  imm = rd8 (regs.pc+1) mainMem

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
add2RegsToHL msByteReg lsByteReg regs =
  regs { h = split.ms, l = split.ls, f = sum.flags, m = 3 } 
 where
  split = splitI16 sum.res
  sum = addI16s hl joinedRegs regs.f
  joinedRegs = joinRegs msByteReg lsByteReg regs
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
subImmFromA { mainMem, regs } = (subXFromA imm regs) { pc = regs.pc + 2, m = 2 }
 where imm = rd8 (regs.pc+1) mainMem

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
subImmCarryToA { mainMem, regs } = (subXCarryFromA imm regs) { pc = regs.pc + 2, m = 2 }
 where imm = rd8 (regs.pc+1) mainMem

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
  (boolOpXIntoA op imm regs) {pc = regs.pc + 2, m = 2}
 where imm = rd8 (regs.pc+1) mainMem

boolOpXIntoA :: (I8 -> I8 -> I8) -> I8 -> Regs -> Regs
boolOpXIntoA op x regs =
  regs { a = a', f = testZeroFlag a', m = 1 }
 where a' = (regs.a `op` x) .&. 255

-- Bit operations
-- ==============

--CPL
cmplA :: Regs -> Regs
cmplA regs = regs { a = a', f = testZeroFlag a' ,m = 1 }
 where a' = 255 .^. regs.a

--BIT N,R
testBitNOfReg :: I8 -> GetReg -> Regs -> Regs
testBitNOfReg n getReg regs = testBitNOfX n (getReg regs) regs

--BIT N,(HL)
testBitNOfHLMem :: I8 -> Mem -> Regs
testBitNOfHLMem n { mainMem, regs } =
  (testBitNOfX n hlMem regs) { m = 3 }
 where
  hlMem = rd8 (joinRegs h l regs) mainMem

testBitNOfX :: Int -> I8 -> Regs -> Regs
testBitNOfX n x regs = regs { f = f', m = 2 }
 where
  f' =  setZeroFlag bitTest
     $  halfCarryFlag
    .|. (regs.f .&. carryFlag) -- NOTE: should it be 0x1F instead?
  bitTest = x .&. (1 `shl` n)

--SET N,R
--RES N,R
setBitNOfReg :: Boolean -> Int -> SetReg -> GetReg
               -> Regs -> Regs
setBitNOfReg setVal n setReg getReg regs = setReg reg' regs { m = 2 }
 where reg' = setBitNOfX setVal n (getReg regs)

--SET N,(HL)
--RES N,(HL)
setBitNOfHLMem :: Boolean -> Int -> Mem -> Mem
setBitNOfHLMem setVal n mem@{mainMem,regs} =
  mem { mainMem = mainMem', regs = regs { m = 4 } }
 where
  mainMem' = wr8 hl' addr mainMem
  hl' = setBitNOfX setVal n $ rd8 addr mainMem
  addr = joinRegs h l regs

setBitNOfX :: Boolean -> Int -> I8 -> I8
setBitNOfX setVal n x = if setVal
  then x .|. bitShift
  else x .&. complement bitShift
 where bitShift = 1 `shl` n

-- Rotations
-- =========

data Dir = Left | Right
derive instance eqDir :: Eq Dir

-- RL[C] A, base-opcode
-- RR[C] A, base-opcode
rotA :: Dir -> Boolean -> Regs -> Regs
rotA dir isCarryRot regs = regs { a = rotated.res, f = rotated.flags, m = 1 }
 where rotated = rotX {dir,isCarryRot,isCB:false} regs.a regs.f

-- RL[C] R, CB-prefix instruction
-- RR[C] R, CB-prefix instruction
rotReg :: Dir -> Boolean -> SetReg -> GetReg -> Regs -> Regs
rotReg dir isCarryRot setReg getReg regs =
  setReg rotated.res regs { f = rotated.flags, m = 2 }
 where rotated = rotX {dir,isCarryRot,isCB:true} (getReg regs) regs.f

-- RL[C] (HL)
-- RR[C] (HL)
rotHLMem :: Dir -> Boolean -> Mem -> Mem
rotHLMem dir isCarryRot mem@{mainMem,regs} =
  mem { mainMem = mainMem', regs = regs { f = rotated.flags, m = 4 } }
 where
  mainMem' = wr8 rotated.res addr mainMem
  addr = joinRegs h l regs
  rotated = rotX {dir,isCarryRot,isCB:true} (rd8 addr mainMem) regs.f

rotX :: {dir::Dir,isCarryRot::Boolean,isCB::Boolean}
      -> I8 -> I8 -> { res :: I8, flags :: I8 }
rotX { dir, isCarryRot, isCB } x oldFlags = { res, flags }
 where
  res = 255 .&. (currCarry + (x `shiftFunc` 1))
  -- RotA basic, use old flags, RotR CB, start fresh with zeroFlag
  flags = setNewCarry
    $ if isCB then testZeroFlag res else oldFlags
  currCarry = if   (not isCarryRot) && (oldFlags .&. carryFlag /= 0)
                || (isCarryRot      && isNewCarry)
    then case dir of
      Left -> 1
      Right -> 0x80 
    else 0
  setNewCarry = if isNewCarry
    then (carryFlag .|. _)
    else (cmplCarryFlag .&. _)
  isNewCarry = x .&. edgeBit /= 0
  edgeBit = case dir of
    Left -> 0x80
    Right -> 1
  shiftFunc = case dir of
    Left -> shl
    Right -> zshr

-- Shifts
-- ======

--SLA R
--SRA R preserve sign
--SRL R
shiftReg :: Dir -> Boolean -> SetReg -> GetReg
         -> Regs -> Regs
shiftReg dir sign setReg getReg regs =
  setReg shifted.res regs { f = shifted.flags, m = 2 }
 where
  shifted = shiftX dir sign $ getReg regs

shiftMemHL :: Dir -> Boolean -> Mem -> Mem
shiftMemHL dir sign mem@{mainMem,regs} =
  mem { mainMem = mainMem', regs = regs { f = shifted.flags, m = 4 } }
 where
  mainMem' = wr8 shifted.res addr mainMem
  shifted = shiftX dir sign $ rd8 addr mainMem
  addr = joinRegs h l regs

shiftX :: Dir -> Boolean -> I8
       -> { res::I8, flags::I8 }
shiftX dir sign x = { res, flags }
 where
  res =  (255 .&. _)
     <<< (if sign && dir == Right then (currCarry + _) else id)
      $  x `shiftFunc` 1
  shiftFunc = case dir of
    Left -> shl
    Right -> zshr
  flags = newCarry .|. testZeroFlag res
  newCarry = if x .&. edgeBit /= 0 then carryFlag else 0
  currCarry = x .&. 0x80 --sign is only relevant for right shifts
  edgeBit = case dir of
    Left -> 0x80
    Right -> 1

-- Increments / Decrements
-- =======================

--INC R
incReg :: GetReg -> SetReg -> Regs -> Regs
incReg = incDecReg incI8

--DEC R
decReg :: GetReg -> SetReg -> Regs -> Regs
decReg getReg setReg regs = regs' { f = setFlag subtractionFlag regs'.f }
  where regs' = (incDecReg decI8 getReg setReg regs)
  

incDecReg :: (I8 -> { res :: I8, carry :: Boolean })
          -> GetReg -> SetReg -> Regs -> Regs
incDecReg op getReg setReg regs =
  setReg inced .res regs {f = testZeroFlag inced.res, m = 1}
 where
  inced  = op $ getReg regs

--INC RR
incRegWithCarry :: GetReg -> GetReg -> SetReg -> SetReg
                -> Regs -> Regs
incRegWithCarry = incDecRegWithCarry incI8

--DEC RR
decRegWithCarry :: GetReg -> GetReg -> SetReg -> SetReg
                -> Regs -> Regs
decRegWithCarry = incDecRegWithCarry decI8

--NOTE: consider refactoring to something along the lines of
--"split2Regs . incI16 . joinRegs", if you have leeway, performance-wise.
incDecRegWithCarry :: (I8 -> { res :: I8, carry :: Boolean })
          -> GetReg -> GetReg -> SetReg -> SetReg
          -> Regs -> Regs
incDecRegWithCarry op getCarryReg getMainReg setCarryReg setMainReg regs = 
  setCarryReg carryReg' <<< setMainReg mainReg' $ regs{m = 1}
 where
  carryReg' = (if mainRegInc.carry then _.res <<< op else id)
    $ getCarryReg regs
  mainReg' = mainRegInc.res
  mainRegInc = op $ getMainReg regs

--INC SP
incSP :: Regs -> Regs
incSP = incDecSP $ \x -> 0xFFFF .&. (x + 1)

--DEC SP
decSP :: Regs -> Regs
decSP = incDecSP $ \x -> 0xFFFF .&. (x - 1)

incDecSP :: (I16 -> I16) -> Regs -> Regs
incDecSP op regs = regs {sp = op $ regs.sp, m = 1}

--INC (HL)
incHLMem :: Mem -> Mem
incHLMem = incDecHLMem incI8

--DEC (HL)
decHLMem :: Mem -> Mem
decHLMem = incDecHLMem decI8

incDecHLMem :: (I8 -> { res :: I8, carry :: Boolean })
            -> Mem -> Mem
incDecHLMem op mem@{mainMem,regs} =
  mem { mainMem = mainMem', regs = regs {f = testZeroFlag inced.res, m = 3} }
 where
  mainMem' = wr8 inced.res addr mainMem
  inced = op $ rd8 addr mainMem
  addr = joinRegs h l regs

incI8 :: I8 -> { res :: I8, carry :: Boolean }
incI8 i8 = { res : i8', carry : i8' == 0 }
 where
  i8' = (i8 + 1) .&. 255

decI8 :: I8 -> { res :: I8, carry :: Boolean }
decI8 i8 = { res : i8', carry : i8' == 255 }
 where
  i8' = (i8 - 1) .&. 255

-- Loads
-- =====

--LD R,R
ldRegFromReg :: SetReg -> GetReg
      -> Regs -> Regs
ldRegFromReg setDestReg getSrcReg regs = setDestReg (getSrcReg regs)
  regs { m = 1 }

--LD (nn),SP
ldMemImmFromSP :: Mem -> Mem
ldMemImmFromSP mem@{mainMem, regs} = 
  mem { mainMem = mainMem', regs = regs { pc = regs.pc + 3, m = 5 } }
 where
  mainMem' = wr16 regs.sp addr mainMem
  addr = rd16 (regs.pc+1) mainMem

--LD SP,HL
ldSPFromHL :: Regs -> Regs
ldSPFromHL regs  = 
  regs { sp = sp', m = 2 }
 where sp' = joinRegs h l regs
  
ldSPFromImm :: Mem -> Regs
ldSPFromImm { mainMem, regs } = 
  regs { sp = sp', pc = regs.pc + 3, m = 3 }
 where sp' = rd16 (regs.pc+1) mainMem
  
--LD RR,nn
ldTwoRegsFromImm :: SetReg -> SetReg
               -> Mem -> Regs
ldTwoRegsFromImm setReg1 setReg2 mem = regs'' { m = 3 }
 where
  regs'  = ldRegFromImm setReg2 mem
  regs'' = ldRegFromImm setReg1 mem { regs = regs' }

--LD R,(IOC)
ldRegFromFF00CMem :: SetReg -> Mem -> Regs
ldRegFromFF00CMem setReg mem@{regs, mainMem} = regs' { m = 2 }
 where regs' = ldRegFromFF00PlusX regs.c setReg mem

--LD R,(IOn)
ldRegFromFF00ImmMem :: SetReg -> Mem -> Regs
ldRegFromFF00ImmMem setReg mem@{regs, mainMem} = regs' { pc = regs.pc + 2, m = 3 }
 where regs' = ldRegFromFF00PlusX (rd8 (regs.pc+1) mainMem) setReg mem

ldRegFromFF00PlusX :: I8 -> SetReg -> Mem -> Regs
ldRegFromFF00PlusX  x setReg mem@{regs} = ldRegFromMem setReg addr mem
 where addr = 0xFF00 + x

--LD R,n
ldRegFromImm :: SetReg -> Mem -> Regs
ldRegFromImm setReg mem@{regs} = regs' { pc = regs.pc + 2 }
  where regs' = ldRegFromMem setReg (regs.pc+1) mem

--LD R,(RR)
ldRegFromMem2R :: SetReg -> GetReg -> GetReg
               -> Mem -> Regs
ldRegFromMem2R setReg msByteReg lsByteReg mem@{regs} =
  ldRegFromMem setReg ((msByteReg regs `shl` 8) + lsByteReg regs) mem

--LD A,(nn)
ldRegAFromMemImm :: Mem -> Regs
ldRegAFromMemImm mem@{ mainMem, regs } = regs' { pc = regs.pc + 3, m = 4 }
 where
  regs' = ldRegFromMem setA addr mem
  addr = rd16 (regs.pc+1) mainMem

--LDD A,(HL)
ldRegFromMemHLDec :: SetReg -> Mem -> Regs
ldRegFromMemHLDec = ldRegFromMemHLIncDec decRegWithCarry

--LDI A,(HL)
ldRegFromMemHLInc :: SetReg -> Mem -> Regs
ldRegFromMemHLInc = ldRegFromMemHLIncDec incRegWithCarry

ldRegFromMemHLIncDec :: (GetReg -> GetReg -> SetReg -> SetReg -> Regs -> Regs)
                   -- Inc/Dec reg operation's type signature
                   -> SetReg -> Mem -> Regs
ldRegFromMemHLIncDec op setReg mem@{ regs } = regs' {m = 2}
 where
  regs' = op h l setH setL
        $ ldRegFromMem setA addr mem
  addr = joinRegs h l regs

ldRegFromMem :: SetReg -> I16 -> Mem -> Regs
ldRegFromMem setReg addr { mainMem, regs } =
  setReg (rd8 addr mainMem) regs{m = 2}

--LD HL,SP|n|
ldHLFromSPImm :: Mem -> Regs
ldHLFromSPImm  mem@{ regs, mainMem } =
  regs {h = split.ms, l = split.ls, pc = regs.pc + 2, m = 3}
 where
  split = splitI16 x
  x = (absI8 imm + regs.sp) .&. 0xFFFF
  imm = rd8 (regs.pc+1) mainMem

--LDI (HL),A
ldMemHLFromRegInc :: GetReg -> Mem -> Mem
ldMemHLFromRegInc = ldMemHLFromRegIncDec incRegWithCarry

--LDD (HL),A
ldMemHLFromRegDec :: GetReg -> Mem -> Mem
ldMemHLFromRegDec  = ldMemHLFromRegIncDec decRegWithCarry

ldMemHLFromRegIncDec :: (GetReg -> GetReg -> SetReg -> SetReg -> Regs -> Regs)
                   -- Inc/Dec reg operation's type signature
                   -> GetReg
                   -> Mem -> Mem
ldMemHLFromRegIncDec op getReg mem@{regs} =
  mem { mainMem = mainMem', regs = regs' {m = 2} }
 where
  regs' = op h l setH setL regs
  mainMem' = _.mainMem $ ldMemFromReg addr a mem
  addr = joinRegs h l regs

--LD (IOC),R
ldFF00CMemFromReg :: GetReg -> Mem -> Mem
ldFF00CMemFromReg getReg mem@{regs, mainMem} =
  mem' { regs = regs{m = 2} }
 where mem' = ldFF00PlusXFromReg  regs.c getReg mem

--LD (IOn),R
ldFF00ImmMemFromReg :: GetReg -> Mem -> Mem
ldFF00ImmMemFromReg getReg mem@{regs, mainMem} =
  mem' { regs = regs{pc = regs.pc + 2, m = 3} }
 where
  mem' = ldFF00PlusXFromReg imm getReg mem
  imm = rd8 (regs.pc+1) mainMem

ldFF00PlusXFromReg :: I8 -> GetReg -> Mem -> Mem
ldFF00PlusXFromReg   x getReg mem@{regs, mainMem} = mem'
 where
  mem' = ldMemFromReg addr getReg mem
  addr = 0xFF00 + x

--LD (nn),A
ldMemImmFromRegA :: Mem -> Mem
ldMemImmFromRegA  mem@{ mainMem, regs } =
 mem' { regs = regs{m = 4, pc = regs.pc + 3} }
 where
  mem' = ldMemFromReg addr a mem
  addr = rd16 (regs.pc+1) mainMem

--LD (HL),n
ldMemHLFromImm :: Mem -> Mem
ldMemHLFromImm  mem@{mainMem,regs} =
  mem { mainMem = mainMem' , regs = regs{m = 3, pc = regs.pc + 2} }
 where
  mainMem' = wr8 imm addr mainMem
  addr =  joinRegs h l regs
  imm = rd8 (regs.pc+1) mainMem

--LD (RR),R
ldMem2RFromReg :: GetReg -> GetReg -> GetReg
                        -> Mem -> Mem
ldMem2RFromReg msbAddrReg lsbAddrReg getReg mem@{ regs } =
  ldMemFromReg (joinRegs msbAddrReg lsbAddrReg regs) getReg mem

ldMemFromReg :: I16 -> GetReg
           -> Mem -> Mem
ldMemFromReg addr getReg mem@{ regs } = ldMemFromX addr (getReg regs) mem

ldMemFromX :: I16 -> I8
           -> Mem -> Mem
ldMemFromX addr x mem@{mainMem,regs} =
  mem { mainMem = mainMem', regs = regs{m = 2} }
 where mainMem' = wr8 x addr mainMem

-- Compares
-- ========

--CP A,R
compAToReg :: GetReg -> Regs -> Regs
compAToReg getReg regs = compAToX (getReg regs) regs

--CP A,(HL)
compAToMemHL :: Mem -> Regs
compAToMemHL { mainMem, regs } = (compAToX hlMem regs) { m = 2 }
 where hlMem = rd8 (joinRegs h l regs) mainMem

--CP A,n
compAToImm :: Mem -> Regs
compAToImm { mainMem, regs } = (compAToX imm regs) { pc = regs.pc + 2, m = 2 }
 where imm = rd8 (regs.pc+1) mainMem

compAToX :: I8 -> Regs -> Regs
compAToX x regs = regs { f = diff.flags, m = 1 }
 where
  diff = subI8s regs.a x

-- Stack operations
-- ================

--NOTE: On the one hand, there's an advantage of letting the compiler catch
--more mistakes by using row polymorphism in this case,
--on the other hand it obfuscates the signature, is there a better way?
--forall r s. { regs :: {sp::I8 | s}, mem :: MainMem | r }
--         -> { regs :: {sp::I8 | s}, mem :: MainMem | r }
--NOTE: Only 4 versions of PUSH so consider hard-coding them for performance.
--PUSH RR
pushReg :: GetReg -> GetReg
        -> Mem -> Mem
pushReg msByteReg lsByteReg mem@{regs,mainMem} =
  mem { mainMem = mainMem', regs = regs { sp = regs.sp - 2, m = 3 } }
 where
  mainMem' =  wr8 (lsByteReg regs) (regs.sp - 2)
          <<< wr8 (msByteReg regs) (regs.sp - 1)
           $  mainMem

--POP RR
popReg :: SetReg -> SetReg
       -> Mem -> Regs
popReg setMsByteReg setLsByteReg { mainMem, regs = regs@{sp} }
    = setMsByteReg (rd8 (sp + 1) mainMem)
   <<< setLsByteReg (rd8 sp mainMem)
    $  regs { sp = sp + 2, m = 3 }

-- Jumps
-- =====

-- JR NZ,n
-- JR Z,n
-- JR NC,n
-- JR C,n
jumpRelImmFlag :: Boolean -> I8 -> Mem -> Regs
jumpRelImmFlag inverse flag mem@{mainMem, regs} =
  if inverse `xor` isSetFlag flag regs.f
    then jumpRelImm mem
    else regs { pc = regs.pc + 2, m = 2 }

{----JR n--}
{--jumpRelImm :: Mem -> Regs--}
{--jumpRelImm { mainMem, regs } =--}
  {--regs { pc = pc', m = 3 }--}
 {--where--}
  {----NOTE: is it necessary to abs the immediate?--}
  {--pc' = regs.pc + 1 + absI8 imm--}
  {--imm = rd8 regs.pc mainMem--}

--JR n
jumpRelImm :: Mem -> Regs
jumpRelImm { mainMem, regs } =
  regs { pc = pc', m = 3 }
 where
  --NOTE: is it necessary to abs the immediate?
  pc' = regs.pc + 2 + absI8 imm
  imm = rd8 (regs.pc+1) mainMem

-- JP NZ,nn
-- JP Z,nn
-- JP NC,nn
-- JP C,nn
jumpImmFlag :: Boolean -> I8 -> Mem -> Regs
jumpImmFlag inverse flag mem@{mainMem,regs} =
  if inverse `xor` isSetFlag flag regs.f
    then (jumpImm mem) { m = 4 }
    else regs { pc = regs.pc + 3, m = 3 }

-- JP nn
jumpImm :: Mem -> Regs
jumpImm { mainMem, regs } =
  regs { pc = pc', m = 3 }
 where pc' = rd16 (regs.pc+1) mainMem

-- JP (HL)
jumpHL :: Regs -> Regs
jumpHL regs = regs { pc = pc', m = 1 }
 where pc' = joinRegs h l regs

{--jumpX :: I8 -> Regs--}
{--jumpX { mainMem, regs } =--}
  {--regs { pc = pc', m = 3 }--}
 {--where pc' = rd16 regs.pc mainMem--}

-- Call Z,nn
-- Call NZ,nn
-- Call C,nn
-- Call NC,nn
callImmFlag :: Boolean -> I8 -> Mem -> Mem
callImmFlag inverse flag mem@{mainMem,regs} = 
  if inverse `xor` isSetFlag flag regs.f
    then callImm mem
    else mem { regs = regs {pc = regs.pc + 3, m = 3} }

-- Call nn
callImm :: Mem -> Mem
callImm mem@{mainMem,regs} =
  mem { mainMem =  mainMem', regs = regs { pc = imm, sp = regs.sp - 2, m = 5 } }
 where
  mainMem' = wr16 (regs.pc + 3) (regs.sp-2) $ mainMem
  imm = rd16 (regs.pc+1) mainMem

-- RET Z
-- RET NZ
-- RET C
-- RET NC
retFlag :: Boolean -> I8 -> Mem -> Regs
retFlag inverse flag mem@{mainMem,regs} = 
  if inverse `xor` isSetFlag flag regs.f
    then ret mem
    else regs { m = 1 }

-- RETI
retEnableInterrupt :: Mem -> Regs
retEnableInterrupt { mainMem, regs, svdRegs } =
  restoreRegs svdRegs regs { ime = true, pc = pc', sp = regs.sp + 2, m = 3 }
 where pc' = rd16 regs.sp mainMem

-- RET
ret :: Mem -> Regs
ret { mainMem, regs } =
  regs { pc = pc', sp = regs.sp + 2, m = 3 }
 where pc' = rd16 regs.sp mainMem
  
--RST XX
callRoutine :: I16 -> Mem -> Mem
callRoutine addr { mainMem, regs, svdRegs } =
  { mainMem : mainMem', regs : regs', svdRegs : saveRegs regs svdRegs }
 where
  regs' = regs { sp = regs.sp - 2, pc = addr, m = 3 }
  mainMem' = wr16 regs.pc (regs.sp - 2) mainMem

-- Misc.
-- =====

--SWAP R
swapReg :: SetReg -> GetReg -> Regs -> Regs
swapReg setReg getReg regs = setReg reg' $ regs { m = 1 }
 where
  reg' = ((0x0F.&.reg) `shl` 4) .|. ((0xF0.&.reg) `zshr` 4)
  reg = getReg regs

--SWAP (HL)
swapMemHL :: Mem -> Mem
swapMemHL mem@{mainMem, regs} =
  mem { mainMem = mainMem', regs = regs { m = 4 } }
 where
  mainMem' = wr8 hlMem' addr mainMem
  hlMem' = ((0x0F.&.hlMem) `shl` 4) .|. ((0xF0.&.hlMem) `zshr` 4)
  hlMem = rd8 addr mainMem
  addr = joinRegs h l regs

--Why a command called 'clear' actually flips the tag
--and not unset it, is a question that shouldn't be directed at me!
--CCF
clearCarryFlag :: Regs -> Regs
clearCarryFlag = changeCarryFlag false

--SCF
setCarryFlag :: Regs -> Regs
setCarryFlag = changeCarryFlag true

changeCarryFlag :: Boolean -> Regs -> Regs
changeCarryFlag isSet regs = regs { f = f', m = 1 }
 where f' =  unsetFlag subtractionFlag
         <<< unsetFlag halfCarryFlag
         <<< (if isSet then setFlag else flipFlag) carryFlag
          $  regs.f

--DAA
adjAForBCDAdd :: Regs -> Regs
adjAForBCDAdd regs = regs { f = adjF regs.f, a = a', m = 1 }
 where
  a' = adjHighNyb <<< adjLowNyb $ regs.a
  adjLowNyb = if (regs.f .&. halfCarryFlag /= 0) || (regs.a .&. 15 > 9)
    then (6 + _) else id
  adjHighNyb = if shouldAdjHighNyb then (0x60 + _) else id
  adjF = if shouldAdjHighNyb then (carryFlag .|. _) else (cmplCarryFlag .&. _)
  shouldAdjHighNyb = (regs.f .&. halfCarryFlag /= 0) || (regs.a > 0x99)

--NOP
nop :: Regs -> Regs
nop = _ { m = 1}

--Invalid opCode
invalidOpCode :: Z80State -> Z80State
--NOTE: LOG THIS! for debugging purposes
invalidOpCode state = state { stop = true }

--STOP 
--NOTE: this is just a guess
stop :: Z80State -> Z80State
stop state@{mem=mem@{regs}} =
  state { stop = true
        , mem = mem { regs = regs { m = 1 } }
        }

--HALT
halt :: Z80State -> Z80State
halt state@{mem=mem@{regs}} =
  state { halt = true
        , mem = mem { regs = regs { m = 1 } }
        }

--DI
--EI
setInterrupts :: Boolean -> Regs -> Regs
setInterrupts enable = _ { ime = enable, m = 1 }

--Extended Ops
execExtOps :: Array (Z80State -> Z80State)
           -> Z80State -> Z80State
execExtOps opsMap state@{ mem = mem@{mainMem,regs} } = extOp state'
 where
  --NOTE replace toMaybe, with something to log errors
  extOp = getOpcode opCode opsMap 
  state' = state { mem = mem { regs = regs { pc = 65535 .&. (regs.pc + 1) } } }
  opCode = rd8 regs.pc mainMem

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

joinRegs :: GetReg -> GetReg -> Regs -> I16
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

--NOTE: replace fromMaybe with an error report mechanism to trace bad cases
getOpcode :: Int -> Array (Z80State -> Z80State) -> Z80State -> Z80State
getOpcode ix table = fromMaybe id $ table A.!! ix

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
