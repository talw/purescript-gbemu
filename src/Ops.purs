module Ops where

import Prelude (class Eq, not, negate, ($), return, (/=), (<), (>), (==)
               ,(+), bind, id, (||), (-), (<<<), (<$>), (&&))
import Data.Int.Bits ((.^.), (.&.), (.|.), complement, zshr, shl)
import Data.Maybe (fromMaybe)
import Data.Array ((!!)) as A
import Control.Monad.Eff (Eff)
import Control.Bind ((<=<))

import Types (I8, Z80State, MemAccess, I16, Regs, GetReg, SavedRegs, Mem
             ,SetReg, l, h, adjRegs, a, setL, setH, setA)
import MainMem (rd8, setIme, setImeCntDwn, wr8, wr16, rd16)
import Utils (cmp2)

-- Additions
-- =========

--ADD A,R
addRegToA :: GetReg -> Regs -> Regs
addRegToA getReg regs = addXToA (getReg regs) regs

--ADD A,(HL)
addHLMemToA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
addHLMemToA { mainMem, regs } = do
  hlMem <- rd8 (joinRegs h l regs) mainMem
  return $ addXToA hlMem regs

--ADD A,n
addImmToA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
addImmToA { mainMem, regs } = do
  imm <- rd8 (regs.pc+1) mainMem
  return (addXToA imm regs) { pc = regs.pc + 2 }

addXToA :: I8 -> Regs -> Regs
addXToA x regs =
  regs {a = sum.res, f = sum.flags }
 where sum = addI8s regs.a x

--ADC A,R
addRegCarryToA :: GetReg -> Regs -> Regs
addRegCarryToA getReg regs = addXCarryToA (getReg regs) regs

--ADC A,(HL)
addHLMemCarryToA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
addHLMemCarryToA { mainMem, regs } = do
  hlMem <- rd8 (joinRegs h l regs) mainMem
  return $ addXCarryToA hlMem regs 

--ADC A,n
addImmCarryToA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
addImmCarryToA { mainMem, regs } = do
  imm <- rd8 (regs.pc+1) mainMem
  return (addXCarryToA imm regs) { pc = regs.pc + 2 }

--NOTE: Test edge case of carry flag of 1 that causes a half-carry,
--Is the half-carry flag set correctly?
addXCarryToA :: I8 -> Regs -> Regs
addXCarryToA x regs =
  regs {a = sum.res, f = f'}
 where
  --Overriding addI8's half-carry flag because it used regs.a+carry
  --Which is essential for the other flags, but not for this one.
  f' = setHalfCarryFlag8 x regs.a sum.res sum.flags
  sum = addI8s (regs.a + carry) x
  carry = if regs.f .&. carryFlag /= 0 then 1 else 0

--ADD SP,|n|
addImmToSP :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
addImmToSP { regs, mainMem } = do
  immAbs <- absI8 <$> rd8 (regs.pc+1) mainMem
  let sp' = (immAbs + regs.sp) .&. 0xFFFF
      f' =  testHalfCarryFlag16 immAbs regs.sp sp'
        .|. testHalfCarryFlag8 immAbs regs.sp sp'
  return regs {sp = sp', pc = regs.pc + 2, f = f'}

addI8s :: I8 -> I8 -> { res :: I8, flags :: I8 }
addI8s x1 x2 = { res , flags }
 where
  flags =  testZeroFlag res
       .|. testCarryFlag8 sum
       .|. testHalfCarryFlag8 x1 x2 res
  res = sum .&. 255
  sum = x1 + x2

--ADD HL,RR
add2RegsToHL :: GetReg -> GetReg -> Regs -> Regs
add2RegsToHL msByteReg lsByteReg regs =
  regs { h = split.ms, l = split.ls, f = sum.flags } 
 where
  split = splitI16 sum.res
  sum = addI16s hl joinedRegs regs.f
  joinedRegs = joinRegs msByteReg lsByteReg regs
  hl = joinRegs h l regs

--ADD HL,SP
addSPToHL :: Regs -> Regs
addSPToHL regs =
  regs { h = split.ms, l = split.ls, f = sum.flags } 
 where
  split = splitI16 sum.res
  sum = addI16s hl regs.sp regs.f
  hl = joinRegs h l regs

addI16s :: I8 -> I16 -> I16 -> { res :: I16, flags :: I8 }
addI16s x1 x2 oldFlags = { res, flags }
 where
  flags =  testCarryFlag16 sum
       .|. testWeirdHalfCarryFlag16 x1 x2 res
       .|. zeroFlag .&. oldFlags
  res = sum .&. 0xFFFF
  sum = x1 + x2

-- Subtractions
-- ============

--SUB A,R
subRegFromA :: GetReg -> Regs -> Regs
subRegFromA getReg regs = subXFromA (getReg regs) regs

--SUB A,(HL)
subHLMemFromA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
subHLMemFromA { mainMem, regs } = do
  hlMem <- rd8 (joinRegs h l regs) mainMem
  return $ subXFromA hlMem regs

--SUB A,n
subImmFromA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
subImmFromA { mainMem, regs } = do
  imm <- rd8 (regs.pc+1) mainMem
  return (subXFromA imm regs) { pc = regs.pc + 2 }

--NOTE: Subtractions and additions are relatively similar. Consider
--Using same functions for both,
--if it doesn't obfuscate much nor degrade performance.
subXFromA :: I8 -> Regs -> Regs
subXFromA x regs =
  regs {a = diff.res, f = diff.flags}
 where diff = subI8s regs.a x

--SBC A,R
subRegCarryFromA :: GetReg -> Regs -> Regs
subRegCarryFromA getReg regs = subXCarryFromA (getReg regs) regs

--SBC A,(HL)
subHLMemCarryFromA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
subHLMemCarryFromA { mainMem, regs } =
  (\mhl -> subXCarryFromA mhl regs)
    <$> rd8 (joinRegs h l regs) mainMem

--SBC A,n
subImmCarryToA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
subImmCarryToA { mainMem, regs } = do
  imm <- rd8 (regs.pc+1) mainMem
  return (subXCarryFromA imm regs) { pc = regs.pc + 2 }

--NOTE: Test edge case of carry flag of 1 that causes a half-carry,
--Is the half-carry flag set correctly?
subXCarryFromA :: I8 -> Regs -> Regs
subXCarryFromA x regs =
  regs {a = diff.res, f = f'}
 where
  f' = setHalfCarryFlag8 x regs.a diff.res diff.flags
  diff = subI8s (regs.a - carry) x
  carry = if regs.f .&. carryFlag /= 0 then 1 else 0

subI8s :: I8 -> I8 -> { res :: I8, flags :: I8 }
subI8s x1 x2 = { res , flags }
 where
  flags =  testZeroFlag res
       .|. testNegCarryFlag diff
       .|. subtractionFlag
       .|. testHalfCarryFlag8 x1 x2 res
  res = diff .&. 255
  diff = x1 - x2

-- Boolean operations
-- ==================

--AND A,R
andOpRegIntoA :: GetReg -> Regs -> Regs
andOpRegIntoA = adjFlag halfCarryFlag `cmp2` boolOpRegIntoA (.&.)

--OR A,R
orOpRegIntoA :: GetReg -> Regs -> Regs
orOpRegIntoA = boolOpRegIntoA (.|.)

--XOR A,R
xorOpRegIntoA :: GetReg -> Regs -> Regs
xorOpRegIntoA = boolOpRegIntoA (.^.)

boolOpRegIntoA :: (I8 -> I8 -> I8) -> GetReg -> Regs -> Regs
boolOpRegIntoA op getReg regs = boolOpXIntoA op (getReg regs) regs

--AND A,(HL)
andOpHLMemIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
andOpHLMemIntoA = (adjFlag halfCarryFlag <$> _)
               <<< boolOpHlMemIntoA (.&.)

--OR A,(HL)
orOpHLMemIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
orOpHLMemIntoA = boolOpHlMemIntoA (.|.)

--XOR A,(HL)
xorOpHLMemIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
xorOpHLMemIntoA = boolOpHlMemIntoA (.^.)

boolOpHlMemIntoA  :: forall e. (I8 -> I8 -> I8) -> Mem -> Eff (ma :: MemAccess | e) Regs
boolOpHlMemIntoA op { mainMem, regs } = do
  hlMem <- rd8 (joinRegs h l regs) mainMem
  return $ boolOpXIntoA op hlMem regs

--AND A,Imm
andOpImmIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
andOpImmIntoA = (adjFlag halfCarryFlag <$> _)
             <<< boolOpImmIntoA (.&.)

--OR A,Imm
orOpImmIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
orOpImmIntoA = boolOpImmIntoA (.|.)

--XOR A,Imm
xorOpImmIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
xorOpImmIntoA = boolOpImmIntoA (.^.)

boolOpImmIntoA :: forall e. (I8 -> I8 -> I8) -> Mem -> Eff (ma :: MemAccess | e) Regs
boolOpImmIntoA op { mainMem, regs } = do
  imm <- rd8 (regs.pc+1) mainMem
  return (boolOpXIntoA op imm regs) {pc = regs.pc + 2}

boolOpXIntoA :: (I8 -> I8 -> I8) -> I8 -> Regs -> Regs
boolOpXIntoA op x regs =
  regs { a = a', f = testZeroFlag a' }
 where a' = (regs.a `op` x) .&. 255

-- Bit operations
-- ==============

--CPL
cmplA :: Regs -> Regs
cmplA regs = regs { a = a', f = f' }
 where
   a' = 255 .^. regs.a
   f' =  setFlag halfCarryFlag
     <<< setFlag subtractionFlag
      $  regs.f

--BIT N,R
testBitNOfReg :: I8 -> GetReg -> Regs -> Regs
testBitNOfReg n getReg regs = testBitNOfX n (getReg regs) regs

--BIT N,(HL)
testBitNOfHLMem :: forall e. I8 -> Mem -> Eff (ma :: MemAccess | e) Regs
testBitNOfHLMem n { mainMem, regs } = do
  hlMem <- rd8 (joinRegs h l regs) mainMem
  return $ testBitNOfX n hlMem regs

testBitNOfX :: Int -> I8 -> Regs -> Regs
testBitNOfX n x regs = regs { f = f' }
 where
  f' =  setZeroFlag bitTest
     $  unsetFlag subtractionFlag
     $  halfCarryFlag
    .|. (regs.f .&. carryFlag) -- NOTE: should it be 0x1F instead?
  bitTest = x .&. (1 `shl` n)

--SET N,R
--RES N,R
setBitNOfReg :: Boolean -> Int -> SetReg -> GetReg
               -> Regs -> Regs
setBitNOfReg setVal n setReg getReg regs = setReg reg' regs
 where reg' = setBitNOfX setVal n (getReg regs)

--SET N,(HL)
--RES N,(HL)
setBitNOfHLMem :: forall e. Boolean -> Int -> Mem -> Eff (ma :: MemAccess | e) Mem
setBitNOfHLMem setVal n mem@{mainMem,regs} = do
  hl' <- setBitNOfX setVal n <$> rd8 addr mainMem
  mainMem' <- wr8 hl' addr mainMem
  return mem { mainMem = mainMem' }
 where
  addr = joinRegs h l regs

setBitNOfX :: Boolean -> Int -> I8 -> I8
setBitNOfX setVal n x = if setVal
  then x .|. bitShift
  else x .&. complement bitShift
 where bitShift = 1 `shl` n

-- Rotations
-- =========

data Dir = LeftD | RightD
derive instance eqDir :: Eq Dir

-- RL[C] A, base-opcode
-- RR[C] A, base-opcode
rotA :: Dir -> Boolean -> Regs -> Regs
rotA dir isCarryRot regs = regs { a = rotated.res, f = rotated.flags }
 where rotated = rotX {dir,isCarryRot,isCB:false} regs.a regs.f

-- RL[C] R, CB-prefix instruction
-- RR[C] R, CB-prefix instruction
rotReg :: Dir -> Boolean -> SetReg -> GetReg -> Regs -> Regs
rotReg dir isCarryRot setReg getReg regs =
  setReg rotated.res regs { f = rotated.flags }
 where rotated = rotX {dir,isCarryRot,isCB:true} (getReg regs) regs.f

-- RL[C] (HL)
-- RR[C] (HL)
rotHLMem :: forall e. Dir -> Boolean -> Mem -> Eff (ma :: MemAccess | e) Mem
rotHLMem dir isCarryRot mem@{mainMem,regs} = do
  hlMem <- rd8 addr mainMem
  let rotated = rotX {dir,isCarryRot,isCB:true} hlMem regs.f
  mainMem' <- wr8 rotated.res addr mainMem
  return mem { mainMem = mainMem', regs = regs { f = rotated.flags } }
 where
  addr = joinRegs h l regs

rotX :: {dir::Dir,isCarryRot::Boolean,isCB::Boolean}
      -> I8 -> I8 -> { res :: I8, flags :: I8 }
rotX { dir, isCarryRot, isCB } x oldFlags = { res, flags }
 where
  res = 255 .&. (currCarry + (x `shiftFunc` 1))
  flags = setNewCarry
    $ if isCB then testZeroFlag res else 0
  setNewCarry = if isNewCarry
    then (carryFlag .|. _)
    else (cmplCarryFlag .&. _)

  currCarry = if   (not isCarryRot) && (oldFlags .&. carryFlag /= 0)
                || (isCarryRot      && isNewCarry)
    then case dir of
      LeftD -> 1
      RightD -> 0x80 
    else 0
  isNewCarry = x .&. edgeBit /= 0
  edgeBit = case dir of
    LeftD -> 0x80
    RightD -> 1
  shiftFunc = case dir of
    LeftD -> shl
    RightD -> zshr

-- Shifts
-- ======

--SLA R
--SRA R preserve sign
--SRL R
shiftReg :: Dir -> Boolean -> SetReg -> GetReg
         -> Regs -> Regs
shiftReg dir sign setReg getReg regs =
  setReg shifted.res regs { f = shifted.flags }
 where shifted = shiftX dir sign $ getReg regs

--NOTE fix inconsistency in naming, memhl / hlmem
shiftMemHL :: forall e. Dir -> Boolean -> Mem -> Eff (ma :: MemAccess | e) Mem
shiftMemHL dir sign mem@{mainMem,regs} = do
  shifted <- shiftX dir sign <$> rd8 addr mainMem
  mem { mainMem = _, regs = regs { f = shifted.flags } }
    <$> wr8 shifted.res addr mainMem
 where addr = joinRegs h l regs

shiftX :: Dir -> Boolean -> I8
       -> { res::I8, flags::I8 }
shiftX dir sign x = { res, flags }
 where
  res =  (255 .&. _)
     <<< (if sign && dir == RightD then (currCarry + _) else id)
      $  x `shiftFunc` 1
  shiftFunc = case dir of
    LeftD -> shl
    RightD -> zshr
  flags = newCarry .|. testZeroFlag res
  newCarry = if x .&. edgeBit /= 0 then carryFlag else 0
  currCarry = x .&. 0x80 --sign is only relevant for right shifts
  edgeBit = case dir of
    LeftD -> 0x80
    RightD -> 1

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
  setReg inced.res regs {f = f'}
 where
  f' =  testZeroFlag inced.res
    .|. testHalfCarryFlag8 1 targetReg inced.res
    .|. (carryFlag .&. regs.f)
  inced = op targetReg
  targetReg = getReg regs

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
  setCarryReg carryReg' <<< setMainReg mainReg' $ regs
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
incDecSP op regs = regs {sp = op $ regs.sp}

--INC (HL)
incHLMem :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
incHLMem = incDecHLMem incI8

--DEC (HL)
decHLMem :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
decHLMem mem = do
 res <- incDecHLMem decI8 mem 
 return res { regs = adjFlag subtractionFlag res.regs }

incDecHLMem :: forall e. (I8 -> { res :: I8, carry :: Boolean })
            -> Mem -> Eff (ma :: MemAccess | e) Mem
incDecHLMem op mem@{mainMem,regs} = do
  inced <- op <$> rd8 addr mainMem
  mem { mainMem = _, regs = regs {f = testZeroFlag inced.res} }
    <$> wr8 inced.res addr mainMem
 where addr = joinRegs h l regs

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
ldRegFromReg setDestReg getSrcReg regs = setDestReg (getSrcReg regs) regs

--LD (nn),SP
ldMemImmFromSP :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
ldMemImmFromSP mem@{mainMem, regs} = do
  addr <- rd16 (regs.pc+1) mainMem
  mem { mainMem = _, regs = regs { pc = regs.pc + 3 } }
    <$> wr16 regs.sp addr mainMem

--LD SP,HL
ldSPFromHL :: Regs -> Regs
ldSPFromHL regs  = 
  regs { sp = sp' }
 where sp' = joinRegs h l regs
  
ldSPFromImmMem :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
ldSPFromImmMem { mainMem, regs } = 
  regs { sp = _, pc = regs.pc + 3 } <$> rd16 (regs.pc+1) mainMem
  
--LD RR,nn
ldTwoRegsFromImm :: forall e. SetReg -> SetReg
                 -> Mem -> Eff (ma :: MemAccess | e) Regs
ldTwoRegsFromImm setHighReg setLowReg mem@{regs} = do
  regs'  <- ldRegFromMem setLowReg (regs.pc+1) mem
  regs'' <- ldRegFromMem setHighReg (regs.pc+2) $ mem { regs = regs' }
  return regs'' { pc = regs.pc+3 }

--LD R,(IOC)
ldRegFromFF00CMem :: forall e. SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromFF00CMem setReg mem@{regs, mainMem} =
  ldRegFromFF00PlusX regs.c setReg mem

--LD R,(IOn)
ldRegFromFF00ImmMem :: forall e. SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromFF00ImmMem setReg mem@{regs, mainMem} = do
  imm <- rd8 (regs.pc+1) mainMem
  _ { pc = regs.pc + 2 } <$> ldRegFromFF00PlusX imm setReg mem

ldRegFromFF00PlusX :: forall e. I8 -> SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromFF00PlusX  x setReg mem@{regs} = ldRegFromMem setReg addr mem
 where addr = 0xFF00 + x

--LD R,n
ldRegFromImm :: forall e. SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromImm setReg mem@{regs} =
  _ { pc = regs.pc + 2 } <$> ldRegFromMem setReg (regs.pc+1) mem

--LD R,(RR)
ldRegFromMem2R :: forall e. SetReg -> GetReg -> GetReg
               -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromMem2R setReg msByteReg lsByteReg mem@{regs} =
  ldRegFromMem setReg ((msByteReg regs `shl` 8) + lsByteReg regs) mem

--LD A,(nn)
ldRegAFromMemImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
ldRegAFromMemImm mem@{ mainMem, regs } = do
  addr <- rd16 (regs.pc+1) mainMem
  _ { pc = regs.pc + 3 } <$> ldRegFromMem setA addr mem
  

--LDD A,(HL)
ldRegFromMemHLDec :: forall e. SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromMemHLDec = ldRegFromMemHLIncDec decRegWithCarry

--LDI A,(HL)
ldRegFromMemHLInc :: forall e. SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromMemHLInc = ldRegFromMemHLIncDec incRegWithCarry

ldRegFromMemHLIncDec :: forall e.
  -- Inc/Dec reg operation's type signature
  (GetReg -> GetReg -> SetReg -> SetReg -> Regs -> Regs)
  -> SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromMemHLIncDec op setReg mem@{ regs }
  = op h l setH setL
 <$> ldRegFromMem setA addr mem
 where
  addr = joinRegs h l regs

ldRegFromMem :: forall e. SetReg -> I16 -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromMem setReg addr { mainMem, regs } =
  (_ `setReg` regs) <$> rd8 addr mainMem

--LD HL,SP|n|
ldHLFromSPImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
ldHLFromSPImm  mem@{ regs, mainMem } = do
  immAbs <- absI8 <$> rd8 (regs.pc+1) mainMem
  let sum = (immAbs + regs.sp) .&. 0xFFFF
      split = splitI16 sum
      f' =  testHalfCarryFlag16 immAbs regs.sp sum
        .|. testHalfCarryFlag8 immAbs regs.sp sum
  return regs {h = split.ms, l = split.ls, pc = regs.pc + 2, f = f'}

--LDI (HL),A
ldMemHLFromRegInc :: forall e. GetReg -> Mem -> Eff (ma :: MemAccess | e) Mem
ldMemHLFromRegInc = ldMemHLFromRegIncDec incRegWithCarry

--LDD (HL),A
ldMemHLFromRegDec :: forall e. GetReg -> Mem -> Eff (ma :: MemAccess | e) Mem
ldMemHLFromRegDec  = ldMemHLFromRegIncDec decRegWithCarry

ldMemHLFromRegIncDec :: forall e.
  -- Inc/Dec reg operation's type signature
     (GetReg -> GetReg -> SetReg -> SetReg -> Regs -> Regs)
  -> GetReg
  -> Mem -> Eff (ma :: MemAccess | e) Mem
ldMemHLFromRegIncDec op getReg mem@{regs} = do
  mainMem' <- _.mainMem <$> ldMemFromReg addr a mem
  return mem { mainMem = mainMem', regs = regs' }
 where
  regs' = op h l setH setL regs
  addr = joinRegs h l regs

--LD (IOC),R
ldFF00CMemFromReg :: forall e. GetReg -> Mem -> Eff (ma :: MemAccess | e) Mem
ldFF00CMemFromReg getReg mem@{regs, mainMem} =
  ldFF00PlusXFromReg  regs.c getReg mem

--LD (IOn),R
ldFF00ImmMemFromReg :: forall e. GetReg -> Mem -> Eff (ma :: MemAccess | e) Mem
ldFF00ImmMemFromReg getReg mem@{regs, mainMem} = do
  imm <- rd8 (regs.pc+1) mainMem
  _ { regs = regs{pc = regs.pc + 2 } }
    <$> ldFF00PlusXFromReg imm getReg mem

ldFF00PlusXFromReg :: forall e. I8 -> GetReg -> Mem -> Eff (ma :: MemAccess | e) Mem
ldFF00PlusXFromReg x getReg mem@{regs, mainMem} = mem'
 where
  mem' = ldMemFromReg addr getReg mem
  addr = 0xFF00 + x

--LD (nn),A
ldMemImmFromRegA :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
ldMemImmFromRegA  mem@{ mainMem, regs } = do
  addr <- rd16 (regs.pc+1) mainMem
  _ { regs = regs{pc = regs.pc + 3} }
    <$> ldMemFromReg addr a mem

--LD (HL),n
ldMemHLFromImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
ldMemHLFromImm  mem@{mainMem,regs} = do
  imm <- rd8 (regs.pc+1) mainMem
  mem { mainMem = _, regs = regs{pc = regs.pc + 2} }
    <$> wr8 imm addr mainMem
 where
  addr = joinRegs h l regs

--LD (RR),R
ldMem2RFromReg :: forall e. GetReg -> GetReg -> GetReg
                        -> Mem -> Eff (ma :: MemAccess | e) Mem
ldMem2RFromReg msbAddrReg lsbAddrReg getReg mem@{ regs } =
  ldMemFromReg (joinRegs msbAddrReg lsbAddrReg regs) getReg mem

ldMemFromReg :: forall e. I16 -> GetReg
           -> Mem -> Eff (ma :: MemAccess | e) Mem
ldMemFromReg addr getReg mem@{ regs } = ldMemFromX addr (getReg regs) mem

ldMemFromX :: forall e. I16 -> I8
           -> Mem -> Eff (ma :: MemAccess | e) Mem
ldMemFromX addr x mem@{mainMem} =
  mem { mainMem = _ } <$> wr8 x addr mainMem 

-- Compares
-- ========

--CP A,R
compAToReg :: GetReg -> Regs -> Regs
compAToReg getReg regs = compAToX (getReg regs) regs

--CP A,(HL)
compAToMemHL :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
compAToMemHL { mainMem, regs } =
  (\mhl -> compAToX mhl regs)
    <$> rd8 (joinRegs h l regs) mainMem

--CP A,n
compAToImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
compAToImm { mainMem, regs } = do
  imm <- rd8 (regs.pc+1) mainMem
  return (compAToX imm regs) { pc = regs.pc + 2 }

compAToX :: I8 -> Regs -> Regs
compAToX x regs = regs { f = diff.flags }
 where diff = subI8s regs.a x

-- Stack operations
-- ================

--NOTE: Only 4 versions of PUSH so consider hard-coding them for performance.
--PUSH RR
pushReg :: forall e. GetReg -> GetReg
        -> Mem -> Eff (ma :: MemAccess | e) Mem
pushReg msByteReg lsByteReg mem@{regs,mainMem} = do
  mainMem' <-  wr8 (lsByteReg regs) (regs.sp - 2)
           <=< wr8 (msByteReg regs) (regs.sp - 1)
           $   mainMem
  return mem { mainMem = mainMem', regs = regs { sp = regs.sp - 2 } }

--POP RR
popReg :: forall e. SetReg -> SetReg
       -> Mem -> Eff (ma :: MemAccess | e) Regs
popReg setMsByteReg setLsByteReg { mainMem, regs = regs@{sp} } = do
  msByte <- rd8 (sp + 1) mainMem
  lsByte <- rd8 sp mainMem
  return <<< setMsByteReg msByte
         <<< setLsByteReg lsByte
          $  regs { sp = sp + 2 }

-- Jumps
-- =====

-- JR NZ,n
-- JR Z,n
-- JR NC,n
-- JR C,n
jumpRelImmFlag :: forall e. Boolean -> I8 -> Mem -> Eff (ma :: MemAccess | e) Regs
jumpRelImmFlag inverse flag mem@{mainMem, regs} =
  if inverse `xor` isSetFlag flag regs.f
    then _ { brTkn = true } <$> (jumpRelImm mem)
    else return regs { pc = regs.pc + 2, brTkn = false }

--JR n
jumpRelImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
jumpRelImm { mainMem, regs } = do
  imm <- rd8 (regs.pc+1) mainMem
  let absImm = absI8 imm
      addOrSubImm = if absImm == imm
        then (_ + absImm) else (_ - absImm)
      pc' = addOrSubImm $ regs.pc + 2
  return regs { pc = pc' }

-- JP NZ,nn
-- JP Z,nn
-- JP NC,nn
-- JP C,nn
jumpImmFlag :: forall e. Boolean -> I8 -> Mem -> Eff (ma :: MemAccess | e) Regs
jumpImmFlag inverse flag mem@{mainMem,regs} =
  if inverse `xor` isSetFlag flag regs.f
    then _ { brTkn = true } <$> (jumpImm mem)
    else return regs { pc = regs.pc + 3, brTkn = false }

-- JP nn
jumpImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
jumpImm { mainMem, regs } =
  regs { pc = _ } <$> rd16 (regs.pc+1) mainMem

-- JP (HL)
jumpHL :: Regs -> Regs
jumpHL regs = regs { pc = pc' }
 where pc' = joinRegs h l regs

-- Call Z,nn
-- Call NZ,nn
-- Call C,nn
-- Call NC,nn
callImmFlag :: forall e. Boolean -> I8 -> Mem -> Eff (ma :: MemAccess | e) Mem
callImmFlag inverse flag mem@{mainMem,regs} = 
  if inverse `xor` isSetFlag flag regs.f
    then adjRegs (_ { brTkn = true }) <$> (callImm mem)
    else return mem { regs = regs {pc = regs.pc + 3, brTkn = false} }

-- Call nn
callImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
callImm mem@{mainMem,regs} = do
  imm <- rd16 (regs.pc+1) mainMem
  mainMem' <- wr16 (regs.pc + 3) (regs.sp-2) $ mainMem
  return mem { mainMem =  mainMem', regs = regs { pc = imm, sp = regs.sp - 2 } }

-- RET Z
-- RET NZ
-- RET C
-- RET NC
retFlag :: forall e. Boolean -> I8 -> Mem -> Eff (ma :: MemAccess | e) Regs
retFlag inverse flag mem@{mainMem,regs} = 
  if inverse `xor` isSetFlag flag regs.f
    then _ { brTkn = true } <$> (ret mem) 
    else return regs { brTkn = false }

-- RETI
--NOTE; not certain restoreRegs is necessary here
retEnableInterrupt :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
retEnableInterrupt mem@{ mainMem, regs, svdRegs } = do
  pc' <- rd16 regs.sp mainMem
  let regs' = regs { pc = pc', sp = regs.sp + 2 }
  mainMem' <- setIme true mainMem
  return mem { mainMem = mainMem', regs = regs' }

-- RET
ret :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
ret { mainMem, regs } =
  regs { pc = _, sp = regs.sp + 2 } <$> rd16 regs.sp mainMem
  
--RST XX
callRoutine :: forall e. Boolean -> I16 -> Mem -> Eff (ma :: MemAccess | e) Mem
callRoutine fromIntrr addr { mainMem, regs, svdRegs } =
  { mainMem : _, regs : regs', svdRegs : saveRegs regs svdRegs }
    <$> wr16 retAddr (regs.sp - 2) mainMem
 where
  regs' = regs { sp = regs.sp - 2, pc = addr }
  retAddr = if fromIntrr then regs.pc else regs.pc + 1

-- Misc.
-- =====

--SWAP R
swapReg :: SetReg -> GetReg -> Regs -> Regs
swapReg setReg getReg regs = setReg reg' $ regs { f = f' }
 where
  f' = testZeroFlag reg'
  reg' = ((0x0F.&.reg) `shl` 4) .|. ((0xF0.&.reg) `zshr` 4)
  reg = getReg regs

--SWAP (HL)
swapMemHL :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
swapMemHL mem@{mainMem, regs} = do
  hlMem <- rd8 addr mainMem
  let hlMem' = ((0x0F.&.hlMem) `shl` 4) .|. ((0xF0.&.hlMem) `zshr` 4)
  mem { mainMem = _ }
    <$> wr8 hlMem' addr mainMem
 where addr = joinRegs h l regs

--Why a command called 'clear' actually flips the tag
--and not unset it, is a question that shouldn't be directed at me!
--CCF
clearCarryFlag :: Regs -> Regs
clearCarryFlag = changeCarryFlag false

--SCF
setCarryFlag :: Regs -> Regs
setCarryFlag = changeCarryFlag true

changeCarryFlag :: Boolean -> Regs -> Regs
changeCarryFlag isSet regs = regs { f = f' }
 where f' =  unsetFlag subtractionFlag
         <<< unsetFlag halfCarryFlag
         <<< (if isSet then setFlag else flipFlag) carryFlag
          $  regs.f

--DAA
adjAForBCDAdd :: Regs -> Regs
adjAForBCDAdd regs = regs { f = f', a = 0xFF .&. a' }
 where
  f' = setFlagCond (0x100 .&. a' /= 0) carryFlag
     $ setZeroFlag (0xFF .&. a')
     $ subtractionFlag .&. regs.f 

  a' = adj.highNyb <<< adj.lowNyb $ regs.a
  adj = if regs.f .&. subtractionFlag /= 0
    then
      { lowNyb  : if (regs.f .&. halfCarryFlag /= 0)
                    then  (0xFF .&. _) <<< (_ - 6) else id
      , highNyb : if (regs.f .&. carryFlag /= 0)
                    then (_ - 0x60) else id
      }
    else
      { lowNyb  : if (regs.f .&. halfCarryFlag /= 0) || (0x0F .&. regs.a > 9)
                    then (6 + _) else id
      , highNyb : if (regs.f .&. carryFlag /= 0) || (regs.a > 0x9F)
                    then (0x60 + _) else id
      }

--NOP
nop :: Regs -> Regs
nop = id

--Invalid opCode
--NOTE: LOG THIS! for debugging purposes
invalidOpCode :: Z80State -> Z80State
invalidOpCode state = state { stop = true }

--STOP 
--NOTE: this is just a guess
stop :: Z80State -> Z80State
stop state =
  state { stop = true }

--HALT
halt :: Z80State -> Z80State
halt state =
  state { halt = true }

--DI
--EI
setInterrupts :: forall e. Boolean -> Mem -> Eff (ma :: MemAccess | e) Mem
setInterrupts enable mem@{ mainMem } = do
  if enable
    then setImeCntDwn mainMem
    else setIme false mainMem
  return mem

--Extended Ops
execExtOps :: forall e. Array (Z80State -> Eff (ma :: MemAccess | e) Z80State)
           -> Z80State -> Eff (ma :: MemAccess | e) Z80State
execExtOps opsMap state = do
  opCode <- rd8 (state.mem.regs.pc+1) state.mem.mainMem
  let extOp = getCpuOp opCode opsMap 
  state' <- extOp state
  return state' { mem = state'.mem {
           regs = state'.mem.regs {
             pc = 65535 .&. (state'.mem.regs.pc + 2) } } }

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

adjFlag :: I8 -> Regs -> Regs
adjFlag flag regs = regs { f = setFlag flag regs.f }

isSetFlag :: I8 -> I8 -> Boolean
isSetFlag f fs = f .&. fs /= 0

setFlag :: I8 -> I8 -> I8
setFlag  f = (f .|. _)

setFlagCond :: Boolean -> I8 -> I8 -> I8
setFlagCond true  f = (f .|. _)
setFlagCond false f = (complement f .&. _)

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

setCarryFlag8 :: Int -> I8 -> I8
setCarryFlag8 x = if x > 255
  then (_ .|. carryFlag)
  else (_ .&. cmplCarryFlag)

testCarryFlag16 :: Int -> I8
testCarryFlag16 x = if x > 0xFFFF then carryFlag else 0

setCarryFlag16 :: Int -> I8 -> I8
setCarryFlag16 x = if x > 0xFFFF
  then (_ .|. carryFlag)
  else (_ .&. cmplCarryFlag)

testNegCarryFlag :: Int -> I8
testNegCarryFlag reg = if reg < 0 then carryFlag else 0

testHalfCarryFlag8 :: I8 -> I8 -> I8 -> I8
testHalfCarryFlag8 reg1 reg2 sum = if res /= 0 then halfCarryFlag else 0
 where res = 0x10 .&. (reg1 .^. reg2 .^. sum)

setHalfCarryFlag8 :: I8 -> I8 -> I8 -> I8 -> I8
setHalfCarryFlag8 reg1 reg2 sum =
  if res /= 0 then (_ .|. halfCarryFlag) else (_ .&. cmplHalfCarryFlag)
 where res = 0x10 .&. (reg1 .^. reg2 .^. sum)

testHalfCarryFlag16 :: I16 -> I16 -> I16 -> I8
testHalfCarryFlag16 reg1 reg2 sum = if res /= 0 then carryFlag else 0
 where res = 0x0100 .&. (reg1 .^. reg2 .^. sum)

--NOTE: I'm not sure why this calculation is the way it is, or what it represents.
--but according to specifications, this is how it should be done.
testWeirdHalfCarryFlag16 :: I16 -> I16 -> I16 -> I8
testWeirdHalfCarryFlag16 reg1 reg2 sum = if res /= 0 then halfCarryFlag else 0
 where res = 0x1000 .&. (reg1 .^. reg2 .^. sum)

--NOTE: replace fromMaybe with an error report mechanism to trace bad cases
getCpuOp :: forall e. Int -> Array (Z80State -> Eff (ma :: MemAccess | e) Z80State)
         -> Z80State -> Eff (ma :: MemAccess | e) Z80State
getCpuOp ix table = fromMaybe return $ table A.!! ix

--NOTE: replace fromMaybe with an error report mechanism to trace bad cases
getOpTiming :: Int -> Array Int -> Int
getOpTiming ix table = fromMaybe (-1) $ table A.!! ix

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
