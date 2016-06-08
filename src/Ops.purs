module Ops where

import Prelude (class Eq, not, negate, ($), return, (/=), (<), (>), (==)
               ,(+), bind, id, (||), (-), (<<<), (<$>), (&&), flip)
import Data.Int.Bits ((.^.), (.&.), (.|.), complement, zshr, shl)
import Data.Maybe (fromMaybe)
import Data.Array ((!!)) as A
import Control.Monad.Eff (Eff)
import Control.Bind ((<=<),(=<<))

import Types (I8, Z80State, MemAccess, I16, Regs, GetReg, SavedRegs, Mem
             ,SetReg,adjRegs)
import MainMem (rd8, setIme, setImeCntDwn, wr8, wr16, rd16)
import Utils (cmp2)
import Regs

-- Additions
-- =========

--ADD A,R
addRegToA :: forall e. GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
addRegToA getReg regs = addXToA (getReg regs) regs

--ADD A,(HL)
addHLMemToA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
addHLMemToA { mainMem, regs } = do
  hlMem <- rd8 (joinRegs h l regs) mainMem
  addXToA hlMem regs

--ADD A,n
addImmToA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
addImmToA { mainMem, regs } = do
  imm <- rd8 ((pc regs)+1) mainMem
  addXToA imm regs
  setPC (pc regs + 2) regs

addXToA :: forall e. I8 -> Regs -> Eff (ma :: MemAccess | e) Regs
addXToA x regs =
  setA sum.res =<< setF sum.flags regs
 where sum = addI8s (a regs) x

--ADC A,R
addRegCarryToA :: forall e. GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
addRegCarryToA getReg regs = addXCarryToA (getReg regs) regs

--ADC A,(HL)
addHLMemCarryToA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
addHLMemCarryToA { mainMem, regs } = do
  hlMem <- rd8 (joinRegs h l regs) mainMem
  addXCarryToA hlMem regs

--ADC A,n
addImmCarryToA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
addImmCarryToA { mainMem, regs } = do
  imm <- rd8 ((pc regs)+1) mainMem
  addXCarryToA imm regs
  setPC (pc regs + 2) regs

--NOTE: Test edge case of carry flag of 1 that causes a half-carry,
--Is the half-carry flag set correctly?
addXCarryToA :: forall e. I8 -> Regs -> Eff (ma :: MemAccess | e) Regs
addXCarryToA x regs =
  setA sum.res =<< setF f' regs
 where
  --Overriding addI8's half-carry flag because it used (a regs)+carry
  --Which is essential for the other flags, but not for this one.
  f' = setHalfCarryFlag8 x (a regs) sum.res sum.flags
  sum = addI8s ((a regs) + carry) x
  carry = if (f regs) .&. carryFlag /= 0 then 1 else 0

--ADD SP,|n|
addImmToSP :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
addImmToSP { regs, mainMem } = do
  immAbs <- absI8 <$> rd8 ((pc regs)+1) mainMem
  let sp' = (immAbs + (sp regs)) .&. 0xFFFF
      f' =  testHalfCarryFlag16 immAbs (sp regs) sp'
        .|. testHalfCarryFlag8 immAbs (sp regs) sp'
  setSP sp' =<< setPC ((pc regs) + 2) =<< setF f' regs

addI8s :: I8 -> I8 -> { res :: I8, flags :: I8 }
addI8s x1 x2 = { res , flags }
 where
  flags =  testZeroFlag res
       .|. testCarryFlag8 sum
       .|. testHalfCarryFlag8 x1 x2 res
  res = sum .&. 255
  sum = x1 + x2

--ADD HL,RR
add2RegsToHL :: forall e. GetReg -> GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
add2RegsToHL msByteReg lsByteReg regs =
  setH split.ms =<< setL split.ls =<< setF sum.flags regs
 where
  split = splitI16 sum.res
  sum = addI16s hl joinedRegs (f regs)
  joinedRegs = joinRegs msByteReg lsByteReg regs
  hl = joinRegs h l regs

--ADD HL,SP
addSPToHL :: forall e. Regs -> Eff (ma :: MemAccess | e) Regs
addSPToHL regs =
  setH split.ms =<< setL split.ls =<< setF sum.flags regs
 where
  split = splitI16 sum.res
  sum = addI16s hl (sp regs) (f regs)
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
subRegFromA :: forall e. GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
subRegFromA getReg regs = subXFromA (getReg regs) regs

--SUB A,(HL)
subHLMemFromA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
subHLMemFromA { mainMem, regs } = do
  hlMem <- rd8 (joinRegs h l regs) mainMem
  subXFromA hlMem regs

--SUB A,n
subImmFromA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
subImmFromA { mainMem, regs } = do
  imm <- rd8 ((pc regs)+1) mainMem
  subXFromA imm regs
  setPC (pc regs + 2) regs

--NOTE: Subtractions and additions are relatively similar. Consider
--Using same functions for both,
--if it doesn't obfuscate much nor degrade performance.
subXFromA :: forall e. I8 -> Regs -> Eff (ma :: MemAccess | e) Regs
subXFromA x regs =
  setA diff.res =<< setF diff.flags regs
 where diff = subI8s (a regs) x

--SBC A,R
subRegCarryFromA :: forall e. GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
subRegCarryFromA getReg regs = subXCarryFromA (getReg regs) regs

--SBC A,(HL)
subHLMemCarryFromA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
subHLMemCarryFromA { mainMem, regs } = do
  mhl <- rd8 (joinRegs h l regs) mainMem
  subXCarryFromA mhl regs

--SBC A,n
subImmCarryToA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
subImmCarryToA { mainMem, regs } = do
  imm <- rd8 ((pc regs)+1) mainMem
  subXCarryFromA imm regs
  setPC (pc regs + 2) regs

--NOTE: Test edge case of carry flag of 1 that causes a half-carry,
--Is the half-carry flag set correctly?
subXCarryFromA :: forall e. I8 -> Regs -> Eff (ma :: MemAccess | e) Regs
subXCarryFromA x regs =
  setA diff.res =<< setF f' regs
 where
  f' = setHalfCarryFlag8 x (a regs) diff.res diff.flags
  diff = subI8s ((a regs) - carry) x
  carry = if (f regs) .&. carryFlag /= 0 then 1 else 0

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
andOpRegIntoA :: forall e. GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
andOpRegIntoA getReg =  adjFlag halfCarryFlag
                    <=< boolOpRegIntoA (.&.) getReg

--OR A,R
orOpRegIntoA :: forall e. GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
orOpRegIntoA = boolOpRegIntoA (.|.)

--XOR A,R
xorOpRegIntoA :: forall e. GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
xorOpRegIntoA = boolOpRegIntoA (.^.)

boolOpRegIntoA :: forall e. (I8 -> I8 -> I8) -> GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
boolOpRegIntoA op getReg regs = boolOpXIntoA op (getReg regs) regs

--AND A,(HL)
andOpHLMemIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
andOpHLMemIntoA =  adjFlag halfCarryFlag 
               <=< boolOpHlMemIntoA (.&.)

--OR A,(HL)
orOpHLMemIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
orOpHLMemIntoA = boolOpHlMemIntoA (.|.)

--XOR A,(HL)
xorOpHLMemIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
xorOpHLMemIntoA = boolOpHlMemIntoA (.^.)

boolOpHlMemIntoA  :: forall e. (I8 -> I8 -> I8) -> Mem -> Eff (ma :: MemAccess | e) Regs
boolOpHlMemIntoA op { mainMem, regs } = do
  hlMem <- rd8 (joinRegs h l regs) mainMem
  boolOpXIntoA op hlMem regs

--AND A,Imm
andOpImmIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
andOpImmIntoA =  adjFlag halfCarryFlag
             <=< boolOpImmIntoA (.&.)

--OR A,Imm
orOpImmIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
orOpImmIntoA = boolOpImmIntoA (.|.)

--XOR A,Imm
xorOpImmIntoA :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
xorOpImmIntoA = boolOpImmIntoA (.^.)

boolOpImmIntoA :: forall e. (I8 -> I8 -> I8) -> Mem -> Eff (ma :: MemAccess | e) Regs
boolOpImmIntoA op { mainMem, regs } = do
  imm <- rd8 ((pc regs)+1) mainMem
  boolOpXIntoA op imm regs
  setPC (pc regs + 2) regs

boolOpXIntoA :: forall e. (I8 -> I8 -> I8) -> I8 -> Regs -> Eff (ma :: MemAccess | e) Regs
boolOpXIntoA op x regs =
  setA a' =<< setF (testZeroFlag a') regs
 where a' = ((a regs) `op` x) .&. 255

-- Bit operations
-- ==============

--CPL
cmplA :: forall e. Regs -> Eff (ma :: MemAccess | e) Regs
cmplA regs = setA a' =<< setF f' regs
 where
   a' = 255 .^. (a regs)
   f' =  setFlag halfCarryFlag
     <<< setFlag subtractionFlag
      $  (f regs)

--BIT N,R
testBitNOfReg :: forall e. I8 -> GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
testBitNOfReg n getReg regs = testBitNOfX n (getReg regs) regs

--BIT N,(HL)
testBitNOfHLMem :: forall e. I8 -> Mem -> Eff (ma :: MemAccess | e) Regs
testBitNOfHLMem n { mainMem, regs } = do
  hlMem <- rd8 (joinRegs h l regs) mainMem
  testBitNOfX n hlMem regs

testBitNOfX :: forall e. Int -> I8 -> Regs -> Eff (ma :: MemAccess | e) Regs
testBitNOfX n x regs = setF f' regs
 where
  f' =  setZeroFlag bitTest
     $  unsetFlag subtractionFlag
     $  halfCarryFlag
    .|. ((f regs) .&. carryFlag) -- NOTE: should it be 0x1F instead?
  bitTest = x .&. (1 `shl` n)

--SET N,R
--RES N,R
setBitNOfReg :: forall e. Boolean -> Int -> SetReg -> GetReg
               -> Regs -> Eff (ma :: MemAccess | e) Regs
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
rotA :: forall e. Dir -> Boolean -> Regs -> Eff (ma :: MemAccess | e) Regs
rotA dir isCarryRot regs = setA rotated.res =<< setF rotated.flags regs
 where rotated = rotX {dir,isCarryRot,isCB:false} (a regs) (f regs)

-- RL[C] R, CB-prefix instruction
-- RR[C] R, CB-prefix instruction
rotReg :: forall e. Dir -> Boolean -> SetReg -> GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
rotReg dir isCarryRot setReg getReg regs =
  setReg rotated.res =<< setF rotated.flags regs
 where rotated = rotX {dir,isCarryRot,isCB:true} (getReg regs) (f regs)

-- RL[C] (HL)
-- RR[C] (HL)
rotHLMem :: forall e. Dir -> Boolean -> Mem -> Eff (ma :: MemAccess | e) Mem
rotHLMem dir isCarryRot mem@{mainMem,regs} = do
  hlMem <- rd8 addr mainMem
  let rotated = rotX {dir,isCarryRot,isCB:true} hlMem (f regs)
  mainMem' <- wr8 rotated.res addr mainMem
  setF rotated.flags regs
  return mem { mainMem = mainMem' }
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
shiftReg :: forall e. Dir -> Boolean -> SetReg -> GetReg
         -> Regs -> Eff (ma :: MemAccess | e) Regs
shiftReg dir sign setReg getReg regs =
  setReg shifted.res =<< setF shifted.flags regs
 where shifted = shiftX dir sign $ getReg regs

--NOTE fix inconsistency in naming, memhl / hlmem
shiftMemHL :: forall e. Dir -> Boolean -> Mem -> Eff (ma :: MemAccess | e) Mem
shiftMemHL dir sign mem@{mainMem,regs} = do
  shifted <- shiftX dir sign <$> rd8 addr mainMem
  setF shifted.flags regs
  mem { mainMem = _ }
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
incReg :: forall e. GetReg -> SetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
incReg = incDecReg incI8

--DEC R
decReg :: forall e. GetReg -> SetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
decReg getReg setReg regs = do
  incDecReg decI8 getReg setReg regs
  adjFlag subtractionFlag regs

incDecReg :: forall e. (I8 -> { res :: I8, carry :: Boolean })
          -> GetReg -> SetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
incDecReg op getReg setReg regs =
  setReg inced.res =<< setF f' regs
 where
  f' =  testZeroFlag inced.res
    .|. testHalfCarryFlag8 1 targetReg inced.res
    .|. (carryFlag .&. (f regs))
  inced = op targetReg
  targetReg = getReg regs

--INC RR
incRegWithCarry :: forall e. GetReg -> GetReg -> SetReg -> SetReg
                -> Regs -> Eff (ma :: MemAccess | e) Regs
incRegWithCarry = incDecRegWithCarry incI8

--DEC RR
decRegWithCarry :: forall e. GetReg -> GetReg -> SetReg -> SetReg
                -> Regs -> Eff (ma :: MemAccess | e) Regs
decRegWithCarry = incDecRegWithCarry decI8

--NOTE: consider refactoring to something along the lines of
--"split2Regs . incI16 . joinRegs", if you have leeway, performance-wise.
incDecRegWithCarry :: forall e. (I8 -> { res :: I8, carry :: Boolean })
          -> GetReg -> GetReg -> SetReg -> SetReg
          -> Regs -> Eff (ma :: MemAccess | e) Regs
incDecRegWithCarry op getCarryReg getMainReg setCarryReg setMainReg regs = 
  setCarryReg carryReg' =<< setMainReg mainReg' regs
 where
  carryReg' = (if mainRegInc.carry then _.res <<< op else id)
    $ getCarryReg regs
  mainReg' = mainRegInc.res
  mainRegInc = op $ getMainReg regs

--INC SP
incSP :: forall e. Regs -> Eff (ma :: MemAccess | e) Regs
incSP = incDecSP $ \x -> 0xFFFF .&. (x + 1)

--DEC SP
decSP :: forall e. Regs -> Eff (ma :: MemAccess | e) Regs
decSP = incDecSP $ \x -> 0xFFFF .&. (x - 1)

incDecSP :: forall e. (I16 -> I16) -> Regs -> Eff (ma :: MemAccess | e) Regs
incDecSP op regs = setSP (op $ sp regs) regs

--INC (HL)
incHLMem :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
incHLMem = incDecHLMem incI8

--DEC (HL)
decHLMem :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
decHLMem mem = do
 mem' <- incDecHLMem decI8 mem 
 adjFlag subtractionFlag mem.regs
 return mem'

incDecHLMem :: forall e. (I8 -> { res :: I8, carry :: Boolean })
            -> Mem -> Eff (ma :: MemAccess | e) Mem
incDecHLMem op mem@{mainMem,regs} = do
  inced <- op <$> rd8 addr mainMem
  setF (testZeroFlag inced.res) regs
  mem { mainMem = _ }
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
ldRegFromReg :: forall e. SetReg -> GetReg
      -> Regs -> Eff (ma :: MemAccess | e) Regs
ldRegFromReg setDestReg getSrcReg regs = setDestReg (getSrcReg regs) regs

--LD (nn),SP
ldMemImmFromSP :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
ldMemImmFromSP mem@{mainMem, regs} = do
  addr <- rd16 ((pc regs)+1) mainMem
  setPC ((pc regs) + 3) regs
  mem { mainMem = _ }
    <$> wr16 (sp regs) addr mainMem

--LD SP,HL
ldSPFromHL :: forall e. Regs -> Eff (ma :: MemAccess | e) Regs
ldSPFromHL regs  = setSP sp' regs
 where sp' = joinRegs h l regs
  
ldSPFromImmMem :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
ldSPFromImmMem { mainMem, regs } = do
  sp' <- rd16 ((pc regs)+1) mainMem
  setSP sp' =<< setPC ((pc regs) + 3) regs
  
--LD RR,nn
ldTwoRegsFromImm :: forall e. SetReg -> SetReg
                 -> Mem -> Eff (ma :: MemAccess | e) Regs
ldTwoRegsFromImm setHighReg setLowReg mem@{regs} = do
  ldRegFromMem setLowReg (pc regs + 1) mem
  ldRegFromMem setHighReg (pc regs + 2)  mem
  setPC (pc regs + 3) regs

--LD R,(IOC)
ldRegFromFF00CMem :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromFF00CMem mem@{regs, mainMem} =
  ldRegFromFF00PlusX (c regs) setA mem

--LD R,(IOn)
ldRegFromFF00ImmMem :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromFF00ImmMem mem@{regs, mainMem} = do
  imm <- rd8 (pc regs + 1) mainMem
  ldRegFromFF00PlusX imm setA mem
  setPC (pc regs + 2) regs

ldRegFromFF00PlusX :: forall e. I8 -> SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromFF00PlusX  x setReg mem@{regs} = ldRegFromMem setReg addr mem
 where addr = 0xFF00 + x

--LD R,n
ldRegFromImm :: forall e. SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromImm setReg mem@{regs} =
  setPC (pc regs + 2) =<< ldRegFromMem setReg ((pc regs)+1) mem

--LD R,(RR)
ldRegFromMem2R :: forall e. SetReg -> GetReg -> GetReg
               -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromMem2R setReg msByteReg lsByteReg mem@{regs} =
  ldRegFromMem setReg ((msByteReg regs `shl` 8) + lsByteReg regs) mem

--LD A,(nn)
ldRegAFromMemImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
ldRegAFromMemImm mem@{ mainMem, regs } = do
  addr <- rd16 ((pc regs)+1) mainMem
  ldRegFromMem setA addr mem
  setPC (pc regs + 3) regs
  

--LDD A,(HL)
ldRegFromMemHLDec :: forall e. SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromMemHLDec = ldRegFromMemHLIncDec decRegWithCarry

--LDI A,(HL)
ldRegFromMemHLInc :: forall e. SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromMemHLInc = ldRegFromMemHLIncDec incRegWithCarry

ldRegFromMemHLIncDec :: forall e.
  -- Inc/Dec reg operation's type signature
  (GetReg -> GetReg -> SetReg -> SetReg -> Regs -> Eff (ma :: MemAccess | e) Regs)
  -> SetReg -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromMemHLIncDec op setReg mem@{ regs }
  = op h l setH setL =<< ldRegFromMem setA addr mem
 where addr = joinRegs h l regs

ldRegFromMem :: forall e. SetReg -> I16 -> Mem -> Eff (ma :: MemAccess | e) Regs
ldRegFromMem setReg addr { mainMem, regs } =
  flip setReg regs =<< rd8 addr mainMem

--LD HL,SP|n|
ldHLFromSPImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
ldHLFromSPImm  mem@{ regs, mainMem } = do
  immAbs <- absI8 <$> rd8 ((pc regs)+1) mainMem
  let sum = (immAbs + (sp regs)) .&. 0xFFFF
      split = splitI16 sum
      f' =  testHalfCarryFlag16 immAbs (sp regs) sum
        .|. testHalfCarryFlag8 immAbs (sp regs) sum
  setH split.ms =<< setL split.ls =<< setPC ((pc regs) + 2) =<< setF f' regs

--LDI (HL),A
ldMemHLFromRegInc :: forall e. GetReg -> Mem -> Eff (ma :: MemAccess | e) Mem
ldMemHLFromRegInc = ldMemHLFromRegIncDec incRegWithCarry

--LDD (HL),A
ldMemHLFromRegDec :: forall e. GetReg -> Mem -> Eff (ma :: MemAccess | e) Mem
ldMemHLFromRegDec  = ldMemHLFromRegIncDec decRegWithCarry

ldMemHLFromRegIncDec :: forall e.
  -- Inc/Dec reg operation's type signature
     (GetReg -> GetReg -> SetReg -> SetReg -> Regs -> Eff (ma :: MemAccess | e) Regs)
  -> GetReg
  -> Mem -> Eff (ma :: MemAccess | e) Mem
ldMemHLFromRegIncDec op getReg mem@{regs} = do
  mainMem' <- _.mainMem <$> ldMemFromReg addr a mem
  op h l setH setL regs
  return mem
 where addr = joinRegs h l regs

--LD (IOC),R
ldFF00CMemFromReg :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
ldFF00CMemFromReg mem@{regs, mainMem} =
  ldFF00PlusXFromReg  (c regs) a mem

--LD (IOn),A
ldFF00ImmMemFromReg :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
ldFF00ImmMemFromReg mem@{regs, mainMem} = do
  imm <- rd8 ((pc regs)+1) mainMem
  ldFF00PlusXFromReg imm a mem
  setPC (pc regs + 2) regs
  return mem

ldFF00PlusXFromReg :: forall e. I8 -> GetReg -> Mem -> Eff (ma :: MemAccess | e) Mem
ldFF00PlusXFromReg x getReg mem@{regs, mainMem} = mem'
 where
  mem' = ldMemFromReg addr getReg mem
  addr = 0xFF00 + x

--LD (nn),A
ldMemImmFromRegA :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
ldMemImmFromRegA  mem@{ mainMem, regs } = do
  addr <- rd16 ((pc regs)+1) mainMem
  ldMemFromReg addr a mem
  setPC (pc regs + 3) regs
  return mem

--LD (HL),n
ldMemHLFromImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
ldMemHLFromImm  mem@{mainMem,regs} = do
  imm <- rd8 ((pc regs)+1) mainMem
  setPC (pc regs + 2) regs
  mem { mainMem = _ }
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
compAToReg :: forall e. GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
compAToReg getReg regs = compAToX (getReg regs) regs

--CP A,(HL)
compAToMemHL :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
compAToMemHL { mainMem, regs } = do
  mhl <- rd8 (joinRegs h l regs) mainMem
  compAToX mhl regs

--CP A,n
compAToImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
compAToImm { mainMem, regs } = do
  imm <- rd8 ((pc regs)+1) mainMem
  compAToX imm regs
  setPC (pc regs + 2) regs

compAToX :: forall e. I8 -> Regs -> Eff (ma :: MemAccess | e) Regs
compAToX x regs = setF diff.flags regs
 where diff = subI8s (a regs) x

-- Stack operations
-- ================

--NOTE: Only 4 versions of PUSH so consider hard-coding them for performance.
--PUSH RR
pushReg :: forall e. GetReg -> GetReg
        -> Mem -> Eff (ma :: MemAccess | e) Mem
pushReg msByteReg lsByteReg mem@{regs,mainMem} = do
  mainMem' <-  wr8 (lsByteReg regs) ((sp regs) - 2)
           <=< wr8 (msByteReg regs) ((sp regs) - 1)
           $   mainMem
  setSP ((sp regs) - 2) regs
  return mem { mainMem = mainMem' }

--POP RR
popReg :: forall e. SetReg -> SetReg
       -> Mem -> Eff (ma :: MemAccess | e) Regs
popReg setMsByteReg setLsByteReg { mainMem, regs } = do
  msByte <- rd8 (oldSP + 1) mainMem
  lsByte <- rd8 oldSP mainMem
  setSP (oldSP + 2) regs
  setMsByteReg msByte =<< setLsByteReg lsByte regs
 where oldSP = sp regs

-- Jumps
-- =====

-- JR NZ,n
-- JR Z,n
-- JR NC,n
-- JR C,n
jumpRelImmFlag :: forall e. Boolean -> I8 -> Mem -> Eff (ma :: MemAccess | e) Regs
jumpRelImmFlag inverse flag mem@{mainMem, regs} =
  if inverse `xor` isSetFlag flag (f regs)
    then do
      jumpRelImm mem
      setBrTkn true regs
    else do
      setBrTkn false regs
      setPC ((pc regs) + 2) regs

--JR n
jumpRelImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
jumpRelImm { mainMem, regs } = do
  imm <- rd8 ((pc regs)+1) mainMem
  let absImm = absI8 imm
      addOrSubImm = if absImm == imm
        then (_ + absImm) else (_ - absImm)
      pc' = addOrSubImm $ (pc regs) + 2
  setPC pc' regs

-- JP NZ,nn
-- JP Z,nn
-- JP NC,nn
-- JP C,nn
jumpImmFlag :: forall e. Boolean -> I8 -> Mem -> Eff (ma :: MemAccess | e) Regs
jumpImmFlag inverse flag mem@{mainMem,regs} =
  if inverse `xor` isSetFlag flag (f regs)
    then do
      jumpImm mem
      setBrTkn true regs
    else setPC ((pc regs) + 3) =<< setBrTkn false regs

-- JP nn
jumpImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
jumpImm { mainMem, regs } =
  flip setPC regs =<< rd16 ((pc regs)+1) mainMem

-- JP (HL)
jumpHL :: forall e. Regs -> Eff (ma :: MemAccess | e) Regs
jumpHL regs = setPC pc' regs
 where pc' = joinRegs h l regs

-- Call Z,nn
-- Call NZ,nn
-- Call C,nn
-- Call NC,nn
callImmFlag :: forall e. Boolean -> I8 -> Mem -> Eff (ma :: MemAccess | e) Mem
callImmFlag inverse flag mem@{mainMem,regs} = 
  if inverse `xor` isSetFlag flag (f regs)
    then do
      callImm mem
      setBrTkn true regs
      return mem
    else do
      setPC ((pc regs) + 3) =<< setBrTkn false regs
      return mem

-- Call nn
callImm :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
callImm mem@{mainMem,regs} = do
  imm <- rd16 ((pc regs)+1) mainMem
  mainMem' <- wr16 ((pc regs) + 3) ((sp regs)-2) $ mainMem
  setPC imm =<< setSP ((sp regs) - 2) regs
  return mem { mainMem =  mainMem' }

-- RET Z
-- RET NZ
-- RET C
-- RET NC
retFlag :: forall e. Boolean -> I8 -> Mem -> Eff (ma :: MemAccess | e) Regs
retFlag inverse flag mem@{mainMem,regs} = 
  if inverse `xor` isSetFlag flag (f regs)
    then do
      ret mem
      setBrTkn true regs
    else setBrTkn false regs

-- RETI
--NOTE; not certain restoreRegs is necessary here
retEnableInterrupt :: forall e. Mem -> Eff (ma :: MemAccess | e) Mem
retEnableInterrupt mem@{ mainMem, regs, svdRegs } = do
  pc' <- rd16 (sp regs) mainMem
  mainMem' <- setIme true mainMem
  setPC pc' =<< setSP ((sp regs) + 2) regs
  return mem --{ mainMem = mainMem' }

-- RET
ret :: forall e. Mem -> Eff (ma :: MemAccess | e) Regs
ret { mainMem, regs } =
  setSP ((sp regs) + 2) =<< flip setPC regs =<< rd16 (sp regs) mainMem
  
--RST XX
callRoutine :: forall e. Boolean -> I16 -> Mem -> Eff (ma :: MemAccess | e) Mem
callRoutine fromIntrr addr mem@{mainMem, regs, svdRegs} = do
  let svdRegs' = saveRegs regs svdRegs
      retAddr = if fromIntrr then (pc regs) else (pc regs) + 1
  setPC addr regs
  setSP (sp regs - 2) regs
  mem { mainMem = _, svdRegs = svdRegs' }
    <$> wr16 retAddr (sp regs) mainMem

-- Misc.
-- =====

--SWAP R
swapReg :: forall e. SetReg -> GetReg -> Regs -> Eff (ma :: MemAccess | e) Regs
swapReg setReg getReg regs = setReg reg' =<< setF f' regs
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
clearCarryFlag :: forall e. Regs -> Eff (ma :: MemAccess | e) Regs
clearCarryFlag = changeCarryFlag false

--SCF
setCarryFlag :: forall e. Regs -> Eff (ma :: MemAccess | e) Regs
setCarryFlag = changeCarryFlag true

changeCarryFlag :: forall e. Boolean -> Regs -> Eff (ma :: MemAccess | e) Regs
changeCarryFlag isSet regs = setF f' regs
 where f' =  unsetFlag subtractionFlag
         <<< unsetFlag halfCarryFlag
         <<< (if isSet then setFlag else flipFlag) carryFlag
          $  (f regs)

--DAA
adjAForBCDAdd :: forall e. Regs -> Eff (ma :: MemAccess | e) Regs
adjAForBCDAdd regs = setF f' =<< setA (0xFF .&. a') regs
 where
  f' = setFlagCond (0x100 .&. a' /= 0) carryFlag
     $ setZeroFlag (0xFF .&. a')
     $ subtractionFlag .&. (f regs) 

  a' = adj.highNyb <<< adj.lowNyb $ (a regs)
  adj = if (f regs) .&. subtractionFlag /= 0
    then
      { lowNyb  : if ((f regs) .&. halfCarryFlag /= 0)
                    then  (0xFF .&. _) <<< (_ - 6) else id
      , highNyb : if ((f regs) .&. carryFlag /= 0)
                    then (_ - 0x60) else id
      }
    else
      { lowNyb  : if ((f regs) .&. halfCarryFlag /= 0) || (0x0F .&. (a regs) > 9)
                    then (6 + _) else id
      , highNyb : if ((f regs) .&. carryFlag /= 0) || ((a regs) > 0x9F)
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
  opCode <- rd8 ((pc state.mem.regs)+1) state.mem.mainMem
  let extOp = getCpuOp opCode opsMap 
  state' <- extOp state
  setPC (65535 .&. ((pc state'.mem.regs) + 2)) state'.mem.regs
  return state'

-- Helpers
-- =======

saveRegs :: Regs -> SavedRegs -> SavedRegs
saveRegs regs svdRegs =
  svdRegs { a = a regs, b = b regs, c = c regs, d = d regs
          , e = e regs, f = f regs, h = h regs, l = l regs
          }

restoreRegs :: forall e. SavedRegs -> Regs -> Eff (ma :: MemAccess | e) Regs
restoreRegs svdRegs regs = do
  setA svdRegs.a regs
  setB svdRegs.b regs
  setC svdRegs.c regs
  setD svdRegs.d regs
  setE svdRegs.e regs
  setF svdRegs.f regs
  setH svdRegs.h regs
  setL svdRegs.l regs

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

adjFlag :: forall e. I8 -> Regs -> Eff (ma :: MemAccess | e) Regs
adjFlag flag regs = setF (setFlag flag (f regs)) regs

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
