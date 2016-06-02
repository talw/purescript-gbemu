module OpCodeMap where

import Prelude
import Control.Monad.Eff

import Types
import Ops
import Utils
import Regs


--Tried to use typeclasses, but was forced to turn all my type synonyms
--to 'newtypes' which will require writing a specialised function for each
--case to convert from the type synonyms to the newtypes which will beat
--the purpose of using a typeclass in the first place (lets me have function
--overload in this case).

ss2op :: forall e. (Z80State -> Z80State)
      -> Z80State -> Eff (ma :: MemAccess | e) Z80State
ss2op ss = return <<< ss

rr2op :: forall e. (Regs -> Regs)
      -> Z80State -> Eff (ma :: MemAccess | e) Z80State
rr2op rr state@{mem=mem@{regs}} =
  return $ state { mem = mem { regs = rr regs } }

mr2op :: forall e. (Mem -> Regs)
      -> Z80State -> Eff (ma :: MemAccess | e) Z80State
mr2op mr state@{mem=mem@{regs}} =
  return $ state { mem = mem { regs = mr mem } }

mm2op :: forall e. (Mem -> Mem)
      -> Z80State -> Eff (ma :: MemAccess | e) Z80State
mm2op mm state@{mem=mem@{regs}} =
  return $ state { mem = mm mem }

rre2op :: forall e. (Regs -> Eff (ma :: MemAccess | e) Regs)
      -> Z80State -> Eff (ma :: MemAccess | e) Z80State
rre2op rr state@{mem=mem@{regs}} = do
  regs' <- rr regs
  return state { mem = mem { regs = regs' } }

mre2op :: forall e. (Mem -> Eff (ma :: MemAccess | e) Regs)
      -> Z80State -> Eff (ma :: MemAccess | e) Z80State
mre2op mr state@{mem=mem@{regs}} = do
  regs' <- mr mem
  return state { mem = mem { regs = regs' } }

mme2op :: forall e. (Mem -> Eff (ma :: MemAccess | e) Mem)
      -> Z80State -> Eff (ma :: MemAccess | e) Z80State
mme2op mm state@{mem=mem@{regs}} =
  state { mem = _ } <$> mm mem

basicOpTimings :: Array Int
basicOpTimings = 
  [ 1, 3, 2, 2, 1, 1, 2, 1, 5, 2, 2, 2, 1, 1, 2, 1
  , 1, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2, 1
  , 2, 3, 2, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 2, 1
  , 2, 3, 2, 2, 3, 3, 3, 1, 2, 2, 2, 2, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 2, 3, 3, 4, 3, 4, 2, 4, 2, 4, 3, 0, 3, 6, 2, 4
  , 2, 3, 3, 0, 3, 4, 2, 4, 2, 4, 3, 0, 3, 0, 2, 4
  , 3, 3, 2, 0, 0, 4, 2, 4, 4, 1, 4, 0, 0, 0, 2, 4
  , 3, 3, 2, 1, 0, 4, 2, 4, 3, 2, 4, 1, 0, 0, 2, 4
  ]

branchBasicOpTimings :: Array Int
branchBasicOpTimings = 
  [ 1, 3, 2, 2, 1, 1, 2, 1, 5, 2, 2, 2, 1, 1, 2, 1
  , 1, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2, 1
  , 3, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2, 1
  , 3, 3, 2, 2, 3, 3, 3, 1, 3, 2, 2, 2, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1
  , 5, 3, 4, 4, 6, 4, 2, 4, 5, 4, 4, 0, 6, 6, 2, 4
  , 5, 3, 4, 0, 6, 4, 2, 4, 5, 4, 4, 0, 6, 0, 2, 4
  , 3, 3, 2, 0, 0, 4, 2, 4, 4, 1, 4, 0, 0, 0, 2, 4
  , 3, 3, 2, 1, 0, 4, 2, 4, 3, 2, 4, 1, 0, 0, 2, 4
]

cbOpTimings :: Array Int
cbOpTimings = 
  [ 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2
  , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2
  , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2
  , 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2
  , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  , 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
  ]

basicOps :: forall e. Array (Z80State -> Eff (ma :: MemAccess | e) Z80State)
basicOps = 
                                             -- 0x
  [ rr2op $ nop                              -- NOP
  , mre2op $ ldTwoRegsFromImm setB setC      -- LD BC,nn
  , mme2op $ ldMem2RFromReg b c a            -- LD (BC),A
  , rre2op $ incRegWithCarry b c setB setC    -- INC BC

  , rre2op $ incReg b setB                    -- INC B
  , rre2op $ decReg b setB                    -- DEC B
  , mre2op $ ldRegFromImm setB               -- LD B,n
  , rre2op $ rotA LeftD true                  -- RLC A

  , mme2op $ ldMemImmFromSP                  -- LD (nn),SP
  , rre2op $ add2RegsToHL b c                 -- ADD HL,BC
  , mre2op $ ldRegFromMem2R setA b c         -- LD A,(BC)
  , rre2op $ decRegWithCarry b c setB setC    -- DEC BC

  , rre2op $ incReg c setC                    -- INC C
  , rre2op $ decReg c setC                    -- DEC C
  , mre2op $ ldRegFromImm setC               -- LD C,n
  , rre2op $ rotA RightD true                 -- RRC A

                                             -- 1x
  , ss2op $ stop                             -- STOP
  , mre2op $ ldTwoRegsFromImm setD setE      -- LD DE,nn
  , mme2op $ ldMem2RFromReg d e a            -- LD (DE),A
  , rre2op $ incRegWithCarry d e setD setE    -- INC DE

  , rre2op $ incReg d setD                    -- INC D
  , rre2op $ decReg d setD                    -- DEC D
  , mre2op $ ldRegFromImm setD               -- LD D,n
  , rre2op $ rotA LeftD false                 -- RL A

  , mre2op $ jumpRelImm                      -- JR n
  , rre2op $ add2RegsToHL d e                 -- ADD HL,DE
  , mre2op $ ldRegFromMem2R setA d e         -- LD A,(DE)
  , rre2op $ decRegWithCarry d e setD setE    -- DEC DE

  , rre2op $ incReg e setE                    -- INC E
  , rre2op $ decReg e setE                    -- DEC E
  , mre2op $ ldRegFromImm setE               -- LD E,n
  , rre2op $ rotA RightD false                -- RR A

                                             -- 2x
  , mre2op $ jumpRelImmFlag true zeroFlag    -- JR NZ,n
  , mre2op $ ldTwoRegsFromImm setH setL      -- LD HL,nn
  , mme2op $ ldMemHLFromRegInc a             -- LDI (HL),A
  , rre2op $ incRegWithCarry h l setH setL    -- INC HL

  , rre2op $ incReg h setH                    -- INC H
  , rre2op $ decReg h setH                    -- DEC H
  , mre2op $ ldRegFromImm setH               -- LD H,n
  , rre2op $ adjAForBCDAdd                    -- DAA

  , mre2op $ jumpRelImmFlag false zeroFlag   -- JR Z,n
  , rre2op $ add2RegsToHL h l                 -- ADD HL,HL
  , mre2op $ ldRegFromMemHLInc setA          -- LDI A,(HL)
  , rre2op $ decRegWithCarry h l setH setL    -- DEC HL

  , rre2op $ incReg l setL                    -- INC L
  , rre2op $ decReg l setL                    -- DEC L
  , mre2op $ ldRegFromImm setL               -- LD L,n
  , rre2op $ cmplA                            -- CPL

                                             -- 3x
  , mre2op $ jumpRelImmFlag true carryFlag   -- JR NC,n
  , mre2op $ ldSPFromImmMem                  -- LD SP,nn
  , mme2op $ ldMemHLFromRegDec a             -- LDD (HL),A
  , rre2op $ incSP                            -- INC SP

  , mme2op $ incHLMem                        -- INC (HL)
  , mme2op $ decHLMem                        -- DEC (HL)
  , mme2op $ ldMemHLFromImm                  -- LD (HL),n
  , rre2op $ setCarryFlag                     -- SCF

  , mre2op $ jumpRelImmFlag false carryFlag  -- JR C,n
  , rre2op $ addSPToHL                        -- ADD HL,SP
  , mre2op $ ldRegFromMemHLDec setA          -- LDD A,(HL)
  , rre2op $ decSP                            -- DEC SP

  , rre2op $ incReg a setA                    -- INC A
  , rre2op $ decReg a setA                    -- DEC A
  , mre2op $ ldRegFromImm setA               -- LD A,n
  , rre2op $ clearCarryFlag                   -- CCF

                                             -- 4x
  , rre2op $ ldRegFromReg setB b              -- LD B,B
  , rre2op $ ldRegFromReg setB c              -- LD B,C
  , rre2op $ ldRegFromReg setB d              -- LD B,D
  , rre2op $ ldRegFromReg setB e              -- LD B,E

  , rre2op $ ldRegFromReg setB h              -- LD B,H
  , rre2op $ ldRegFromReg setB l              -- LD B,L
  , mre2op $ ldRegFromMem2R setB h l         -- LD B,(HL)
  , rre2op $ ldRegFromReg setB a              -- LD B,A

  , rre2op $ ldRegFromReg setC b              -- LD C,B
  , rre2op $ ldRegFromReg setC c              -- LD C,C
  , rre2op $ ldRegFromReg setC d              -- LD C,D
  , rre2op $ ldRegFromReg setC e              -- LD C,E

  , rre2op $ ldRegFromReg setC h              -- LD C,H
  , rre2op $ ldRegFromReg setC l              -- LD C,L
  , mre2op $ ldRegFromMem2R setC h l         -- LD C,(HL)
  , rre2op $ ldRegFromReg setC a              -- LD C,A

                                             -- 5x
  , rre2op $ ldRegFromReg setD b              -- LD D,B
  , rre2op $ ldRegFromReg setD c              -- LD D,C
  , rre2op $ ldRegFromReg setD d              -- LD D,D
  , rre2op $ ldRegFromReg setD e              -- LD D,E

  , rre2op $ ldRegFromReg setD h              -- LD D,H
  , rre2op $ ldRegFromReg setD l              -- LD D,L
  , mre2op $ ldRegFromMem2R setD h l         -- LD D,(HL)
  , rre2op $ ldRegFromReg setD a              -- LD D,A

  , rre2op $ ldRegFromReg setE b              -- LD E,B
  , rre2op $ ldRegFromReg setE c              -- LD E,C
  , rre2op $ ldRegFromReg setE d              -- LD E,D
  , rre2op $ ldRegFromReg setE e              -- LD E,E

  , rre2op $ ldRegFromReg setE h              -- LD E,H
  , rre2op $ ldRegFromReg setE l              -- LD E,L
  , mre2op $ ldRegFromMem2R setE h l         -- LD E,(HL)
  , rre2op $ ldRegFromReg setE a              -- LD E,A

                                             -- 6x
  , rre2op $ ldRegFromReg setH b              -- LD H,B
  , rre2op $ ldRegFromReg setH c              -- LD H,C
  , rre2op $ ldRegFromReg setH d              -- LD H,D
  , rre2op $ ldRegFromReg setH e              -- LD H,E

  , rre2op $ ldRegFromReg setH h              -- LD H,H
  , rre2op $ ldRegFromReg setH l              -- LD H,L
  , mre2op $ ldRegFromMem2R setH h l         -- LD H,(HL)
  , rre2op $ ldRegFromReg setH a              -- LD H,A

  , rre2op $ ldRegFromReg setL b              -- LD L,B
  , rre2op $ ldRegFromReg setL c              -- LD L,C
  , rre2op $ ldRegFromReg setL d              -- LD L,D
  , rre2op $ ldRegFromReg setL e              -- LD L,E

  , rre2op $ ldRegFromReg setL h              -- LD L,H
  , rre2op $ ldRegFromReg setL l              -- LD L,L
  , mre2op $ ldRegFromMem2R setL h l         -- LD L,(HL)
  , rre2op $ ldRegFromReg setL a              -- LD L,A

                                             -- 7x
  , mme2op $ ldMem2RFromReg h l b            -- LD (HL),B
  , mme2op $ ldMem2RFromReg h l c            -- LD (HL),C
  , mme2op $ ldMem2RFromReg h l d            -- LD (HL),D
  , mme2op $ ldMem2RFromReg h l e            -- LD (HL),E

  , mme2op $ ldMem2RFromReg h l h            -- LD (HL),H
  , mme2op $ ldMem2RFromReg h l l            -- LD (HL),L
  , ss2op $ halt                             -- HALT
  , mme2op $ ldMem2RFromReg h l a            -- LD (HL),A

  , rre2op $ ldRegFromReg setA b              -- LD A,B
  , rre2op $ ldRegFromReg setA c              -- LD A,C
  , rre2op $ ldRegFromReg setA d              -- LD A,D
  , rre2op $ ldRegFromReg setA e              -- LD A,E

  , rre2op $ ldRegFromReg setA h              -- LD A,H
  , rre2op $ ldRegFromReg setA l              -- LD A,L
  , mre2op $ ldRegFromMem2R setA h l         -- LD A,(HL)
  , rre2op $ ldRegFromReg setA a              -- LD A,A

                                             -- 8x
  , rre2op $ addRegToA b                      -- ADD A,B
  , rre2op $ addRegToA c                      -- ADD A,C
  , rre2op $ addRegToA d                      -- ADD A,D
  , rre2op $ addRegToA e                      -- ADD A,E

  , rre2op $ addRegToA h                      -- ADD A,H
  , rre2op $ addRegToA l                      -- ADD A,L
  , mre2op $ addHLMemToA                     -- ADD A,(HL)
  , rre2op $ addRegToA a                      -- ADD A,A

  , rre2op $ addRegCarryToA b                 -- ADC A,B
  , rre2op $ addRegCarryToA c                 -- ADC A,C
  , rre2op $ addRegCarryToA d                 -- ADC A,D
  , rre2op $ addRegCarryToA e                 -- ADC A,E

  , rre2op $ addRegCarryToA h                 -- ADC A,H
  , rre2op $ addRegCarryToA l                 -- ADC A,L
  , mre2op $ addHLMemCarryToA                -- ADC A,(HL)
  , rre2op $ addRegCarryToA a                 -- ADC A,A

                                             -- 9x
  , rre2op $ subRegFromA b                    -- SUB A,B
  , rre2op $ subRegFromA c                    -- SUB A,C
  , rre2op $ subRegFromA d                    -- SUB A,D
  , rre2op $ subRegFromA e                    -- SUB A,E

  , rre2op $ subRegFromA h                    -- SUB A,H
  , rre2op $ subRegFromA l                    -- SUB A,L
  , mre2op $ subHLMemFromA                   -- SUB A,(HL)
  , rre2op $ subRegFromA a                    -- SUB A,A

  , rre2op $ subRegCarryFromA b               -- SBC A,B
  , rre2op $ subRegCarryFromA c               -- SBC A,C
  , rre2op $ subRegCarryFromA d               -- SBC A,D
  , rre2op $ subRegCarryFromA e               -- SBC A,E

  , rre2op $ subRegCarryFromA h               -- SBC A,H
  , rre2op $ subRegCarryFromA l               -- SBC A,L
  , mre2op $ subHLMemCarryFromA              -- SBC A,(HL)
  , rre2op $ subRegCarryFromA a               -- SBC A,A

                                             -- Ax
  , rre2op $ andOpRegIntoA b                  -- AND B
  , rre2op $ andOpRegIntoA c                  -- AND C
  , rre2op $ andOpRegIntoA d                  -- AND D
  , rre2op $ andOpRegIntoA e                  -- AND E

  , rre2op $ andOpRegIntoA h                  -- AND H
  , rre2op $ andOpRegIntoA l                  -- AND L
  , mre2op $ andOpHLMemIntoA                 -- AND (HL)
  , rre2op $ andOpRegIntoA a                  -- AND A

  , rre2op $ xorOpRegIntoA b                  -- XOR B
  , rre2op $ xorOpRegIntoA c                  -- XOR C
  , rre2op $ xorOpRegIntoA d                  -- XOR D
  , rre2op $ xorOpRegIntoA e                  -- XOR E

  , rre2op $ xorOpRegIntoA h                  -- XOR H
  , rre2op $ xorOpRegIntoA l                  -- XOR L
  , mre2op $ xorOpHLMemIntoA                 -- XOR (HL)
  , rre2op $ xorOpRegIntoA a                  -- XOR A

                                             -- Bx
  , rre2op $ orOpRegIntoA b                   -- OR B
  , rre2op $ orOpRegIntoA c                   -- OR C
  , rre2op $ orOpRegIntoA d                   -- OR D
  , rre2op $ orOpRegIntoA e                   -- OR E

  , rre2op $ orOpRegIntoA h                   -- OR H
  , rre2op $ orOpRegIntoA l                   -- OR L
  , mre2op $ orOpHLMemIntoA                  -- OR (HL)
  , rre2op $ orOpRegIntoA a                   -- OR A

  , rre2op $ compAToReg b                     -- CP B
  , rre2op $ compAToReg c                     -- CP C
  , rre2op $ compAToReg d                     -- CP D
  , rre2op $ compAToReg e                     -- CP E

  , rre2op $ compAToReg h                     -- CP H
  , rre2op $ compAToReg l                     -- CP L
  , mre2op $ compAToMemHL                    -- CP(HL)
  , rre2op $ compAToReg a                     -- CP A

                                             -- Cx
  , mre2op $ retFlag true zeroFlag           -- RET NZ
  , mre2op $ popReg setB setC                -- POP BC
  , mre2op $ jumpImmFlag true zeroFlag       -- JP NZ,nn
  , mre2op $ jumpImm                         -- JP nn

  , mme2op $ callImmFlag true zeroFlag       -- CALL NZ,nn
  , mme2op $ pushReg b c                     -- PUSH BC
  , mre2op $ addImmToA                       -- ADD A,n
  , mme2op $ callRoutine false 0x00          -- RST 0

  , mre2op $ retFlag false zeroFlag          -- RET Z
  , mre2op $ ret                             -- RET
  , mre2op $ jumpImmFlag false zeroFlag      -- JP Z,nn
  , execExtOps extOps                        -- CB OpCode map

  , mme2op $ callImmFlag false zeroFlag      -- CALL Z,nn
  , mme2op $ callImm                         -- CALL nn
  , mre2op $ addImmCarryToA                  -- ADC A,n
  , mme2op $ callRoutine false 0x08          -- RST 8

                                             -- Dx
  , mre2op $ retFlag true carryFlag          -- RET NC
  , mre2op $ popReg setD setE                -- POP DE
  , mre2op $ jumpImmFlag true carryFlag      -- JP NC,nn
  , ss2op $ invalidOpCode

  , mme2op $ callImmFlag true carryFlag      -- CALL NC,nn
  , mme2op $ pushReg d e                     -- PUSH DE
  , mre2op $ subImmFromA                     -- SUB A,n
  , mme2op $ callRoutine false 0x10          -- RST 10

  , mre2op $ retFlag false carryFlag         -- RET C
  , mme2op $ retEnableInterrupt              -- RETI
  , mre2op $ jumpImmFlag false carryFlag     -- JP C,nn
  , ss2op $ invalidOpCode

  , mme2op $ callImmFlag true carryFlag      -- CALL C,nn
  , ss2op $ invalidOpCode
  , mre2op subImmCarryToA                    -- SBC A,n
  , mme2op $ callRoutine false 0x18          -- RST 18

                                             -- Ex
  , mme2op $ ldFF00ImmMemFromReg             -- LDH (n),A
  , mre2op $ popReg setH setL                -- POP HL
  , mme2op $ ldFF00CMemFromReg a             -- LDH (C),A
  , ss2op $ invalidOpCode

  , ss2op $ invalidOpCode
  , mme2op $ pushReg h l                     -- PUSH HL
  , mre2op andOpImmIntoA                     -- AND n
  , mme2op $ callRoutine false 0x20          -- RST 20

  , mre2op addImmToSP                        -- ADD SP,d
  , rre2op jumpHL                             -- JP (HL)
  , mme2op ldMemImmFromRegA                  -- LD (nn),A
  , ss2op $ invalidOpCode

  , ss2op $ invalidOpCode
  , ss2op $ invalidOpCode
  , mre2op xorOpImmIntoA                     -- XOR n
  , mme2op $ callRoutine false 0x28          -- RST 28

                                             -- Fx
  , mre2op $ ldRegFromFF00ImmMem             -- LDH A,(n)
  , mre2op $ popReg setA setF                -- POP AF
  , mre2op $ ldRegFromFF00CMem setA          -- LD A,(IOC)
  , mm2op $ setInterrupts false              -- DI

  , ss2op $ invalidOpCode
  , mme2op $ pushReg a f                     -- PUSH AF
  , mre2op orOpImmIntoA                      -- OR n
  , mme2op $ callRoutine false 0x30          -- RST 30

  , mre2op ldHLFromSPImm                     -- LDHL SP,d
  , rre2op ldSPFromHL                         -- LD SP,HL
  , mre2op ldRegAFromMemImm                  -- LD A,(nn)
  , mm2op $ setInterrupts true               -- EI

  , ss2op $ invalidOpCode
  , ss2op $ invalidOpCode
  , mre2op compAToImm                        -- CP n
  , mme2op $ callRoutine false 0x38          -- RST 38
]

extOps :: forall e. Array (Z80State -> Eff (ma :: MemAccess | e) Z80State)
extOps = 
                                           -- 0x
  [ rre2op $ rotReg LeftD true setB b       -- RLC B
  , rre2op $ rotReg LeftD true setC c       -- RLC C
  , rre2op $ rotReg LeftD true setD d       -- RLC D
  , rre2op $ rotReg LeftD true setE e       -- RLC E

  , rre2op $ rotReg LeftD true setH h       -- RLC H
  , rre2op $ rotReg LeftD true setL l       -- RLC L
  , mme2op $ rotHLMem LeftD true           -- RLC (HL)
  , rre2op $ rotReg LeftD true setA a       -- RLC A

  , rre2op $ rotReg RightD true setB b      -- RRC B
  , rre2op $ rotReg RightD true setC c      -- RRC C
  , rre2op $ rotReg RightD true setD d      -- RRC D
  , rre2op $ rotReg RightD true setE e      -- RRC E

  , rre2op $ rotReg RightD true setH h      -- RRC H
  , rre2op $ rotReg RightD true setL l      -- RRC L
  , mme2op $ rotHLMem RightD true          -- RRC (HL)
  , rre2op $ rotReg RightD true setA a      -- RRC A

                                           -- 1x
  , rre2op $ rotReg LeftD false setB b      -- RL B
  , rre2op $ rotReg LeftD false setC c      -- RL C
  , rre2op $ rotReg LeftD false setD d      -- RL D
  , rre2op $ rotReg LeftD false setE e      -- RL E

  , rre2op $ rotReg LeftD false setH h      -- RL H
  , rre2op $ rotReg LeftD false setL l      -- RL L
  , mme2op $ rotHLMem LeftD false          -- RL (HL)
  , rre2op $ rotReg LeftD false setA a      -- RL A

  , rre2op $ rotReg RightD false setB b     -- RR B
  , rre2op $ rotReg RightD false setC c     -- RR C
  , rre2op $ rotReg RightD false setD d     -- RR D
  , rre2op $ rotReg RightD false setE e     -- RR E

  , rre2op $ rotReg RightD false setH h     -- RR H
  , rre2op $ rotReg RightD false setL l     -- RR L
  , mme2op $ rotHLMem RightD false         -- RR(HL)
  , rre2op $ rotReg RightD false setA a     -- RR A

                                           -- 2x
  , rre2op $ shiftReg LeftD false setB b    -- SLA B
  , rre2op $ shiftReg LeftD false setC c    -- SLA C
  , rre2op $ shiftReg LeftD false setD d    -- SLA D
  , rre2op $ shiftReg LeftD false setE e    -- SLA E

  , rre2op $ shiftReg LeftD false setH h    -- SLA H
  , rre2op $ shiftReg LeftD false setL l    -- SLA L
  , mme2op $ shiftMemHL LeftD false        -- SLA(HL)
  , rre2op $ shiftReg LeftD false setA a    -- SLA A

  , rre2op $ shiftReg RightD true setB b    -- SRA B
  , rre2op $ shiftReg RightD true setC c    -- SRA C
  , rre2op $ shiftReg RightD true setD d    -- SRA D
  , rre2op $ shiftReg RightD true setE e    -- SRA E

  , rre2op $ shiftReg RightD true setH h    -- SRA H
  , rre2op $ shiftReg RightD true setL l    -- SRA L
  , mme2op $ shiftMemHL RightD true        -- SRA (HL)
  , rre2op $ shiftReg RightD true setA a    -- SRA A

                                           -- 3x
  , rre2op $ swapReg setB b                 -- SWAP B
  , rre2op $ swapReg setC c                 -- SWAP C
  , rre2op $ swapReg setD d                 -- SWAP D
  , rre2op $ swapReg setE e                 -- SWAP E

  , rre2op $ swapReg setH h                 -- SWAP H
  , rre2op $ swapReg setL l                 -- SWAP L
  , mme2op $ swapMemHL                     -- SWAP(HL)
  , rre2op $ swapReg setA a                 -- SWAP A

  , rre2op $ shiftReg RightD false setB b   -- SRL B
  , rre2op $ shiftReg RightD false setC c   -- SRL C
  , rre2op $ shiftReg RightD false setD d   -- SRL D
  , rre2op $ shiftReg RightD false setE e   -- SRL E

  , rre2op $ shiftReg RightD false setH h   -- SRL H
  , rre2op $ shiftReg RightD false setL l   -- SRL L
  , mme2op $ shiftMemHL RightD false       -- SRL (HL)
  , rre2op $ shiftReg RightD false setA a   -- SRL A

                                           -- 4x
  , rre2op $ testBitNOfReg 0 b              -- BIT 0,B
  , rre2op $ testBitNOfReg 0 c              -- BIT 0,C
  , rre2op $ testBitNOfReg 0 d              -- BIT 0,D
  , rre2op $ testBitNOfReg 0 e              -- BIT 0,E

  , rre2op $ testBitNOfReg 0 h              -- BIT 0,H
  , rre2op $ testBitNOfReg 0 l              -- BIT 0,L
  , mre2op $ testBitNOfHLMem 0             -- BIT 0,(HL)
  , rre2op $ testBitNOfReg 0 a              -- BIT 0,A

  , rre2op $ testBitNOfReg 1 b              -- BIT 1,B
  , rre2op $ testBitNOfReg 1 c              -- BIT 1,C
  , rre2op $ testBitNOfReg 1 d              -- BIT 1,D
  , rre2op $ testBitNOfReg 1 e              -- BIT 1,E

  , rre2op $ testBitNOfReg 1 h              -- BIT 1,H
  , rre2op $ testBitNOfReg 1 l              -- BIT 1,L
  , mre2op $ testBitNOfHLMem 1             -- BIT 1,(HL)
  , rre2op $ testBitNOfReg 1 a              -- BIT 1,A

                                           -- 5x
  , rre2op $ testBitNOfReg 2 b              -- BIT 2,B
  , rre2op $ testBitNOfReg 2 c              -- BIT 2,C
  , rre2op $ testBitNOfReg 2 d              -- BIT 2,D
  , rre2op $ testBitNOfReg 2 e              -- BIT 2,E

  , rre2op $ testBitNOfReg 2 h              -- BIT 2,H
  , rre2op $ testBitNOfReg 2 l              -- BIT 2,L
  , mre2op $ testBitNOfHLMem 2             -- BIT 2,(HL)
  , rre2op $ testBitNOfReg 2 a              -- BIT 2,A

  , rre2op $ testBitNOfReg 3 b              -- BIT 3,B
  , rre2op $ testBitNOfReg 3 c              -- BIT 3,C
  , rre2op $ testBitNOfReg 3 d              -- BIT 3,D
  , rre2op $ testBitNOfReg 3 e              -- BIT 3,E

  , rre2op $ testBitNOfReg 3 h              -- BIT 3,H
  , rre2op $ testBitNOfReg 3 l              -- BIT 3,L
  , mre2op $ testBitNOfHLMem 3             -- BIT 3,(HL)
  , rre2op $ testBitNOfReg 3 a              -- BIT 3,A

                                           -- 6x
  , rre2op $ testBitNOfReg 4 b              -- BIT 4,B
  , rre2op $ testBitNOfReg 4 c              -- BIT 4,C
  , rre2op $ testBitNOfReg 4 d              -- BIT 4,D
  , rre2op $ testBitNOfReg 4 e              -- BIT 4,E

  , rre2op $ testBitNOfReg 4 h              -- BIT 4,H
  , rre2op $ testBitNOfReg 4 l              -- BIT 4,L
  , mre2op $ testBitNOfHLMem 4             -- BIT 4,(HL)
  , rre2op $ testBitNOfReg 4 a              -- BIT 4,A

  , rre2op $ testBitNOfReg 5 b              -- BIT 5,B
  , rre2op $ testBitNOfReg 5 c              -- BIT 5,C
  , rre2op $ testBitNOfReg 5 d              -- BIT 5,D
  , rre2op $ testBitNOfReg 5 e              -- BIT 5,E

  , rre2op $ testBitNOfReg 5 h              -- BIT 5,H
  , rre2op $ testBitNOfReg 5 l              -- BIT 5,L
  , mre2op $ testBitNOfHLMem 5             -- BIT 5,(HL)
  , rre2op $ testBitNOfReg 5 a              -- BIT 5,A

                                           -- 7x
  , rre2op $ testBitNOfReg 6 b              -- BIT 6,B
  , rre2op $ testBitNOfReg 6 c              -- BIT 6,C
  , rre2op $ testBitNOfReg 6 d              -- BIT 6,D
  , rre2op $ testBitNOfReg 6 e              -- BIT 6,E

  , rre2op $ testBitNOfReg 6 h              -- BIT 6,H
  , rre2op $ testBitNOfReg 6 l              -- BIT 6,L
  , mre2op $ testBitNOfHLMem 6             -- BIT 6,(HL)
  , rre2op $ testBitNOfReg 6 a              -- BIT 6,A

  , rre2op $ testBitNOfReg 7 b              -- BIT 7,B
  , rre2op $ testBitNOfReg 7 c              -- BIT 7,C
  , rre2op $ testBitNOfReg 7 d              -- BIT 7,D
  , rre2op $ testBitNOfReg 7 e              -- BIT 7,E

  , rre2op $ testBitNOfReg 7 h              -- BIT 7,H
  , rre2op $ testBitNOfReg 7 l              -- BIT 7,L
  , mre2op $ testBitNOfHLMem 7             -- BIT 7,(HL)
  , rre2op $ testBitNOfReg 7 a              -- BIT 7,A

                                           -- 8x
  , rre2op $ setBitNOfReg false 0 setB b    -- RES 0,B
  , rre2op $ setBitNOfReg false 0 setC c    -- RES 0,C
  , rre2op $ setBitNOfReg false 0 setD d    -- RES 0,D
  , rre2op $ setBitNOfReg false 0 setE e    -- RES 0,E

  , rre2op $ setBitNOfReg false 0 setH h    -- RES 0,H
  , rre2op $ setBitNOfReg false 0 setL l    -- RES 0,L
  , mme2op $ setBitNOfHLMem false 0        -- RES 0,(HL)
  , rre2op $ setBitNOfReg false 0 setA a    -- RES 0,A

  , rre2op $ setBitNOfReg false 1 setB b    -- RES 1,B
  , rre2op $ setBitNOfReg false 1 setC c    -- RES 1,C
  , rre2op $ setBitNOfReg false 1 setD d    -- RES 1,D
  , rre2op $ setBitNOfReg false 1 setE e    -- RES 1,E

  , rre2op $ setBitNOfReg false 1 setH h    -- RES 1,H
  , rre2op $ setBitNOfReg false 1 setL l    -- RES 1,L
  , mme2op $ setBitNOfHLMem false 1        -- RES 1,(HL)
  , rre2op $ setBitNOfReg false 1 setA a    -- RES 1,A

                                           -- 9x
  , rre2op $ setBitNOfReg false 2 setB b    -- RES 2,B
  , rre2op $ setBitNOfReg false 2 setC c    -- RES 2,C
  , rre2op $ setBitNOfReg false 2 setD d    -- RES 2,D
  , rre2op $ setBitNOfReg false 2 setE e    -- RES 2,E

  , rre2op $ setBitNOfReg false 2 setH h    -- RES 2,H
  , rre2op $ setBitNOfReg false 2 setL l    -- RES 2,L
  , mme2op $ setBitNOfHLMem false 2        -- RES 2,(HL)
  , rre2op $ setBitNOfReg false 2 setA a    -- RES 2,A

  , rre2op $ setBitNOfReg false 3 setB b    -- RES 3,B
  , rre2op $ setBitNOfReg false 3 setC c    -- RES 3,C
  , rre2op $ setBitNOfReg false 3 setD d    -- RES 3,D
  , rre2op $ setBitNOfReg false 3 setE e    -- RES 3,E

  , rre2op $ setBitNOfReg false 3 setH h    -- RES 3,H
  , rre2op $ setBitNOfReg false 3 setL l    -- RES 3,L
  , mme2op $ setBitNOfHLMem false 3        -- RES 3,(HL)
  , rre2op $ setBitNOfReg false 3 setA a    -- RES 3,A

                                           -- Ax
  , rre2op $ setBitNOfReg false 4 setB b    -- RES 4,B
  , rre2op $ setBitNOfReg false 4 setC c    -- RES 4,C
  , rre2op $ setBitNOfReg false 4 setD d    -- RES 4,D
  , rre2op $ setBitNOfReg false 4 setE e    -- RES 4,E

  , rre2op $ setBitNOfReg false 4 setH h    -- RES 4,H
  , rre2op $ setBitNOfReg false 4 setL l    -- RES 4,L
  , mme2op $ setBitNOfHLMem false 4        -- RES 4,(HL)
  , rre2op $ setBitNOfReg false 4 setA a    -- RES 4,A

  , rre2op $ setBitNOfReg false 5 setB b    -- RES 5,B
  , rre2op $ setBitNOfReg false 5 setC c    -- RES 5,C
  , rre2op $ setBitNOfReg false 5 setD d    -- RES 5,D
  , rre2op $ setBitNOfReg false 5 setE e    -- RES 5,E

  , rre2op $ setBitNOfReg false 5 setH h    -- RES 5,H
  , rre2op $ setBitNOfReg false 5 setL l    -- RES 5,L
  , mme2op $ setBitNOfHLMem false 5        -- RES 5,(HL)
  , rre2op $ setBitNOfReg false 5 setA a    -- RES 5,A

                                           -- Bx
  , rre2op $ setBitNOfReg false 6 setB b    -- RES 6,B
  , rre2op $ setBitNOfReg false 6 setC c    -- RES 6,C
  , rre2op $ setBitNOfReg false 6 setD d    -- RES 6,D
  , rre2op $ setBitNOfReg false 6 setE e    -- RES 6,E

  , rre2op $ setBitNOfReg false 6 setH h    -- RES 6,H
  , rre2op $ setBitNOfReg false 6 setL l    -- RES 6,L
  , mme2op $ setBitNOfHLMem false 6        -- RES 6,(HL)
  , rre2op $ setBitNOfReg false 6 setA a    -- RES 6,A

  , rre2op $ setBitNOfReg false 7 setB b    -- RES 7,B
  , rre2op $ setBitNOfReg false 7 setC c    -- RES 7,C
  , rre2op $ setBitNOfReg false 7 setD d    -- RES 7,D
  , rre2op $ setBitNOfReg false 7 setE e    -- RES 7,E

  , rre2op $ setBitNOfReg false 7 setH h    -- RES 7,H
  , rre2op $ setBitNOfReg false 7 setL l    -- RES 7,L
  , mme2op $ setBitNOfHLMem false 7        -- RES 7,(HL)
  , rre2op $ setBitNOfReg false 7 setA a    -- RES 7,A


                                           -- Cx
  , rre2op $ setBitNOfReg true 0 setB b     -- SET 0,B
  , rre2op $ setBitNOfReg true 0 setC c     -- SET 0,C
  , rre2op $ setBitNOfReg true 0 setD d     -- SET 0,D
  , rre2op $ setBitNOfReg true 0 setE e     -- SET 0,E

  , rre2op $ setBitNOfReg true 0 setH h     -- SET 0,H
  , rre2op $ setBitNOfReg true 0 setL l     -- SET 0,L
  , mme2op $ setBitNOfHLMem true 0         -- SET 0,(HL)
  , rre2op $ setBitNOfReg true 0 setA a     -- SET 0,A

  , rre2op $ setBitNOfReg true 1 setB b     -- SET 1,B
  , rre2op $ setBitNOfReg true 1 setC c     -- SET 1,C
  , rre2op $ setBitNOfReg true 1 setD d     -- SET 1,D
  , rre2op $ setBitNOfReg true 1 setE e     -- SET 1,E

  , rre2op $ setBitNOfReg true 1 setH h     -- SET 1,H
  , rre2op $ setBitNOfReg true 1 setL l     -- SET 1,L
  , mme2op $ setBitNOfHLMem true 1         -- SET 1,(HL)
  , rre2op $ setBitNOfReg true 1 setA a     -- SET 1,A

                                           -- Dx
  , rre2op $ setBitNOfReg true 2 setB b     -- SET 2,B
  , rre2op $ setBitNOfReg true 2 setC c     -- SET 2,C
  , rre2op $ setBitNOfReg true 2 setD d     -- SET 2,D
  , rre2op $ setBitNOfReg true 2 setE e     -- SET 2,E

  , rre2op $ setBitNOfReg true 2 setH h     -- SET 2,H
  , rre2op $ setBitNOfReg true 2 setL l     -- SET 2,L
  , mme2op $ setBitNOfHLMem true 2         -- SET 2,(HL)
  , rre2op $ setBitNOfReg true 2 setA a     -- SET 2,A

  , rre2op $ setBitNOfReg true 3 setB b     -- SET 3,B
  , rre2op $ setBitNOfReg true 3 setC c     -- SET 3,C
  , rre2op $ setBitNOfReg true 3 setD d     -- SET 3,D
  , rre2op $ setBitNOfReg true 3 setE e     -- SET 3,E

  , rre2op $ setBitNOfReg true 3 setH h     -- SET 3,H
  , rre2op $ setBitNOfReg true 3 setL l     -- SET 3,L
  , mme2op $ setBitNOfHLMem true 3         -- SET 3,(HL)
  , rre2op $ setBitNOfReg true 3 setA a     -- SET 3,A

                                           -- Ex
  , rre2op $ setBitNOfReg true 4 setB b     -- SET 4,B
  , rre2op $ setBitNOfReg true 4 setC c     -- SET 4,C
  , rre2op $ setBitNOfReg true 4 setD d     -- SET 4,D
  , rre2op $ setBitNOfReg true 4 setE e     -- SET 4,E

  , rre2op $ setBitNOfReg true 4 setH h     -- SET 4,H
  , rre2op $ setBitNOfReg true 4 setL l     -- SET 4,L
  , mme2op $ setBitNOfHLMem true 4         -- SET 4,(HL)
  , rre2op $ setBitNOfReg true 4 setA a     -- SET 4,A

  , rre2op $ setBitNOfReg true 5 setB b     -- SET 5,B
  , rre2op $ setBitNOfReg true 5 setC c     -- SET 5,C
  , rre2op $ setBitNOfReg true 5 setD d     -- SET 5,D
  , rre2op $ setBitNOfReg true 5 setE e     -- SET 5,E

  , rre2op $ setBitNOfReg true 5 setH h     -- SET 5,H
  , rre2op $ setBitNOfReg true 5 setL l     -- SET 5,L
  , mme2op $ setBitNOfHLMem true 5         -- SET 5,(HL)
  , rre2op $ setBitNOfReg true 5 setA a     -- SET 5,A

                                           -- Fx
  , rre2op $ setBitNOfReg true 6 setB b     -- SET 6,B
  , rre2op $ setBitNOfReg true 6 setC c     -- SET 6,C
  , rre2op $ setBitNOfReg true 6 setD d     -- SET 6,D
  , rre2op $ setBitNOfReg true 6 setE e     -- SET 6,E

  , rre2op $ setBitNOfReg true 6 setH h     -- SET 6,H
  , rre2op $ setBitNOfReg true 6 setL l     -- SET 6,L
  , mme2op $ setBitNOfHLMem true 6         -- SET 6,(HL)
  , rre2op $ setBitNOfReg true 6 setA a     -- SET 6,A

  , rre2op $ setBitNOfReg true 7 setB b     -- SET 7,B
  , rre2op $ setBitNOfReg true 7 setC c     -- SET 7,C
  , rre2op $ setBitNOfReg true 7 setD d     -- SET 7,D
  , rre2op $ setBitNOfReg true 7 setE e     -- SET 7,E

  , rre2op $ setBitNOfReg true 7 setH h     -- SET 7,H
  , rre2op $ setBitNOfReg true 7 setL l     -- SET 7,L
  , mme2op $ setBitNOfHLMem true 7         -- SET 7,(HL)
  , rre2op $ setBitNOfReg true 7 setA a     -- SET 7,A
]
