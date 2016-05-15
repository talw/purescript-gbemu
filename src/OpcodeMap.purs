module OpCodeMap where

import Prelude

import Types
import Ops


--Tried to use typeclasses, but was forced to turn all my type synonyms
--to 'newtypes' which will require writing a specialised function for each
--case to convert from the type synonyms to the newtypes which will beat
--the purpose of using a typeclass in the first place (lets me have function
--overload in this case).
rr2op :: (Regs -> Regs) -> Z80State -> Z80State
rr2op rr state@{mem=mem@{regs}} = state { mem = mem { regs = rr regs } }

mr2op :: (Mem -> Regs) -> Z80State -> Z80State
mr2op mr state@{mem=mem@{regs}} = state { mem = mem { regs = mr mem } }

mm2op :: (Mem -> Mem) -> Z80State -> Z80State
mm2op mm state@{mem=mem@{regs}} = state { mem = mm mem }

basicOps :: Array (Z80State -> Z80State)
basicOps = 
                                            -- 0x
  [ rr2op $ nop                             -- NOP
  , mr2op $ ldTwoRegsFromImm setB setC      -- LD BC,nn
  , mm2op $ ldMem2RFromReg b c a            -- LD (BC),A
  , rr2op $ incRegWithCarry b c setB setC   -- INC BC

  , rr2op $ incReg b setB                   -- INC B
  , rr2op $ decReg b setB                   -- DEC B
  , mr2op $ ldRegFromImm setB               -- LD B,n
  , rr2op $ rotA Left true                  -- RLC A

  , mm2op $ ldMemImmFromSP                  -- LD (nn),SP
  , rr2op $ add2RegsToHL b c                -- ADD HL,BC
  , mr2op $ ldRegFromMem2R setA b c         -- LD A,(BC)
  , rr2op $ decRegWithCarry b c setB setC   -- DEC BC

  , rr2op $ incReg c setC                   -- INC C
  , rr2op $ decReg c setC                   -- DEC C
  , mr2op $ ldRegFromImm setC               -- LD C,n
  , rr2op $ rotA Right true                 -- RRC A

                                            -- 1x
  , stop                                    -- STOP
  , mr2op $ ldTwoRegsFromImm setD setE      -- LD DE,nn
  , mm2op $ ldMem2RFromReg d e a            -- LD (DE),A
  , rr2op $ incRegWithCarry d e setD setE   -- INC DE

  , rr2op $ incReg d setD                   -- INC D
  , rr2op $ decReg d setD                   -- DEC D
  , mr2op $ ldRegFromImm setD               -- LD D,n
  , rr2op $ rotA Left false                 -- RL A

  , mr2op $ jumpRelImm                      -- JR n
  , rr2op $ add2RegsToHL d e                -- ADD HL,DE
  , mr2op $ ldRegFromMem2R setA d e         -- LD A,(DE)
  , rr2op $ decRegWithCarry d e setD setE   -- DEC DE

  , rr2op $ incReg e setE                   -- INC E
  , rr2op $ decReg e setE                   -- DEC E
  , mr2op $ ldRegFromImm setE               -- LD E,n
  , rr2op $ rotA Right false                -- RR A

                                            -- 2x
  , mr2op $ jumpRelImmFlag true zeroFlag    -- JR NZ,n
  , mr2op $ ldTwoRegsFromImm setH setL      -- LD HL,nn
  , mm2op $ ldMemHLFromRegInc a             -- LDI (HL),A
  , rr2op $ incRegWithCarry h l setH setL   -- INC HL

  , rr2op $ incReg h setH                   -- INC H
  , rr2op $ decReg h setH                   -- DEC H
  , mr2op $ ldRegFromImm setH               -- LD H,n
  , rr2op $ adjAForBCDAdd                   -- DAA

  , mr2op $ jumpRelImmFlag false zeroFlag   -- JR Z,n
  , rr2op $ add2RegsToHL h l                -- ADD HL,HL
  , mr2op $ ldRegFromMemHLInc setA          -- LDI A,(HL)
  , rr2op $ decRegWithCarry h l setH setL   -- DEC HL

  , rr2op $ incReg l setL                   -- INC L
  , rr2op $ decReg l setL                   -- DEC L
  , mr2op $ ldRegFromImm setL               -- LD L,n
  , rr2op $ cmplA                           -- CPL

                                            -- 3x
  , mr2op $ jumpRelImmFlag true carryFlag   -- JR NC,n
  , mr2op $ ldSPFromImm                     -- LD SP,nn
  , mm2op $ ldMemHLFromRegDec a             -- LDD (HL),A
  , rr2op $ incSP                           -- INC SP

  , mm2op $ incHLMem                        -- INC (HL)
  , mm2op $ decHLMem                        -- DEC (HL)
  , mm2op $ ldMemHLFromImm                  -- LD (HL),n
  , rr2op $ setCarryFlag                    -- SCF

  , mr2op $ jumpRelImmFlag false carryFlag  -- JR C,n
  , rr2op $ addSPToHL                       -- ADD HL,SP
  , mr2op $ ldRegFromMemHLDec setA          -- LDD A,(HL)
  , rr2op $ decSP                           -- DEC SP

  , rr2op $ incReg a setA                   -- INC A
  , rr2op $ decReg a setA                   -- DEC A
  , mr2op $ ldRegFromImm setA               -- LD A,n
  , rr2op $ clearCarryFlag                  -- CCF

                                            -- 4x
  , rr2op $ ldRegFromReg setB b             -- LD B,B
  , rr2op $ ldRegFromReg setB c             -- LD B,C
  , rr2op $ ldRegFromReg setB d             -- LD B,D
  , rr2op $ ldRegFromReg setB e             -- LD B,E

  , rr2op $ ldRegFromReg setB h             -- LD B,H
  , rr2op $ ldRegFromReg setB l             -- LD B,L
  , mr2op $ ldRegFromMem2R setB h l         -- LD B,(HL)
  , rr2op $ ldRegFromReg setB a             -- LD B,A

  , rr2op $ ldRegFromReg setC b             -- LD C,B
  , rr2op $ ldRegFromReg setC c             -- LD C,C
  , rr2op $ ldRegFromReg setC d             -- LD C,D
  , rr2op $ ldRegFromReg setC e             -- LD C,E

  , rr2op $ ldRegFromReg setC h             -- LD C,H
  , rr2op $ ldRegFromReg setC l             -- LD C,L
  , mr2op $ ldRegFromMem2R setC h l         -- LD C,(HL)
  , rr2op $ ldRegFromReg setC a             -- LD C,A

                                            -- 5x
  , rr2op $ ldRegFromReg setD b             -- LD D,B
  , rr2op $ ldRegFromReg setD c             -- LD D,C
  , rr2op $ ldRegFromReg setD d             -- LD D,D
  , rr2op $ ldRegFromReg setD e             -- LD D,E

  , rr2op $ ldRegFromReg setD h             -- LD D,H
  , rr2op $ ldRegFromReg setD l             -- LD D,L
  , mr2op $ ldRegFromMem2R setD h l         -- LD D,(HL)
  , rr2op $ ldRegFromReg setD a             -- LD D,A

  , rr2op $ ldRegFromReg setE b             -- LD E,B
  , rr2op $ ldRegFromReg setE c             -- LD E,C
  , rr2op $ ldRegFromReg setE d             -- LD E,D
  , rr2op $ ldRegFromReg setE e             -- LD E,E

  , rr2op $ ldRegFromReg setE h             -- LD E,H
  , rr2op $ ldRegFromReg setE l             -- LD E,L
  , mr2op $ ldRegFromMem2R setE h l         -- LD E,(HL)
  , rr2op $ ldRegFromReg setE a             -- LD E,A

                                            -- 6x
  , rr2op $ ldRegFromReg setH b             -- LD H,B
  , rr2op $ ldRegFromReg setH c             -- LD H,C
  , rr2op $ ldRegFromReg setH d             -- LD H,D
  , rr2op $ ldRegFromReg setH e             -- LD H,E

  , rr2op $ ldRegFromReg setH h             -- LD H,H
  , rr2op $ ldRegFromReg setH l             -- LD H,L
  , mr2op $ ldRegFromMem2R setH h l         -- LD H,(HL)
  , rr2op $ ldRegFromReg setH a             -- LD H,A

  , rr2op $ ldRegFromReg setL b             -- LD L,B
  , rr2op $ ldRegFromReg setL c             -- LD L,C
  , rr2op $ ldRegFromReg setL d             -- LD L,D
  , rr2op $ ldRegFromReg setL e             -- LD L,E

  , rr2op $ ldRegFromReg setL h             -- LD L,H
  , rr2op $ ldRegFromReg setL l             -- LD L,L
  , mr2op $ ldRegFromMem2R setL h l         -- LD L,(HL)
  , rr2op $ ldRegFromReg setL a             -- LD L,A

                                            -- 7x
  , mm2op $ ldMem2RFromReg h l b            -- LD (HL),B
  , mm2op $ ldMem2RFromReg h l c            -- LD (HL),C
  , mm2op $ ldMem2RFromReg h l d            -- LD (HL),D
  , mm2op $ ldMem2RFromReg h l e            -- LD (HL),E

  , mm2op $ ldMem2RFromReg h l h            -- LD (HL),H
  , mm2op $ ldMem2RFromReg h l l            -- LD (HL),L
  , halt                                    -- HALT
  , mm2op $ ldMem2RFromReg h l a            -- LD (HL),A

  , rr2op $ ldRegFromReg setA b             -- LD A,B
  , rr2op $ ldRegFromReg setA c             -- LD A,C
  , rr2op $ ldRegFromReg setA d             -- LD A,D
  , rr2op $ ldRegFromReg setA e             -- LD A,E

  , rr2op $ ldRegFromReg setA h             -- LD A,H
  , rr2op $ ldRegFromReg setA l             -- LD A,L
  , mr2op $ ldRegFromMem2R setA h l         -- LD A,(HL)
  , rr2op $ ldRegFromReg setA a             -- LD A,A

                                            -- 8x
  , rr2op $ addRegToA b                     -- ADD A,B
  , rr2op $ addRegToA c                     -- ADD A,C
  , rr2op $ addRegToA d                     -- ADD A,D
  , rr2op $ addRegToA e                     -- ADD A,E

  , rr2op $ addRegToA h                     -- ADD A,H
  , rr2op $ addRegToA l                     -- ADD A,L
  , mr2op $ addHLMemToA                     -- ADD A,(HL)
  , rr2op $ addRegToA a                     -- ADD A,A

  , rr2op $ addRegCarryToA b                -- ADC A,B
  , rr2op $ addRegCarryToA c                -- ADC A,C
  , rr2op $ addRegCarryToA d                -- ADC A,D
  , rr2op $ addRegCarryToA e                -- ADC A,E

  , rr2op $ addRegCarryToA h                -- ADC A,H
  , rr2op $ addRegCarryToA l                -- ADC A,L
  , mr2op $ addHLMemCarryToA                -- ADC A,(HL)
  , rr2op $ addRegCarryToA a                -- ADC A,A

                                            -- 9x
  , rr2op $ subRegFromA b                   -- SUB A,B
  , rr2op $ subRegFromA c                   -- SUB A,C
  , rr2op $ subRegFromA d                   -- SUB A,D
  , rr2op $ subRegFromA e                   -- SUB A,E

  , rr2op $ subRegFromA h                   -- SUB A,H
  , rr2op $ subRegFromA l                   -- SUB A,L
  , mr2op $ subHLMemFromA                   -- SUB A,(HL)
  , rr2op $ subRegFromA a                   -- SUB A,A

  , rr2op $ subRegCarryFromA b              -- SBC A,B
  , rr2op $ subRegCarryFromA c              -- SBC A,C
  , rr2op $ subRegCarryFromA d              -- SBC A,D
  , rr2op $ subRegCarryFromA e              -- SBC A,E

  , rr2op $ subRegCarryFromA h              -- SBC A,H
  , rr2op $ subRegCarryFromA l              -- SBC A,L
  , mr2op $ subHLMemCarryFromA              -- SBC A,(HL)
  , rr2op $ subRegCarryFromA a              -- SBC A,A

                                            -- Ax
  , rr2op $ andOpRegIntoA b                 -- AND B
  , rr2op $ andOpRegIntoA c                 -- AND C
  , rr2op $ andOpRegIntoA d                 -- AND D
  , rr2op $ andOpRegIntoA e                 -- AND E

  , rr2op $ andOpRegIntoA h                 -- AND H
  , rr2op $ andOpRegIntoA l                 -- AND L
  , mr2op $ andOpHLMemIntoA                 -- AND (HL)
  , rr2op $ andOpRegIntoA a                 -- AND A

  , rr2op $ xorOpRegIntoA b                 -- XOR B
  , rr2op $ xorOpRegIntoA c                 -- XOR C
  , rr2op $ xorOpRegIntoA d                 -- XOR D
  , rr2op $ xorOpRegIntoA e                 -- XOR E

  , rr2op $ xorOpRegIntoA h                 -- XOR H
  , rr2op $ xorOpRegIntoA l                 -- XOR L
  , mr2op $ xorOpHLMemIntoA                 -- XOR (HL)
  , rr2op $ xorOpRegIntoA a                 -- XOR A

                                            -- Bx
  , rr2op $ orOpRegIntoA b                  -- OR B
  , rr2op $ orOpRegIntoA c                  -- OR C
  , rr2op $ orOpRegIntoA d                  -- OR D
  , rr2op $ orOpRegIntoA e                  -- OR E

  , rr2op $ orOpRegIntoA h                  -- OR H
  , rr2op $ orOpRegIntoA l                  -- OR L
  , mr2op $ orOpHLMemIntoA                  -- OR (HL)
  , rr2op $ orOpRegIntoA a                  -- OR A

  , rr2op $ compAToReg b                    -- CP B
  , rr2op $ compAToReg c                    -- CP C
  , rr2op $ compAToReg d                    -- CP D
  , rr2op $ compAToReg e                    -- CP E

  , rr2op $ compAToReg h                    -- CP H
  , rr2op $ compAToReg l                    -- CP L
  , mr2op $ compAToMemHL                    -- CP(HL)
  , rr2op $ compAToReg a                    -- CP A

                                            -- Cx
  , mr2op $ retFlag true zeroFlag           -- RET NZ
  , mr2op $ popReg setB setC                -- POP BC
  , mr2op $ jumpImmFlag true zeroFlag       -- JP NZ,nn
  , mr2op $ jumpImm                         -- JP nn

  , mm2op $ callImmFlag true zeroFlag       -- CALL NZ,nn
  , mm2op $ pushReg b c                     -- PUSH BC
  , mr2op $ addImmToA                       -- ADD A,n
  , mm2op $ callRoutine 0x00                -- RST 0

  , mr2op $ retFlag false zeroFlag          -- RET Z
  , mr2op $ ret                             -- RET
  , mr2op $ jumpImmFlag false zeroFlag      -- JP Z,nn
  , execExtOps extOps                       -- CB OpCode map

  , mm2op $ callImmFlag false zeroFlag      -- CALL Z,nn
  , mm2op $ callImm                         -- CALL nn
  , mr2op $ addImmCarryToA                  -- ADC A,n
  , mm2op $ callRoutine 0x08                -- RST 8

                                            -- Dx
  , mr2op $ retFlag true carryFlag          -- RET NC
  , mr2op $ popReg setD setE                -- POP DE
  , mr2op $ jumpImmFlag true carryFlag      -- JP NC,nn
  , invalidOpCode

  , mm2op $ callImmFlag true carryFlag      -- CALL NC,nn
  , mm2op $ pushReg d e                     -- PUSH DE
  , mr2op $ subImmFromA                     -- SUB A,n
  , mm2op $ callRoutine 0x10                -- RST 10

  , mr2op $ retFlag false carryFlag         -- RET C
  , mr2op $ retEnableInterrupt              -- RETI
  , mr2op $ jumpImmFlag false carryFlag     -- JP C,nn
  , invalidOpCode

  , mm2op $ callImmFlag true carryFlag      -- CALL C,nn
  , invalidOpCode
  , mr2op subImmCarryToA                    -- SBC A,n
  , mm2op $ callRoutine 0x18                -- RST 18

                                            -- Ex
  , mm2op $ ldFF00ImmMemFromReg a           -- LDH (n),A
  , mr2op $ popReg setH setL                -- POP HL
  , mm2op $ ldFF00CMemFromReg a             -- LDH (C),A
  , invalidOpCode

  , invalidOpCode
  , mm2op $ pushReg h l                     -- PUSH HL
  , mr2op andOpImmIntoA                     -- AND n
  , mm2op $ callRoutine 0x20                -- RST 20

  , mr2op addImmToSP                        -- ADD SP,d
  , rr2op jumpHL                            -- JP (HL)
  , mm2op ldMemImmFromRegA                  -- LD (nn),A
  , invalidOpCode

  , invalidOpCode
  , invalidOpCode
  , mr2op xorOpImmIntoA                     -- XOR n
  , mm2op $ callRoutine 0x28                -- RST 28

                                            -- Fx
  , mr2op $ ldRegFromFF00ImmMem setA        -- LDH A,(n)
  , mr2op $ popReg setA setF                -- POP AF
  , mr2op $ ldRegFromFF00CMem setA          -- LD A,(IOC)
  , rr2op $ setInterrupts false             -- DI

  , invalidOpCode
  , mm2op $ pushReg a f                     -- PUSH AF
  , mr2op orOpImmIntoA                      -- OR n
  , mm2op $ callRoutine 0x30                -- RST 30

  , mr2op ldHLFromSPImm                     -- LDHL SP,d
  , rr2op ldSPFromHL                        -- LD SP,HL
  , mr2op ldRegAFromMemImm                  -- LD A,(nn)
  , rr2op $ setInterrupts true              -- EI

  , invalidOpCode
  , invalidOpCode
  , mr2op compAToImm                        -- CP n
  , mm2op $ callRoutine 0x38                -- RST 38
]
