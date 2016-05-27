module OpCodeMap where

import Prelude

import Types
import Ops
import Utils


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
  , rr2op $ rotA LeftD true                  -- RLC A

  , mm2op $ ldMemImmFromSP                  -- LD (nn),SP
  , rr2op $ add2RegsToHL b c                -- ADD HL,BC
  , mr2op $ ldRegFromMem2R setA b c         -- LD A,(BC)
  , rr2op $ decRegWithCarry b c setB setC   -- DEC BC

  , rr2op $ incReg c setC                   -- INC C
  , rr2op $ decReg c setC                   -- DEC C
  , mr2op $ ldRegFromImm setC               -- LD C,n
  , rr2op $ rotA RightD true                 -- RRC A

                                            -- 1x
  , stop                                    -- STOP
  , mr2op $ ldTwoRegsFromImm setD setE      -- LD DE,nn
  , mm2op $ ldMem2RFromReg d e a            -- LD (DE),A
  , rr2op $ incRegWithCarry d e setD setE   -- INC DE

  , rr2op $ incReg d setD                   -- INC D
  , rr2op $ decReg d setD                   -- DEC D
  , mr2op $ ldRegFromImm setD               -- LD D,n
  , rr2op $ rotA LeftD false                 -- RL A

  , mr2op $ jumpRelImm                      -- JR n
  , rr2op $ add2RegsToHL d e                -- ADD HL,DE
  , mr2op $ ldRegFromMem2R setA d e         -- LD A,(DE)
  , rr2op $ decRegWithCarry d e setD setE   -- DEC DE

  , rr2op $ incReg e setE                   -- INC E
  , rr2op $ decReg e setE                   -- DEC E
  , mr2op $ ldRegFromImm setE               -- LD E,n
  , rr2op $ rotA RightD false                -- RR A

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
  , mr2op $ ldSPFromImmMem                  -- LD SP,nn
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
  , mm2op $ callRoutine false 0x00          -- RST 0

  , mr2op $ retFlag false zeroFlag          -- RET Z
  , mr2op $ ret                             -- RET
  , mr2op $ jumpImmFlag false zeroFlag      -- JP Z,nn
  , execExtOps extOps                       -- CB OpCode map

  , mm2op $ callImmFlag false zeroFlag      -- CALL Z,nn
  , mm2op $ callImm                         -- CALL nn
  , mr2op $ addImmCarryToA                  -- ADC A,n
  , mm2op $ callRoutine false 0x08          -- RST 8

                                            -- Dx
  , mr2op $ retFlag true carryFlag          -- RET NC
  , mr2op $ popReg setD setE                -- POP DE
  , mr2op $ jumpImmFlag true carryFlag      -- JP NC,nn
  , invalidOpCode

  , mm2op $ callImmFlag true carryFlag      -- CALL NC,nn
  , mm2op $ pushReg d e                     -- PUSH DE
  , mr2op $ subImmFromA                     -- SUB A,n
  , mm2op $ callRoutine false 0x10          -- RST 10

  , mr2op $ retFlag false carryFlag         -- RET C
  , mm2op $ retEnableInterrupt              -- RETI
  , mr2op $ jumpImmFlag false carryFlag     -- JP C,nn
  , invalidOpCode

  , mm2op $ callImmFlag true carryFlag      -- CALL C,nn
  , invalidOpCode
  , mr2op subImmCarryToA                    -- SBC A,n
  , mm2op $ callRoutine false 0x18          -- RST 18

                                            -- Ex
  , mm2op $ ldFF00ImmMemFromReg a           -- LDH (n),A
  , mr2op $ popReg setH setL                -- POP HL
  , mm2op $ ldFF00CMemFromReg a             -- LDH (C),A
  , invalidOpCode

  , invalidOpCode
  , mm2op $ pushReg h l                     -- PUSH HL
  , mr2op andOpImmIntoA                     -- AND n
  , mm2op $ callRoutine false 0x20          -- RST 20

  , mr2op addImmToSP                        -- ADD SP,d
  , rr2op jumpHL                            -- JP (HL)
  , mm2op ldMemImmFromRegA                  -- LD (nn),A
  , invalidOpCode

  , invalidOpCode
  , invalidOpCode
  , mr2op xorOpImmIntoA                     -- XOR n
  , mm2op $ callRoutine false 0x28          -- RST 28

                                            -- Fx
  , mr2op $ ldRegFromFF00ImmMem setA        -- LDH A,(n)
  , mr2op $ popReg setA setF                -- POP AF
  , mr2op $ ldRegFromFF00CMem setA          -- LD A,(IOC)
  , mm2op $ setInterrupts false             -- DI

  , invalidOpCode
  , mm2op $ pushReg a f                     -- PUSH AF
  , mr2op orOpImmIntoA                      -- OR n
  , mm2op $ callRoutine false 0x30          -- RST 30

  , mr2op ldHLFromSPImm                     -- LDHL SP,d
  , rr2op ldSPFromHL                        -- LD SP,HL
  , mr2op ldRegAFromMemImm                  -- LD A,(nn)
  , mm2op $ setInterrupts true              -- EI

  , invalidOpCode
  , invalidOpCode
  , mr2op compAToImm                        -- CP n
  , mm2op $ callRoutine false 0x38          -- RST 38
]

extOps :: Array (Z80State -> Z80State)
extOps = 
                                          -- 0x
  [ rr2op $ rotReg LeftD true setB b       -- RLC B
  , rr2op $ rotReg LeftD true setC c       -- RLC C
  , rr2op $ rotReg LeftD true setD d       -- RLC D
  , rr2op $ rotReg LeftD true setE e       -- RLC E

  , rr2op $ rotReg LeftD true setH h       -- RLC H
  , rr2op $ rotReg LeftD true setL l       -- RLC L
  , mm2op $ rotHLMem LeftD true            -- RLC (HL)
  , rr2op $ rotReg LeftD true setA a       -- RLC A

  , rr2op $ rotReg RightD true setB b      -- RRC B
  , rr2op $ rotReg RightD true setC c      -- RRC C
  , rr2op $ rotReg RightD true setD d      -- RRC D
  , rr2op $ rotReg RightD true setE e      -- RRC E

  , rr2op $ rotReg RightD true setH h      -- RRC H
  , rr2op $ rotReg RightD true setL l      -- RRC L
  , mm2op $ rotHLMem RightD true           -- RRC (HL)
  , rr2op $ rotReg RightD true setA a      -- RRC A

                                          -- 1x
  , rr2op $ rotReg LeftD false setB b      -- RL B
  , rr2op $ rotReg LeftD false setC c      -- RL C
  , rr2op $ rotReg LeftD false setD d      -- RL D
  , rr2op $ rotReg LeftD false setE e      -- RL E

  , rr2op $ rotReg LeftD false setH h      -- RL H
  , rr2op $ rotReg LeftD false setL l      -- RL L
  , mm2op $ rotHLMem LeftD false           -- RL (HL)
  , rr2op $ rotReg LeftD false setA a      -- RL A

  , rr2op $ rotReg RightD false setB b     -- RR B
  , rr2op $ rotReg RightD false setC c     -- RR C
  , rr2op $ rotReg RightD false setD d     -- RR D
  , rr2op $ rotReg RightD false setE e     -- RR E

  , rr2op $ rotReg RightD false setH h     -- RR H
  , rr2op $ rotReg RightD false setL l     -- RR L
  , mm2op $ rotHLMem RightD false          -- RR(HL)
  , rr2op $ rotReg RightD false setA a     -- RR A

                                          -- 2x
  , rr2op $ shiftReg LeftD false setB b    -- SLA B
  , rr2op $ shiftReg LeftD false setC c    -- SLA C
  , rr2op $ shiftReg LeftD false setD d    -- SLA D
  , rr2op $ shiftReg LeftD false setE e    -- SLA E

  , rr2op $ shiftReg LeftD false setH h    -- SLA H
  , rr2op $ shiftReg LeftD false setL l    -- SLA L
  , mm2op $ shiftMemHL LeftD false         -- SLA(HL)
  , rr2op $ shiftReg LeftD false setA a    -- SLA A

  , rr2op $ shiftReg RightD true setB b    -- SRA B
  , rr2op $ shiftReg RightD true setC c    -- SRA C
  , rr2op $ shiftReg RightD true setD d    -- SRA D
  , rr2op $ shiftReg RightD true setE e    -- SRA E

  , rr2op $ shiftReg RightD true setH h    -- SRA H
  , rr2op $ shiftReg RightD true setL l    -- SRA L
  , mm2op $ shiftMemHL RightD true         -- SRA (HL)
  , rr2op $ shiftReg RightD true setA a    -- SRA A

                                          -- 3x
  , rr2op $ swapReg setB b                -- SWAP B
  , rr2op $ swapReg setC c                -- SWAP C
  , rr2op $ swapReg setD d                -- SWAP D
  , rr2op $ swapReg setE e                -- SWAP E

  , rr2op $ swapReg setH h                -- SWAP H
  , rr2op $ swapReg setL l                -- SWAP L
  , mm2op $ swapMemHL                     -- SWAP(HL)
  , rr2op $ swapReg setA a                -- SWAP A

  , rr2op $ shiftReg RightD false setB b   -- SRL B
  , rr2op $ shiftReg RightD false setC c   -- SRL C
  , rr2op $ shiftReg RightD false setD d   -- SRL D
  , rr2op $ shiftReg RightD false setE e   -- SRL E

  , rr2op $ shiftReg RightD false setH h   -- SRL H
  , rr2op $ shiftReg RightD false setL l   -- SRL L
  , mm2op $ shiftMemHL RightD false        -- SRL (HL)
  , rr2op $ shiftReg RightD false setA a   -- SRL A

                                          -- 4x
  , rr2op $ testBitNOfReg 0 b             -- BIT 0,B
  , rr2op $ testBitNOfReg 0 c             -- BIT 0,C
  , rr2op $ testBitNOfReg 0 d             -- BIT 0,D
  , rr2op $ testBitNOfReg 0 e             -- BIT 0,E

  , rr2op $ testBitNOfReg 0 h             -- BIT 0,H
  , rr2op $ testBitNOfReg 0 l             -- BIT 0,L
  , mr2op $ testBitNOfHLMem 0             -- BIT 0,(HL)
  , rr2op $ testBitNOfReg 0 a             -- BIT 0,A

  , rr2op $ testBitNOfReg 1 b             -- BIT 1,B
  , rr2op $ testBitNOfReg 1 c             -- BIT 1,C
  , rr2op $ testBitNOfReg 1 d             -- BIT 1,D
  , rr2op $ testBitNOfReg 1 e             -- BIT 1,E

  , rr2op $ testBitNOfReg 1 h             -- BIT 1,H
  , rr2op $ testBitNOfReg 1 l             -- BIT 1,L
  , mr2op $ testBitNOfHLMem 1             -- BIT 1,(HL)
  , rr2op $ testBitNOfReg 1 a             -- BIT 1,A

                                          -- 5x
  , rr2op $ testBitNOfReg 2 b             -- BIT 2,B
  , rr2op $ testBitNOfReg 2 c             -- BIT 2,C
  , rr2op $ testBitNOfReg 2 d             -- BIT 2,D
  , rr2op $ testBitNOfReg 2 e             -- BIT 2,E

  , rr2op $ testBitNOfReg 2 h             -- BIT 2,H
  , rr2op $ testBitNOfReg 2 l             -- BIT 2,L
  , mr2op $ testBitNOfHLMem 2             -- BIT 2,(HL)
  , rr2op $ testBitNOfReg 2 a             -- BIT 2,A

  , rr2op $ testBitNOfReg 3 b             -- BIT 3,B
  , rr2op $ testBitNOfReg 3 c             -- BIT 3,C
  , rr2op $ testBitNOfReg 3 d             -- BIT 3,D
  , rr2op $ testBitNOfReg 3 e             -- BIT 3,E

  , rr2op $ testBitNOfReg 3 h             -- BIT 3,H
  , rr2op $ testBitNOfReg 3 l             -- BIT 3,L
  , mr2op $ testBitNOfHLMem 3             -- BIT 3,(HL)
  , rr2op $ testBitNOfReg 3 a             -- BIT 3,A

                                          -- 6x
  , rr2op $ testBitNOfReg 4 b             -- BIT 4,B
  , rr2op $ testBitNOfReg 4 c             -- BIT 4,C
  , rr2op $ testBitNOfReg 4 d             -- BIT 4,D
  , rr2op $ testBitNOfReg 4 e             -- BIT 4,E

  , rr2op $ testBitNOfReg 4 h             -- BIT 4,H
  , rr2op $ testBitNOfReg 4 l             -- BIT 4,L
  , mr2op $ testBitNOfHLMem 4             -- BIT 4,(HL)
  , rr2op $ testBitNOfReg 4 a             -- BIT 4,A

  , rr2op $ testBitNOfReg 5 b             -- BIT 5,B
  , rr2op $ testBitNOfReg 5 c             -- BIT 5,C
  , rr2op $ testBitNOfReg 5 d             -- BIT 5,D
  , rr2op $ testBitNOfReg 5 e             -- BIT 5,E

  , rr2op $ testBitNOfReg 5 h             -- BIT 5,H
  , rr2op $ testBitNOfReg 5 l             -- BIT 5,L
  , mr2op $ testBitNOfHLMem 5             -- BIT 5,(HL)
  , rr2op $ testBitNOfReg 5 a             -- BIT 5,A

                                          -- 7x
  , rr2op $ testBitNOfReg 6 b             -- BIT 6,B
  , rr2op $ testBitNOfReg 6 c             -- BIT 6,C
  , rr2op $ testBitNOfReg 6 d             -- BIT 6,D
  , rr2op $ testBitNOfReg 6 e             -- BIT 6,E

  , rr2op $ testBitNOfReg 6 h             -- BIT 6,H
  , rr2op $ testBitNOfReg 6 l             -- BIT 6,L
  , mr2op $ testBitNOfHLMem 6             -- BIT 6,(HL)
  , rr2op $ testBitNOfReg 6 a             -- BIT 6,A

  , rr2op $ testBitNOfReg 7 b             -- BIT 7,B
  , rr2op $ testBitNOfReg 7 c             -- BIT 7,C
  , rr2op $ testBitNOfReg 7 d             -- BIT 7,D
  , rr2op $ testBitNOfReg 7 e             -- BIT 7,E

  , rr2op $ testBitNOfReg 7 h             -- BIT 7,H
  , rr2op $ testBitNOfReg 7 l             -- BIT 7,L
  , mr2op $ testBitNOfHLMem 7             -- BIT 7,(HL)
  , rr2op $ testBitNOfReg 7 a             -- BIT 7,A

                                          -- 8x
  , rr2op $ setBitNOfReg false 0 setB b   -- RES 0,B
  , rr2op $ setBitNOfReg false 0 setC c   -- RES 0,C
  , rr2op $ setBitNOfReg false 0 setD d   -- RES 0,D
  , rr2op $ setBitNOfReg false 0 setE e   -- RES 0,E

  , rr2op $ setBitNOfReg false 0 setH h   -- RES 0,H
  , rr2op $ setBitNOfReg false 0 setL l   -- RES 0,L
  , mm2op $ setBitNOfHLMem false 0        -- RES 0,(HL)
  , rr2op $ setBitNOfReg false 0 setA a   -- RES 0,A

  , rr2op $ setBitNOfReg false 1 setB b   -- RES 1,B
  , rr2op $ setBitNOfReg false 1 setC c   -- RES 1,C
  , rr2op $ setBitNOfReg false 1 setD d   -- RES 1,D
  , rr2op $ setBitNOfReg false 1 setE e   -- RES 1,E

  , rr2op $ setBitNOfReg false 1 setH h   -- RES 1,H
  , rr2op $ setBitNOfReg false 1 setL l   -- RES 1,L
  , mm2op $ setBitNOfHLMem false 1        -- RES 1,(HL)
  , rr2op $ setBitNOfReg false 1 setA a   -- RES 1,A

                                          -- 9x
  , rr2op $ setBitNOfReg false 2 setB b   -- RES 2,B
  , rr2op $ setBitNOfReg false 2 setC c   -- RES 2,C
  , rr2op $ setBitNOfReg false 2 setD d   -- RES 2,D
  , rr2op $ setBitNOfReg false 2 setE e   -- RES 2,E

  , rr2op $ setBitNOfReg false 2 setH h   -- RES 2,H
  , rr2op $ setBitNOfReg false 2 setL l   -- RES 2,L
  , mm2op $ setBitNOfHLMem false 2        -- RES 2,(HL)
  , rr2op $ setBitNOfReg false 2 setA a   -- RES 2,A

  , rr2op $ setBitNOfReg false 3 setB b   -- RES 3,B
  , rr2op $ setBitNOfReg false 3 setC c   -- RES 3,C
  , rr2op $ setBitNOfReg false 3 setD d   -- RES 3,D
  , rr2op $ setBitNOfReg false 3 setE e   -- RES 3,E

  , rr2op $ setBitNOfReg false 3 setH h   -- RES 3,H
  , rr2op $ setBitNOfReg false 3 setL l   -- RES 3,L
  , mm2op $ setBitNOfHLMem false 3        -- RES 3,(HL)
  , rr2op $ setBitNOfReg false 3 setA a   -- RES 3,A

                                          -- Ax
  , rr2op $ setBitNOfReg false 4 setB b   -- RES 4,B
  , rr2op $ setBitNOfReg false 4 setC c   -- RES 4,C
  , rr2op $ setBitNOfReg false 4 setD d   -- RES 4,D
  , rr2op $ setBitNOfReg false 4 setE e   -- RES 4,E

  , rr2op $ setBitNOfReg false 4 setH h   -- RES 4,H
  , rr2op $ setBitNOfReg false 4 setL l   -- RES 4,L
  , mm2op $ setBitNOfHLMem false 4        -- RES 4,(HL)
  , rr2op $ setBitNOfReg false 4 setA a   -- RES 4,A

  , rr2op $ setBitNOfReg false 5 setB b   -- RES 5,B
  , rr2op $ setBitNOfReg false 5 setC c   -- RES 5,C
  , rr2op $ setBitNOfReg false 5 setD d   -- RES 5,D
  , rr2op $ setBitNOfReg false 5 setE e   -- RES 5,E

  , rr2op $ setBitNOfReg false 5 setH h   -- RES 5,H
  , rr2op $ setBitNOfReg false 5 setL l   -- RES 5,L
  , mm2op $ setBitNOfHLMem false 5        -- RES 5,(HL)
  , rr2op $ setBitNOfReg false 5 setA a   -- RES 5,A

                                          -- Bx
  , rr2op $ setBitNOfReg false 6 setB b   -- RES 6,B
  , rr2op $ setBitNOfReg false 6 setC c   -- RES 6,C
  , rr2op $ setBitNOfReg false 6 setD d   -- RES 6,D
  , rr2op $ setBitNOfReg false 6 setE e   -- RES 6,E

  , rr2op $ setBitNOfReg false 6 setH h   -- RES 6,H
  , rr2op $ setBitNOfReg false 6 setL l   -- RES 6,L
  , mm2op $ setBitNOfHLMem false 6        -- RES 6,(HL)
  , rr2op $ setBitNOfReg false 6 setA a   -- RES 6,A

  , rr2op $ setBitNOfReg false 7 setB b   -- RES 7,B
  , rr2op $ setBitNOfReg false 7 setC c   -- RES 7,C
  , rr2op $ setBitNOfReg false 7 setD d   -- RES 7,D
  , rr2op $ setBitNOfReg false 7 setE e   -- RES 7,E

  , rr2op $ setBitNOfReg false 7 setH h   -- RES 7,H
  , rr2op $ setBitNOfReg false 7 setL l   -- RES 7,L
  , mm2op $ setBitNOfHLMem false 7        -- RES 7,(HL)
  , rr2op $ setBitNOfReg false 7 setA a   -- RES 7,A


                                          -- Cx
  , rr2op $ setBitNOfReg true 0 setB b    -- SET 0,B
  , rr2op $ setBitNOfReg true 0 setC c    -- SET 0,C
  , rr2op $ setBitNOfReg true 0 setD d    -- SET 0,D
  , rr2op $ setBitNOfReg true 0 setE e    -- SET 0,E

  , rr2op $ setBitNOfReg true 0 setH h    -- SET 0,H
  , rr2op $ setBitNOfReg true 0 setL l    -- SET 0,L
  , mm2op $ setBitNOfHLMem true 0         -- SET 0,(HL)
  , rr2op $ setBitNOfReg true 0 setA a    -- SET 0,A

  , rr2op $ setBitNOfReg true 1 setB b    -- SET 1,B
  , rr2op $ setBitNOfReg true 1 setC c    -- SET 1,C
  , rr2op $ setBitNOfReg true 1 setD d    -- SET 1,D
  , rr2op $ setBitNOfReg true 1 setE e    -- SET 1,E

  , rr2op $ setBitNOfReg true 1 setH h    -- SET 1,H
  , rr2op $ setBitNOfReg true 1 setL l    -- SET 1,L
  , mm2op $ setBitNOfHLMem true 1         -- SET 1,(HL)
  , rr2op $ setBitNOfReg true 1 setA a    -- SET 1,A

                                          -- Dx
  , rr2op $ setBitNOfReg true 2 setB b    -- SET 2,B
  , rr2op $ setBitNOfReg true 2 setC c    -- SET 2,C
  , rr2op $ setBitNOfReg true 2 setD d    -- SET 2,D
  , rr2op $ setBitNOfReg true 2 setE e    -- SET 2,E

  , rr2op $ setBitNOfReg true 2 setH h    -- SET 2,H
  , rr2op $ setBitNOfReg true 2 setL l    -- SET 2,L
  , mm2op $ setBitNOfHLMem true 2         -- SET 2,(HL)
  , rr2op $ setBitNOfReg true 2 setA a    -- SET 2,A

  , rr2op $ setBitNOfReg true 3 setB b    -- SET 3,B
  , rr2op $ setBitNOfReg true 3 setC c    -- SET 3,C
  , rr2op $ setBitNOfReg true 3 setD d    -- SET 3,D
  , rr2op $ setBitNOfReg true 3 setE e    -- SET 3,E

  , rr2op $ setBitNOfReg true 3 setH h    -- SET 3,H
  , rr2op $ setBitNOfReg true 3 setL l    -- SET 3,L
  , mm2op $ setBitNOfHLMem true 3         -- SET 3,(HL)
  , rr2op $ setBitNOfReg true 3 setA a    -- SET 3,A

                                          -- Ex
  , rr2op $ setBitNOfReg true 4 setB b    -- SET 4,B
  , rr2op $ setBitNOfReg true 4 setC c    -- SET 4,C
  , rr2op $ setBitNOfReg true 4 setD d    -- SET 4,D
  , rr2op $ setBitNOfReg true 4 setE e    -- SET 4,E

  , rr2op $ setBitNOfReg true 4 setH h    -- SET 4,H
  , rr2op $ setBitNOfReg true 4 setL l    -- SET 4,L
  , mm2op $ setBitNOfHLMem true 4         -- SET 4,(HL)
  , rr2op $ setBitNOfReg true 4 setA a    -- SET 4,A

  , rr2op $ setBitNOfReg true 5 setB b    -- SET 5,B
  , rr2op $ setBitNOfReg true 5 setC c    -- SET 5,C
  , rr2op $ setBitNOfReg true 5 setD d    -- SET 5,D
  , rr2op $ setBitNOfReg true 5 setE e    -- SET 5,E

  , rr2op $ setBitNOfReg true 5 setH h    -- SET 5,H
  , rr2op $ setBitNOfReg true 5 setL l    -- SET 5,L
  , mm2op $ setBitNOfHLMem true 5         -- SET 5,(HL)
  , rr2op $ setBitNOfReg true 5 setA a    -- SET 5,A

                                          -- Fx
  , rr2op $ setBitNOfReg true 6 setB b    -- SET 6,B
  , rr2op $ setBitNOfReg true 6 setC c    -- SET 6,C
  , rr2op $ setBitNOfReg true 6 setD d    -- SET 6,D
  , rr2op $ setBitNOfReg true 6 setE e    -- SET 6,E

  , rr2op $ setBitNOfReg true 6 setH h    -- SET 6,H
  , rr2op $ setBitNOfReg true 6 setL l    -- SET 6,L
  , mm2op $ setBitNOfHLMem true 6         -- SET 6,(HL)
  , rr2op $ setBitNOfReg true 6 setA a    -- SET 6,A

  , rr2op $ setBitNOfReg true 7 setB b    -- SET 7,B
  , rr2op $ setBitNOfReg true 7 setC c    -- SET 7,C
  , rr2op $ setBitNOfReg true 7 setD d    -- SET 7,D
  , rr2op $ setBitNOfReg true 7 setE e    -- SET 7,E

  , rr2op $ setBitNOfReg true 7 setH h    -- SET 7,H
  , rr2op $ setBitNOfReg true 7 setL l    -- SET 7,L
  , mm2op $ setBitNOfHLMem true 7         -- SET 7,(HL)
  , rr2op $ setBitNOfReg true 7 setA a    -- SET 7,A
]
