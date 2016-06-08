module Core where

import Prelude (show, (++), ($), (&&), (==), (||), (+), bind, (<$>), (/=)
               ,(>>=), return, id, (<<<), (-), (<=))
import Data.Int.Bits ((.&.), (.|.))
import Data.Tuple (Tuple(Tuple), snd, fst)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Either (Either(Left, Right))
import Data.Foldable (elem)
import Data.Functor ((<$))
import Control.Monad.Eff (Eff)
import Control.Bind ((=<<))
import Control.Monad.Rec.Class (tailRecM)

import Types (Z80State, MemAccess, SavedRegs, Regs, Mem, I8, I16)
import MainMem (getIme, getIntE, getIntF, rd8, cleanMainMem, setIme, setIntF
               ,setVblIntrr, setGpu, getGpu, updImeCnt, setRom)
import Ops (callRoutine, getOpTiming, getCpuOp)
import OpCodeMap (mme2op, basicOpTimings, branchBasicOpTimings, cbOpTimings
                 ,basicOps)
import Gpu (Canvas, gpuStep)
import Utils (toHexStr, getOnBit)
import Regs

import Debug (StatTimer, condTr, timeIt)

--Get a fresh initial Z80State.
--The MemAccess effect is due to some writing operations during initialization.
reset :: forall e. Array I8 -> Eff (ma :: MemAccess, canvas :: Canvas | e) Z80State
reset rom = do
  cs <- cleanState
  return
    cs {
      mem = cs.mem {
        mainMem = setRom rom cs.mem.mainMem
      }
    }


--Run emulation until 'interval' clocks have passed.
run :: forall e. Int -> Z80State -> Eff (ma :: MemAccess, canvas :: Canvas | e) Z80State
run interval state = tailRecM go { intr : interval, st : state }
 where
  go { st = st@{stop = true} } = return $ Right st
  go { intr, st } | intr <= 0 = return $ Right st
  go { intr, st } = do
    {--if (shldTrc st) then traceA "-------step-------" else return unit--}
    st' <- step st
    {--st' <- stepTime st--}
    let elapsed = st'.totalM - st.totalM
    return $ Left { intr : (intr - elapsed), st : st' }

--Perform a single state operation, along with updatig accompanying state.
step :: forall e. Z80State -> Eff (ma :: MemAccess, canvas :: Canvas | e) Z80State
step state@{ mem = oldMem@{regs = oldRegs} } = do
  opt <- opTiming
  state2 <- op state
  state3 <- incPc oldPc state2
  state4 <- incTime state3
  let state5 = updImeCntInMem state4
  handleGpu opt
    state5
    {--$ traceState state5--}
 where
  oldPc = pc oldRegs
  op st = if checkShldHndlIntrr st
    then interruptOp st
    else regularOp st
  regularOp st = do
    coc <- getCurrOpCode st
    getCpuOp coc basicOps st
  getCurrOpCode st = rd8 (pc st.mem.regs) st.mem.mainMem
  incTime st@{totalM} = do
    opt <- opTiming
    return st { totalM = st.totalM + opt }
  opTiming = getCurrOpTiming oldPc state
  updImeCntInMem st = st { mem = st.mem { mainMem = updImeCnt st.mem.mainMem } }

--Get the clock amount of the current operation.
getCurrOpTiming :: forall e. I16 -> Z80State -> Eff (ma :: MemAccess | e) Int
getCurrOpTiming oldPc st = do
  opCode <- rd8 oldPc st.mem.mainMem
  let isExtOp = opCode == 0xCB
  addr <- if isExtOp
    then rd8 (oldPc+1) st.mem.mainMem
    else return opCode
  let opTimingTable = if isExtOp
        then cbOpTimings 
        else
          if brTkn st.mem.regs
            then branchBasicOpTimings
            else basicOpTimings
  return $ getOpTiming addr opTimingTable

--Update the gpu's state with the time it took to do the current operation,
--as it is waiting a certain amount of time between the different modes of
--updating the screen.
--NOTE is there a better way to set nested properties?
--consider checking the lens-equivalent in purescript.
incPc :: forall e. I8 -> Z80State -> Eff (ma :: MemAccess | e) Z80State
incPc oldPc st =
  if oldPc /= pc st.mem.regs
    then return st
    else st <$ setPC (65535 .&. (pc st.mem.regs + 1)) st.mem.regs

--NOTE change handleGpu to get the op timing itself, instead of reacquiring it here.
handleGpu :: forall e. Int -> Z80State
          -> Eff (ma :: MemAccess, canvas :: Canvas | e) Z80State
handleGpu opTiming st = do
  gpu' <- gpuStep opTiming (getGpu st.mem.mainMem)
  let intF' = (if gpu'.vblIntrr
                 then 
                   ({-trace ("screen refresh, clk-count: " ++ show st.totalM) \_ ->-}
                     (0x01 .|. _))
                   else id)
                $ getIntF st.mem.mainMem

  setGpu gpu' st.mem.mainMem
  setVblIntrr false st.mem.mainMem 
  setIntF intF' st.mem.mainMem
  return st

--Get the current interrupt handling operation, if there is an interrupt
--waiting to be handled
interruptOp :: forall e. Z80State -> Eff (ma :: MemAccess | e) Z80State
interruptOp st = do
  setIntF (fst res $ getIntF st.mem.mainMem) st.mem.mainMem 
  setIme false st.mem.mainMem
  snd res st
 where
  res :: Tuple (Int -> Int) (Z80State -> Eff (ma :: MemAccess | e) Z80State)
  res = fromMaybe (Tuple id return) mRes
  mRes = getOnBit (interruptBits st) >>= \x -> case x of
           1  -> Just $ Tuple (0xFE .&. _) (mme2op $ callRoutine true 0x40 )
           2  -> Just $ Tuple (0xFD .&. _) (mme2op $ callRoutine true 0x48 )
           4  -> Just $ Tuple (0xFB .&. _) (mme2op $ callRoutine true 0x50 )
           8  -> Just $ Tuple (0xF7 .&. _) (mme2op $ callRoutine true 0x58 )
           16 -> Just $ Tuple (0xEF .&. _) (mme2op $ callRoutine true 0x60 )
           otherwise -> Nothing -- NOTE log this

--Get bit flags of interrupts waiting to be handled AND enabled
interruptBits :: Z80State -> I8
interruptBits st = getIntE st.mem.mainMem .&. getIntF st.mem.mainMem

checkShldHndlIntrr :: Z80State -> Boolean
checkShldHndlIntrr st = getIme st.mem.mainMem && interruptBits st /= 0 

cleanState :: forall e. Eff (ma :: MemAccess | e) Z80State
cleanState = 
  { mem : _
  , totalM : 1
  , halt : false
  , stop : false
  } <$> cleanMem

cleanMem :: forall e. Eff (ma :: MemAccess | e) Mem
cleanMem =
  { regs : cleanRegs
  , svdRegs : cleanSavedRegs
  , mainMem : _
  } <$> cleanMainMem

--Debug functions

--Step function that uses the Timing functions of Debug to compute average
--running times for operations.
stepTime :: forall e. Z80State
         -> Eff (ma :: MemAccess, canvas :: Canvas, timer :: StatTimer | e) Z80State
stepTime state = do
  currOpCode <- rd8 (pc state.mem.regs) state.mem.mainMem
  currImm <- rd8 (pc state.mem.regs + 1) state.mem.mainMem
  let timerIx = if (currOpCode `elem` memModifyOps)
                || (currOpCode == 0xCB && (currImm `elem` cbMemModifyOps))
                  then 1 else 0
  timeIt timerIx (\_ -> step state)
 where
  memModifyOps =
    [ 0x02 ,0x08 ,0x12 ,0x22 ,0x32 ,0x34 ,0x35 ,0x36 ,0x70 ,0x71
    , 0x72 ,0x73 ,0x74 ,0x75 ,0x77 ,0xc4 ,0xc5 ,0xc7 ,0xcc ,0xcd ,0xcf ,0xd4
    , 0xd5 ,0xd7 ,0xd9 ,0xdc ,0xdf ,0xe0 ,0xe2 ,0xe5 ,0xe7 ,0xea ,0xef ,0xf3
    , 0xf5 ,0xf7 ,0xfb ,0xff
    ]
  cbMemModifyOps = 
    [ 0x06, 0x0e, 0x16, 0x1e, 0x26, 0x2e, 0x36, 0x3e, 0x86, 0x8e, 0x96, 0x9e
    , 0xa6, 0xae, 0xb6, 0xbe, 0xc6, 0xce, 0xd6, 0xde, 0xe6, 0xee, 0xf6, 0xfe
    ]

--Useful function for tracing various properties of the Z80State,
--every cpu step as long as shldTrc condition is satisfied.
--Commented lines remain for ease of enabling.
traceState :: Z80State -> Z80State
traceState state@{ mem = oldMem@{regs = oldRegs} } =
  condTr (shldTrc state) (\_ -> "totalM: "++show (state.totalM)) $
  {--condTr (shldTrc state) (\_ -> "totalM: "++show (state.totalM - 123958)) $--}

  {--condTr (shldTrc state) (\_ -> tileMap0Str state.mem) $--}
  {--condTr (shldTrc state) (\_ -> tileSet0Str state.mem) $--}

  condTr (shldTrc state) (\_ -> regsStr oldRegs) $
  {--condTr (shldTrc state) (\_ -> pcArea oldMem) $--}
  {--condTr (shldTrc state) (\_ -> spArea oldMem) $--}
  {--condTr shldTrc (\_ -> ioArea oldMem) $--}

  condTr (shldTrc state) (\_ -> "IntF: " ++ toHexStr 2 (getIntF state.mem.mainMem)) $
  condTr (shldTrc state) (\_ -> "IntE: " ++ toHexStr 2 (getIntE state.mem.mainMem)) $
  condTr (shldTrc state) (\_ -> "Ime: " ++ show (getIme state.mem.mainMem)) $

  {--condTr shldTrc (\_ -> "IntBits: " ++ toHexStr 2 intBits) $--}
  {--condTr shldTrc (\_ -> "getOnBit: " ++ show (getOnBit intBits)) $--}
  {--condTr shldTrc (\_ -> "shldHndlIntrr: " ++ show (shldHndlIntrr state)) $--}
  {--condTr shldTrc (\_ -> "intF after interruptop: " ++ toHexStr 2 (getIntF (interruptOp state).mem.mainMem)) $--}

  {--condTr (shldTrc state) (\_ -> "enbCnt: " ++ show (getImeEnableCnt state.mem.mainMem)) $--}

  {--condTr (shldTrc state) (\_ -> rd8RangeStr "FF40" 0xFF40 0xFF40 state.mem) $--}
  {--condTr (shldTrc state) (\_ -> rd8RangeStr "FF44" 0xFF44 0xFF44 state.mem) $--}
  state

shldTrc :: Z80State -> Boolean
{--shldTrc state = state.totalM == 233644--}
shldTrc state = false
{--shldTrc state = state.mem.regs.pc == 0x0405 || state.mem.regs.pc == 0x0371--}
