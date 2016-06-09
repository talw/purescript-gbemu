module Main where

import Prelude (Unit, ($), return, bind, (>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff.Class (liftEff)
import DOM.Timer (Timer, timeout)
import Network.HTTP.Affjax (AJAX)

import Loader (loadRom)
import Core (run, reset)
import Gpu (Canvas)
import Types (Z80State, MemAccess)

foreign import changeLabel :: forall e. String -> Eff (console :: CONSOLE | e) Unit

main :: forall e. Eff
  ( err :: EXCEPTION
  , console :: CONSOLE
  , ajax :: AJAX
  , canvas :: Canvas
  , ma :: MemAccess
  , timer :: Timer
  | e) Unit
main = launchAff $ do
  afLog "loading rom..."
  rom <- loadRom
  afLog "loaded"
  initialState <- liftEff $ reset rom
  afLog "reset finished"

  --Run the emulator just enough to get to the title screen of Tetris.
  liftEff $ drive 1034000 initialState
  {--liftEff $ drive 100 initialState--}

drive :: forall e. Int -> Z80State
      -> Eff (ma :: MemAccess, canvas :: Canvas, timer :: Timer, console :: CONSOLE | e) Z80State
drive interval state = do
  {--isPause <- readFromEnvSomething--}
  {--state' <- run frameDur state--}
  state' <- run frameDur state
  if state'.totalM > interval
    then do
      changeLabel "Stopped"
      return state
    else do
      --Between consecutive drive calls, which drive the emulator a frame at a time,
      --interleave idle times of 1 ms to let the canvas refresh itself.
      --If I ever get to a state where the emulator is up to 100% speed, this should be
      --changed to wait the time that remains to complete a 1/60 seconds period
      --to get 60 fps.
      timeout 1 $ drive interval state'
      return state

frameDur :: Int
frameDur = 17556

afLog :: forall e. String -> Aff (console :: CONSOLE | e) Unit
afLog x = liftEff $ log x
