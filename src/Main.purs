module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Array
import Data.Either
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Response
import Network.HTTP.RequestHeader
import Data.MediaType
import Data.DataView
import DOM.Timer

import Loader
import Core
import Gpu
import Types
import Utils
import Debug
import MemSection as M

--NOTE type a type signature once you reach a stable stage
main = launchAff $ do
  afLog "loading rom..."
  rom <- loadRom
  afLog "loaded"
  initialState <- liftEff $ reset rom
  afLog "reset finished"

  liftEff $ drive 2634000 initialState
  
  afLog "stopped."
 where
  frameDur = 17556

drive :: forall e. Int -> Z80State
      -> Eff (ma :: MemAccess, canvas :: Canvas, timer :: Timer | e) Z80State
drive interval state = do
  {--isPause <- readFromEnvSomething--}
  state' <- run frameDur state
  if state'.totalM > interval
    then return state
    else do
      timeout 1 $ drive interval state'
      return state

frameDur :: Int
frameDur = 17556

afLog :: forall e. String -> Aff (console :: CONSOLE | e) Unit
afLog x = liftEff $ log x
