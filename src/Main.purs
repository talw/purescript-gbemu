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

import Loader
import Core
import Gpu
import Types
import Utils
import Debug
import MemSection as M

--TEST FROMINTARRAY AND CHANGING ITEM OF ARRAY THAT IS A MEMBER OF SOME OBJECT

{--main = do--}
  {--let arr = [0,0,0]--}
  {--arr <- getNew 3 0--}
  {--let x = { a : arr }--}
  {--val1<- M.rd8 0 x.a--}
  {--log $ show val1--}
  {--val2<- M.rd8 1 arr--}
  {--val3<- M.rd8 2 arr--}
  {--M.wr8 5 0 arr--}
  {--M.wr8 6 1 arr--}
  {--M.wr8 7 2 arr--}
  {--log $ show val1 ++ show val2 ++ show val3--}


--NOTE type a type signature once you reach a stable stage
main = launchAff $ do
  afLog "loading rom..."
  rom <- loadRom
  afLog "loaded"
  initialState <- liftEff $ reset rom
  afLog "reset finished"

  liftEff $ drive 234000 initialState
  
  afLog "stopped."
 where
  frameDur = 17556

drive :: forall e. Int -> Z80State
      -> Eff (canvas :: Canvas, timer :: Timer | e) Z80State
drive interval state = do
  {--isPause <- readFromEnvSomething--}
  state' <- run interval state
  if true 
    then return state
    else drive interval state'

afLog :: forall e. String -> Aff (console :: CONSOLE | e) Unit
afLog x = liftEff $ log x

loadRom :: forall e. Aff (console :: CONSOLE, ajax :: AJAX | e)
           (Array I8)
loadRom = do
  res <- affjax $ defaultRequest
    { url = "/rom/tetris.gb"
    , headers = [ContentType $ MediaType "ArrayBuffer"] 
    }
  let rom = fromArrayBuffer res.response
  {--liftEff $ log $ "GET /api response: "  ++ show rom--}
  return rom
