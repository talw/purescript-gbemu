module Loader where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Array
import Data.Either
import Data.ArrayBuffer.Types as A
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Response
import Network.HTTP.RequestHeader
import Data.MediaType
import Data.DataView

import Types

foreign import fromArrayBuffer :: A.ArrayBuffer -> Array I8

loadRom :: forall e. Aff (console :: CONSOLE, ajax :: AJAX | e)
           (Array I8)
loadRom = do
  res <- affjax $ defaultRequest
    { url = "/rom/tetris.gb"
    , headers = [ContentType $ MediaType "ArrayBuffer"] 
    }
  let rom = fromArrayBuffer res.response
  return rom

{--loadRom :: forall e.--}
  {--Eff (console :: CONSOLE, ajax :: AJAX, err :: EXCEPTION | e) Unit--}
{--loadRom = launchAff $ do--}
  {--res <- affjax $ defaultRequest--}
    {--{ url = "/rom/rom.gb"--}
    {--, headers = [ContentType $ MediaType "ArrayBuffer"] --}
    {--}--}
  {--let x = fromArrayBuffer res.response--}
  {--liftEff $ log $ "GET /api response: "  ++ show x--}
  {--return x--}
