module Loader where

import Prelude (return, ($), bind)
import Control.Monad.Eff.Console (CONSOLE)
import Data.ArrayBuffer.Types as A
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax (AJAX, defaultRequest, affjax)
import Network.HTTP.RequestHeader (RequestHeader(ContentType))
import Data.MediaType (MediaType(MediaType))

import Types (I8)


foreign import fromArrayBuffer :: A.ArrayBuffer -> Array I8

--An ajax call to download a rom into an ArrayBuffer..
loadRom :: forall e. Aff (console :: CONSOLE, ajax :: AJAX | e)
           (Array I8)
loadRom = do
  res <- affjax $ defaultRequest
    { url = "/rom/tetris.gb"
    , headers = [ContentType $ MediaType "ArrayBuffer"] 
    }
  let rom = fromArrayBuffer res.response
  return rom
