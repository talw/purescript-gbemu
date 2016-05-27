module Debug
  ( module Debug.Trace
  , trh
  , ctrh
  , trs
  , ctrs
  , condTr
  ) where

import Prelude
import Math as M
import Data.Sequence as S
import Data.Maybe
import Data.Int.Bits
import Data.Array as A
import Data.Tuple as T
import Debug.Trace

import Utils

trs :: forall a. Show a => String -> a -> a
trs n a = trace (n++": "++show a) \_ -> a

ctrs :: forall a b. Show a => Boolean -> String -> a -> a
ctrs b n a = if b
  then trace (n++": "++show a) \_ -> a
  else a

trh :: String -> Int -> Int
trh n a = trace (n++": "++toHexStr 2 a) \_ -> a

ctrh :: Boolean -> String -> Int -> Int
ctrh b n a = if b
  then trace (n++": "++toHexStr 2 a) \_ -> a
  else a

condTr :: forall a b. Boolean -> (Unit -> String) -> a -> a
condTr false _ a = a
condTr true strF a = trace (strF unit) \_ -> a
