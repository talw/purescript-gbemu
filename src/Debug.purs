module Debug
  ( module Debug.Trace
  , trh
  , ctrh
  , trs
  , ctrs
  , condTr
  , ramStr
  , memStrRange
  , timeIt
  , Timer
  ) where

import Prelude
import Math as M
import Data.Sequence as S
import Data.Maybe
import Control.Bind
import Control.Monad.Eff
import Data.Int.Bits
import Data.Array as A
import Data.Tuple as T
import Debug.Trace
import Data.Foldable

import Types
import Utils

-- Timer
-- =====

foreign import data Timer :: !

foreign import startTimer :: forall e. Eff (timer :: Timer | e) Unit
foreign import endTimer :: forall e. Eff (timer :: Timer | e) Int
foreign import recordTime :: forall e. Int -> Int -> Int -> Eff (timer :: Timer | e) Unit

timeIt :: forall e a. Int -> (Unit -> Eff (timer :: Timer | e) a) -> Eff (timer :: Timer | e) a
timeIt timerIx f = do
  startTimer
  res <- f unit
  recordTime timerIx 50000 =<< endTimer
  return res

-- Trace functions
-- ===============

trs :: forall a. Show a => String -> a -> a
trs n a = trace (n++": "++show a) \_ -> a

ctrs :: forall a. Show a => Boolean -> String -> a -> a
ctrs b n a = if b
  then trace (n++": "++show a) \_ -> a
  else a

trh :: String -> Int -> Int
trh n a = trace (n++": "++toHexStr 2 a) \_ -> a

ctrh :: Boolean -> String -> Int -> Int
ctrh b n a = if b
  then trace (n++": "++toHexStr 2 a) \_ -> a
  else a

condTr :: forall a. Boolean -> (Unit -> String) -> a -> a
condTr false _ a = a
condTr true strF a = trace (strF unit) \_ -> a

-- Showing data structures
-- =======================

memStrRange :: Int -> Int -> S.Seq I8 -> String
memStrRange from to seq = 
  memStrRange' S.null S.take S.drop from to seq

memStrRange' :: forall f. (Foldable f, Show (f String)) =>
       (forall a. f a -> Boolean) -> (forall a. Int -> f a -> f a)
                                  -> (forall a. Int -> f a -> f a)
       -> Int -> Int -> f I8 -> String
memStrRange' nullF takeF dropF from to s =
  ramStr nullF takeF dropF s'
 where
  s' = takeF (to-from+1) <<< dropF from $ s

ramStr :: forall f. (Foldable f, Show (f String)) =>
       (forall a. f a -> Boolean) -> (forall a. Int -> f a -> f a)
                                  -> (forall a. Int -> f a -> f a)
       -> f I8 -> String
ramStr nullF takeF dropF s = T.snd $ helper (T.Tuple 0 "") hexd
 where
  hexd = s
  helper tup remain | nullF remain = tup
  helper (T.Tuple i acc) remain =
    helper (T.Tuple (i+bytesPerRow) acc') $ dropF bytesPerRow remain
   where
    acc' = acc ++ "\n" ++ toHexStr 4 i ++ showPacked (takeF bytesPerRow remain)
  bytesPerRow = 16
