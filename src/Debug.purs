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
  , StatTimer
  ) where

import Prelude (class Show, Unit, ($), (+), (++), (-), (<<<), unit, show
               ,return, bind)
import Data.Sequence as S
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Data.Tuple as T

import Debug.Trace (spy, trace, traceA, traceAny, traceAnyA, traceAnyM
                   ,traceShow, traceShowA, traceShowM)
import Data.Foldable (class Foldable)
import Types (I8)
import Utils (showPacked, toHexStr)

-- Timer
-- =====

foreign import data StatTimer :: !

foreign import startTimer :: forall e. Eff (timer :: StatTimer | e) Unit
foreign import endTimer :: forall e. Eff (timer :: StatTimer | e) Int
foreign import recordTime :: forall e. Int -> Int -> Int
                          -> Eff (timer :: StatTimer | e) Unit

--Compute average running times for operations,
--segregated on a numeric id (e.g. averages can be computed separately for
--cb operations and regular operations)
timeIt :: forall e a. Int -> (Unit -> Eff (timer :: StatTimer | e) a)
       -> Eff (timer :: StatTimer | e) a
timeIt timerIx f = do
  startTimer
  res <- f unit
  recordTime timerIx 1 =<< endTimer
  return res

-- Trace functions
-- ===============

--Trace show
trs :: forall a. Show a => String -> a -> a
trs n a = trace (n++": "++show a) \_ -> a

--Conditional trace show
ctrs :: forall a. Show a => Boolean -> String -> a -> a
ctrs b n a = if b
  then trace (n++": "++show a) \_ -> a
  else a

--Trace hex
trh :: String -> Int -> Int
trh n a = trace (n++": "++toHexStr 2 a) \_ -> a

--Conditional trace hex
ctrh :: Boolean -> String -> Int -> Int
ctrh b n a = if b
  then trace (n++": "++toHexStr 2 a) \_ -> a
  else a

--Conditional trace of a separate object than the returned one.
condTr :: forall a. Boolean -> (Unit -> String) -> a -> a
condTr false _ a = a
condTr true strF a = trace (strF unit) \_ -> a

-- Stringify memory
-- ================

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
