module MemSection
  ( (!!)
  , getByIx
  , replace
  , getNew
  , fromIntArray
  ) where

import Prelude
import Data.Foldable
import Control.Monad.Eff

import Types
import Debug

foreign import getByIx :: forall e. MemSection -> I16 -> Eff (ma :: MemAccess | e) I8
foreign import replace :: forall e. I8 -> I16 -> MemSection -> Eff (ma :: MemAccess | e) MemSection
foreign import getNew :: forall e. Int -> Int -> MemSection
foreign import fromIntArray :: forall e. Array Int -> MemSection

infixl 5 getByIx as !!

{--main = do--}
  {--arr <- M.getNew 3--}
  {--val1<- M.rd8 0 arr--}
  {--val2<- M.rd8 1 arr--}
  {--val3<- M.rd8 2 arr--}
  {--M.wr8 5 0 arr--}
  {--M.wr8 6 1 arr--}
  {--M.wr8 7 2 arr--}
  {--log $ show val1 ++ show val2 ++ show val3--}

{--main = do--}
  {--let arr = M.fromIntArray [5,3,2]--}
  {--[>let arr = M.getNew 3 0<]--}
  {--let x = { a : arr }--}
  {--val1 <- x.a M.!! 1--}
  {--log $ show val1--}
  {--val2 <- x.a M.!! 2--}
  {--log $ show val2--}

