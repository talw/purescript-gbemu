module MemSection
  ( (!!)
  , getByIx
  , replace
  , getNew
  , fromIntArray
  ) where

import Control.Monad.Eff (Eff)

import Types (MemSection, MemAccess, I16, I8)

--MemSection is basically a javascript array with interface functions to allow
--it to be mutable (as long as the MemAccess effect is available).

foreign import getByIx :: forall e. MemSection -> I16
                       -> Eff (ma :: MemAccess | e) I8
foreign import replace :: forall e. I8 -> I16 -> MemSection
                       -> Eff (ma :: MemAccess | e) MemSection
foreign import getNew :: Int -> Int -> MemSection
foreign import fromIntArray :: Array Int -> MemSection

infixl 5 getByIx as !!
