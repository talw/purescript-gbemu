module Utils where

import Prelude
import Data.Sequence as S
import Data.Array as A
import Data.Maybe
import Data.Tuple
import Data.Unfoldable as U

foreign import toHexStr :: Int -> Int -> String
foreign import fromHexStr :: String -> Int

--NOTES  replace fromMaybe with something that will log invalid indices

getFromSeq :: forall a. a -> Int -> S.Seq a -> a
getFromSeq def ix seq = fromMaybe def $ S.index ix seq

--Order of args is reversed between the two versions, for !! to be more natural
getFromSeqNoDef :: S.Seq Int -> Int -> Int
getFromSeqNoDef seq ix = fromMaybe 0 $ S.index ix seq

infixl 5 getFromSeqNoDef as !!

seqToArray :: forall a. S.Seq a -> Array a
seqToArray = U.unfoldr S.uncons

cmp2 :: forall a b c d. (c -> d) -> (a -> b -> c) -> (a -> b -> d)
cmp2 g f a b = g $ f a b

--NOTES  replace fromMaybe with something that will log invalid indices
{--arrIx :: Array Int -> Int -> Int--}
{--arrIx arr ix = fromMaybe 0 $ A.index arr ix--}
