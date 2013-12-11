-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.FastSet
-- Copyright   :  Felipe Lessa 2010, Bryan O'Sullivan 2008
-- License     :  BSD3
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Fast set membership tests for 'Char' values.  The set
-- representation is unboxed for efficiency.  We test for
-- membership using a binary search.
--
-----------------------------------------------------------------------------
module Text.FastRangeSet
    (
    -- * Data type
      FastSet
    -- * Construction
    , toSet
    -- * Lookup
    , member
    ) where

import Data.List (sortBy)
import qualified Data.Array.Base as AB
import qualified Data.Array.Unboxed as A
import qualified Data.Text as T

import qualified Text.CharRanges as CR

newtype FastSet = FastSet (A.UArray Int Char)
    deriving (Eq, Ord, Show)

-- | Create a set.
toSet :: [CR.Range] -> FastSet
toSet t = mkSet (2 * length t) . rangesToChars $ CR.prepareRanges t
  where
    rangesToChars [] = []
    rangesToChars (CR.Single a  : rs) = a : a : rangesToChars rs
    rangesToChars (CR.Range a b : rs) = a : b : rangesToChars rs

mkSet :: Int -> [Char] -> FastSet
mkSet l = FastSet . A.listArray (0,l-1)

-- | Check the set for membership.
member :: Char -> FastSet -> Bool
member c (FastSet a) = let (0, h) = A.bounds a in search 0 (h `quot` 2)
    where search lo hi
              | hi < lo = False
              | otherwise =
                  let mid = (lo + hi) `quot` 2
                  in case rangeCmp c (mid*2) of
                       GT -> search (mid + 1) hi
                       LT -> search lo (mid - 1)
                       _ -> True
          rangeCmp x i = case compare x (AB.unsafeAt a i) of
              LT -> LT
              EQ -> EQ
              GT -> case compare x (AB.unsafeAt a (i + 1)) of
                  GT -> GT
                  _ -> EQ
          {-# INLINE rangeCmp #-}
