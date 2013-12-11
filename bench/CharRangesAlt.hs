{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE MagicHash #-}
module CharRangesAlt where

import           Data.Hashable
import           Data.List
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set

data Range = Single {-# UNPACK #-} !Char
           | Range  {-# UNPACK #-} !Char {-# UNPACK #-} !Char

instance Hashable Range where
    hashWithSalt s !(Single a) = hashWithSalt s a
    hashWithSalt s !(Range a _) = hashWithSalt s a


instance Eq Range where
    (==) !(Single x) !(Single y) = x == y
    (==) !(Single a) !(Range x y) = x <= a && a <= y
    (==) !(Range x y) !(Single a) = x <= a && a <= y
    (==) !(Range lx ux) !(Range ly uy) = (lx <= uy && ly <= ux)
                                         || (lx <= uy && ly <= ux)

instance Ord Range where
	(<=) !(Single x)  !(Single y)  = x <= y
	(<=) !(Single x)  !(Range y _) = x <= y
	(<=) !(Range _ x) !(Single y)  = x <= y
	(<=) !(Range _ x) !(Range y _) = x <= y

-- | Allows quick lookups using ranges.
toSet :: [Range] -> HashSet Range
toSet = Set.fromList . prepareRanges


prepareRanges :: [Range] -> [Range]
prepareRanges =  go . sort
  where
    go (r1:r2:rs) | Just r' <- maybeMergeRanges r1 r2 = go (r':rs)
                  | rss@(r3:rs') <- go (r2:rs) =
        case maybeMergeRanges r1 r3 of
            Nothing -> r1:rss
            Just r' -> r':rs'
    go rs = rs
{-# INLINE prepareRanges #-}

maybeMergeRanges :: Range -> Range -> Maybe Range
maybeMergeRanges x y = if x == y -- overlap
                       then Just $ minMax x y
                       else Nothing
{-# INLINE maybeMergeRanges #-}

minMax :: Range -> Range -> Range
minMax (Range lx ux) (Range ly uy) = Range (min lx ly) (max ux uy)
minMax (Single _) y = y
minMax x (Single _) = x
{-# INLINE minMax #-}

range :: Char -> Char -> Range
range x y = if x < y then Range x y
            else error "range: x not smaller than y"
{-# INLINE range #-}

single :: Char -> Range
single = Single
{-# INLINE single #-}
