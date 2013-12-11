module Main where

import           Criterion.Main
import qualified Data.Set as Set
import           Text.StringPrep
import           Text.StringPrep.Profiles

import qualified Text.CharRanges as CR
import qualified CharRangesAlt as CRA
import qualified Ranges as R
import qualified Text.FastSet as FS
import qualified Text.FastRangeSet as FRS
import qualified Data.HashSet as HashSet


testString = "X\xC3\x9F\xe3\x8c\x96\xC4\xB0\xE2\x84\xA1\xE2\x92\x9F\xE3\x8c\x80xss\xe3\x82\xad\xe3\x83\xad\xe3\x83\xa1\xe3\x83\xbc\xe3\x83\x88\xe3\x83\xab"


nameprepCharRanges = concat . prohibited $ namePrepProfile True

toRange (CR.Single a) = R.Single a
toRange (CR.Range  a b) = R.Range a b

toCharRangeAlt (CR.Single a) = CRA.Single a
toCharRangeAlt (CR.Range  a b) = CRA.Range a b

nameprepRanges = map toRange nameprepCharRanges
nameprepCharRangesAlt = map toCharRangeAlt nameprepCharRanges

charList = concatMap rangeToList nameprepCharRanges
  where
    rangeToList (CR.Single a) = [a]
    rangeToList (CR.Range x y) = [x..y]

getLB (CR.Single a) = a
getLB (CR.Range a _) = a

bench_nameprepCharRangesToSet = whnf CR.toSet nameprepCharRanges
bench_nameprepRangesToSet = whnf (R.toSet. R.ranges) nameprepRanges
bench_nameprepRangesNewToSet = whnf R.newToSet nameprepRanges
bench_nameprepCharRangesAltToSet = whnf CRA.toSet nameprepCharRangesAlt
bench_nameprepFastSetFromList = whnf FS.fromList charList
bench_nameprepFastRangeSettoSet = whnf FRS.toSet nameprepCharRanges

bench_toSet = bgroup "Set creation"
    [ -- bench "Ranges toSet" bench_nameprepRangesToSet
      bench "Ranges with improved toSet" bench_nameprepRangesNewToSet
    , bench "CharRanges toSet" bench_nameprepCharRangesToSet
    , bench "CharRanges Hash toSet" bench_nameprepCharRangesAltToSet
--    , bench "FastSet fromList" bench_nameprepFastSetFromList
    , bench "FastRangeSet toSet" bench_nameprepFastRangeSettoSet
    ]

lookupChars :: [Char]
lookupChars = ['\100000'..'\100100'] --  map getLB nameprepCharRanges

bench_nameprepRangesLookup = nf (map (\x -> Set.member (R.Single x) set))
                                     lookupChars
  where
    set = R.toSet $ R.ranges nameprepRanges

bench_nameprepCharRangesLookup = nf (map (\x -> Set.member (CR.Single x) set))
                                     lookupChars
  where
    set = CR.toSet nameprepCharRanges

bench_nameprepCharRangesAltLookup = nf (map (\x -> HashSet.member (CRA.Single x) set))
                                     lookupChars
  where
    set = CRA.toSet nameprepCharRangesAlt

bench_nameprepFastRangeSetLookup = nf (map (\x -> FRS.member x set)) lookupChars
  where
    set = FRS.toSet nameprepCharRanges

bench_nameprepFastSetLookup = nf (map (\x -> FS.member x set)) lookupChars
  where
    set = FS.fromList charList

bench_lookup = bgroup "member lookup"
               [ bench "Ranges" bench_nameprepRangesLookup
               , bench "Char Ranges" bench_nameprepCharRangesLookup
               , bench "Char Ranges Hash" bench_nameprepCharRangesAltLookup
               , bench "Fast Range Set" bench_nameprepFastRangeSetLookup
--               , bench "Fast Set" bench_nameprepFastSetLookup
               ]

main = do
    print $ length charList

    defaultMain [ bench_toSet,
                 bench_lookup
                ]
