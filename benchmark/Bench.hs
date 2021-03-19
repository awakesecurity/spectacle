module Main
  ( main,
  )
where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Hashable (hash)
import GHC.Exts (fromList)

import Data.Fingerprint (Fingerprint)
import qualified Data.Fingerprint as Fingerprint
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Strict as IntMap'

-- -----------------------------------------------------------------------------

main :: IO ()
main =
  let xs = [1 .. 20000]
   in defaultMain
        [ bgroup
            "lookup"
            [ bench "Data.Fingerprint" $ whnf (lookupFP xs) xs
            , bench "Data.HashMap.Strict" $ whnf (lookupHM xs) xs
            , bench "Data.IntMap" $ whnf (lookupIM xs) xs
            , bench "Data.IntMap.Strict" $ whnf (lookupIM' xs) xs
            ]
        , bgroup
            "insertion"
            [ bench "Data.Fingerprint" $ whnf insertionFP xs
            , bench "Data.HashMap.Strict" $ whnf insertionHM xs
            , bench "Data.IntMap" $ whnf insertionIM xs
            , bench "Data.IntMap.Strict" $ whnf insertionIM' xs
            ]
        ]

lookupFP :: [Int] -> [Int] -> [Bool]
lookupFP xs ys =
  map (`Fingerprint.member` (fromList (map (fromIntegral) xs))) ys

lookupHM :: [Int] -> [Int] -> [Bool]
lookupHM xs ys = map (`HashMap.member` (HashMap.fromList (zip xs xs))) ys

lookupIM :: [Int] -> [Int] -> [Bool]
lookupIM xs ys = map (`IntMap.member` (IntMap.fromList (zip xs xs))) ys

lookupIM' :: [Int] -> [Int] -> [Bool]
lookupIM' xs ys = map (`IntMap'.member` (IntMap'.fromList (zip xs xs))) ys

insertionFP :: [Int] -> Fingerprint
insertionFP xs = foldl (flip (Fingerprint.insert . fromIntegral . hash)) Fingerprint.empty xs

insertionHM :: [Int] -> HashMap.HashMap Int ()
insertionHM xs = foldl (flip (`HashMap.insert` ())) mempty xs

insertionIM :: [Int] -> IntMap.IntMap ()
insertionIM xs = foldl (flip (`IntMap.insert` ())) mempty xs

insertionIM' :: [Int] -> IntMap'.IntMap ()
insertionIM' xs = foldl (flip (`IntMap'.insert` ())) mempty xs
