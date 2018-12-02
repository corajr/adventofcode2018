module Problem02Spec (main, spec) where

import Data.Map.Strict (fromList)
import Test.Hspec

import Problem02

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f a b = it (show a ++ " ==> " ++ show b) $ f a `shouldBe` b

spec :: Spec
spec = do
  describe "counts" $ do
    let ex' = ex counts
    ex' "abc" (fromList [('a', 1), ('b', 1), ('c', 1)])
    ex' "aaabbc" (fromList [('a', 3), ('b', 2), ('c', 1)])
  describe "hasCount" $ do
    let ex' = ex (hasCount 2 . counts)
    ex' "abc" 0
    ex' "aabc" 1
    ex' "aabbc" 1
  describe "part1" $ do
    let ex' = ex part1
    ex' ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee",  "ababab"] 12
  describe "diff" $ do
    let ex' = ex diff
    ex' ("abcde", "axcye") (2, "ace")
    ex' ("fghij", "fguij") (1, "fgij")
  describe "part2" $ do
    let ex' = ex part2
    ex' ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"] "fgij"
