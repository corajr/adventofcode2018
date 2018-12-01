module Problem01Spec (main, spec) where

import Test.Hspec

import Problem01

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f a b = it (show a ++ " ==> " ++ show b) $ f a `shouldBe` b

spec :: Spec
spec = do
  describe "part1" $ do
    let ex' = ex part1
    ex' [1, 1, 1] 3
    ex' [1, 1, -2] 0
    ex' [-1, -2, -3] (-6)
  describe "part2" $ do
    let ex' = ex part2
    ex' [1, -1] 0
    ex' [3, 3, 4, -2, -4] 10
    ex' [-6, 3, 8, 5, -6] 5
    ex' [7, 7, -2, -7, -4] 14
