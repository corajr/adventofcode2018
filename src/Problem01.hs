module Problem01 where

import Data.Foldable (foldl')
import qualified Data.Set as Set

parseInput :: String -> [Int]
parseInput = map (read . f) . lines
  where
    f ('+':n) = n
    f xs = xs

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = f (0, Set.singleton 0) . cycle
  where
    f (x, s) (y:ys)
      | (x + y) `Set.member` s = x + y
      | otherwise = f (x + y, Set.insert (x + y) s) ys

cliMain :: IO ()
cliMain = do
  input <- readFile "inputs/01.txt"
  print $ part2 (parseInput input)
