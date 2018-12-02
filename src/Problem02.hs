module Problem02 where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map

type CharCount = Map.Map Char Int

counts :: String -> CharCount
counts = Map.unionsWith (+) . map (\x -> Map.singleton x 1)

hasCount :: Int -> CharCount -> Int
hasCount i = f . Map.elems
  where
    f [] = 0
    f (x:xs) = if x == i then 1 else f xs

part1 :: [String] -> Int
part1 = g . foldl' f (0, 0) . map ((hasCount 2 &&& hasCount 3) . counts)
  where
    f (a, b) (c, d) = (a + c, b + d)
    g (a, b) = a * b

diff :: (String, String) -> (Int, String)
diff = go (0, "")
  where
    go (i, s) ([], _) = (i, reverse s)
    go (i, s) (_, []) = (i, reverse s)
    go (i, common) (x:xs, y:ys)
      | x == y = go (i, x:common) (xs, ys)
      | otherwise = go (i + 1, common) (xs, ys)

part2 :: [String] -> String
part2 xs = head $ do
  x <- xs
  y <- xs
  let (i, s) = diff (x, y)
  guard $ i == 1
  pure s

cliMain :: IO ()
cliMain = do
  input <- readFile "inputs/02.txt"
  print $ part2 (lines input)
