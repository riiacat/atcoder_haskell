--from http://abc041.contest.atcoder.jp/assignments Question D
import Data.List

main = do
  p <- getContents
  let n = read $ words (lines p !! 0) !! 0 ::Int
      m = read $ words (lines p !! 0) !! 1 ::Int
      set = map ((map read).words) $ tail $ lines p ::[[Int]]
      re1 = getResult n set

-- n -> set -> result 
getResult :: Int -> [[Int]] -> [[Int]]
getResult n xss = map (judge_just xss) $ permutations [1..n]  

judge_just:: [[Int]] -> [Int] -> Just [Int]
judge_just xss ys | judge xss ys  = Just ys
                  | otherwise  = Nothing
                 
-- information -> list -> Bool
judge:: [[Int]] -> [Int] -> Bool
judge [x,y]:xss ys | ind_x < ind_y = True && (judge xss ys)
                   | otherwise = False
  where ind_x = elemIndex x xs
        ind_y = elemIndex y xs
judge [] ys = True

