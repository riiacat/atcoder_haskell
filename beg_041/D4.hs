--from http://abc041.contest.atcoder.jp/assignments Question D
-- Time Over in this algorithm
-- memorize するぞ！！
import Data.List
import Control.Applicative

main = do
  p <- getContents
  let n = read $ words (lines p !! 0) !! 0 ::Int
      m = read $ words (lines p !! 0) !! 1 ::Int
      set = map ((map read).words) $ tail $ lines p ::[[Int]]
  putStr $ show $ length $ memorizedGetR n set
  
--メモ化するぞい
memorizedGetR:: Int -> [[Int]] -> [[Int]]
memorizedGetR n set = ( [ (flip getR') set n | n <- [0..] ] !! n )
  where getR':: Int -> [[Int]] -> [[Int]]
        getR' 0 set = []
        getR' 1 set = []
        getR' 2 set = judgeR set $ permutations [1,2]
        getR' n set = judgeR set judgedList 
          where  judgedList = insertedlist n $ memorizedGetR (n-1) set 

           
getSumR :: Int -> [[Int]] -> Int
getSumR n set = length $ getR n set 
getR:: Int -> [[Int]] -> [[Int]]
getR 2 set = judgeR set $ permutations [1,2]
getR n set = judgeR set' judgedList 
             where  judgedList = insertedlist n $ getR (n-1) $filter (\(x:[y]) -> x < n && y<n) set
                    set' = filter (\(x:[y]) -> x >= n || y>=n) set
insertedlist :: Int -> [[Int]] -> [[Int]]
insertedlist n xss =  (map (\x ->  insertAt x n) [0..(n-1)] ) <*> xss        
--n個目の後ろに挿入
insertAt :: Int -> a -> [a] -> [a]  
insertAt n x xs =  before ++ (x : after)
  where (before , after) = splitAt n xs
--        
judgeR :: [[Int]] -> [[Int]] -> [[Int]]
judgeR [] xss = xss
judgeR set (xs:xss) | judge set xs = xs : judgeR set xss
                    | otherwise = judgeR set xss
judgeR set [] = []
  
  
------------------------  
getResultForPrint :: [Maybe [Int]] -> [[Int]]
getResultForPrint (Nothing :xs)  = getResultForPrint xs
getResultForPrint (Just s : xs)  = s : getResultForPrint xs
getResultForPrint [] = []

getResult' :: Int -> [[Int]] -> Int                       
getResult' n xss = sum $ map ( judge_int xss ) $ permutations [1..n] 
judge_int :: [[Int]] -> [Int] -> Int
judge_int xss xs | judge xss xs = 1 --judge or judge'
                 | otherwise = 0
-- n -> set -> result             
getResult :: Int -> [[Int]] -> [Maybe [Int]]
getResult n xss = map (judge_just xss) $ permutations [1..n]  

judge_just:: [[Int]] -> [Int] -> Maybe [Int]
judge_just xss ys | judge xss ys  = Just ys
                  | otherwise  = Nothing
                 
-- information -> list -> Bool
judge :: [[Int]] -> [Int] -> Bool
judge ( (x:[y]):xss) ys | (ind_x < ind_y) = True && (judge xss ys)
                        | otherwise = False
  where ind_x = elemIndex x ys
        ind_y = elemIndex y ys
judge [] ys = True






