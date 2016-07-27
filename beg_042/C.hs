-- fo "http://abc042.contest.atcoder.jp/tasks/arc058_a"

import Data.List
import Control.Applicative

main = do
  p <- getContents
  let n = read $ head $ words $ head $ lines p :: Int
      k = read $ last $ words $ head $ lines p :: Int
      d  =   map ( flip (!!) 0 ) $ words $last  $ lines p -- d !! i = D_i
      pre = [n..]
      result = ( find (myNotElem d )  pre ) 
  putStr $  show result


myNotElem :: [Char] -> Int -> Bool  
myNotElem  (s:st) n | s `notElem` n' = myNotElem st n
                   | otherwise = False
  where n' = show n
myNotElem [] _ = True        