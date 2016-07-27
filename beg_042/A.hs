import Data.List
import Control.Applicative

main = do
  p <- getLine
  let list = map read $ words p :: [Int]
      sorted = sort list
      result x | x == [5,5,7] = "YES"
               | otherwise = "NO"
  putStr $ result sorted 
