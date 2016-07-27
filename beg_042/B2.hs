-- fo "http://abc042.contest.atcoder.jp/tasks/abc042_b"

import Data.List
import Control.Applicative

main = do
  p <- getContents
  let n = read $ head $ words $ head $ lines p :: Int
      m = read $ last $ words $ head $ lines p :: Int
      list  = tail $ lines p -- list !! i = S_i
      sorted = sort list
      result = concat sorted
  putStr $ result
  