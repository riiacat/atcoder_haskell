--from http://abc041.contest.atcoder.jp/assignments Question A

import Data.List

main = do 
  p <- getContents 
  let s = lines p !! 0  
      i = read $ lines p !! 1 ::Int  
  putStr $ [s !! (i-1)]
  