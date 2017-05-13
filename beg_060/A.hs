-- fo "http://abc060.contest.atcoder.jp/tasks/abc060_a"

import Data.List
import Control.Applicative

main = do
  input <- getLine
    let inputWords = words input
        a = inputWords !! 1
        b = inputWords !! 2
        a = inputWords !! 3
  case  (last a) == (head b) && (last b) && (head c) of
    True -> putStr "YES"
    False -> putStr "NO"
        
