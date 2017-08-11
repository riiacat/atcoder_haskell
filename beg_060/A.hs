-- fo "http://abc060.contest.atcoder.jp/tasks/abc060_a"

import Data.List
import Control.Applicative

main = do
  input <- getLine
  let inputWords = words input
      a = inputWords !! 0
      b = inputWords !! 1
      c = inputWords !! 2
  case (  (last a) == (head b) ) && ( (last b) == (head c)) of
    True -> putStr "YES"
    False -> putStr "NO"


