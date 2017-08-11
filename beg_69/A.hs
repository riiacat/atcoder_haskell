-- fo "http://abc060.contest.atcoder.jp/tasks/abc060_a"

main = do
  input <- getLine
  let inputWords = words input
      n :: Int
      m :: Int
      n = read $ inputWords !! 0
      m = read $ inputWords !! 1
  putStr . show $  (n-1) * (m-1)

