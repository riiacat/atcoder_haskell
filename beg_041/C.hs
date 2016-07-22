--from http://abc041.contest.atcoder.jp/assignments Question C
import Data.List

main = do
  p <- getContents
  let n = read $ lines p !! 0 ::Integer
      a_set = map read $ words $ lines p !! 1 ::[Integer]
      pair = zipWith (\x y -> (x,y)) a_set [1..n]
      sorted = map (show.snd) $ sortBy (\x y -> compare (fst y) (fst x) ) pair   
  mapM_ putStrLn sorted 


  

  