-- fo "http://abc042.contest.atcoder.jp/tasks/arc058_a"

import Data.List
import Control.Applicative

main = do
  p <- getContents
  let fstline = words $ head $ lines p 
      h = read $ fstline !! 0 ::Integer
      w = read $ fstline !! 1 :: Integer
      a = read $ fstline !! 2 :: Integer
      b = read $ fstline !! 3 ::Integer
      
  putStr $  show $ getR h w a b


getR h w a b = zipWith (*) (getHLenList h a b ) second
  where getHLenList h a b = map ( flip getLen b) [1..(h-a)]
  
getLen h w = myCombination   ( h - 1 + w -1 )  $  min  (h - 1) (w -1) 
myCombination :: Integer -> Integer -> Integer
myCombination n k = (fact n ) `div` ( (fact k) * (fact (n-k) ) )

fact :: Integer -> Integer
fact 1 = 1
fact 0 = 1
fact n = n * fact (n-1)


         