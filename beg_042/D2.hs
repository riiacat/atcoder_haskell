-- fo "http://abc042.contest.atcoder.jp/tasks/arc058_a"

import Data.List
import Control.Applicative
import qualified Control.Monad.State as ST
import qualified Data.Map as M
type Ma = M.Map (Int,Int) Integer
type St = ST.State Ma Integer
  
main = do
  p <- getContents
  let fstline = words $ head $ lines p 
      h = read $ fstline !! 0 ::Integer
      w = read $ fstline !! 1 :: Integer
      a = read $ fstline !! 2 :: Integer
      b = read $ fstline !! 3 ::Integer
      m = M.empty :: Ma  
  putStr $  show h -- ToDo
  



getNum :: Ma  -> (Int, Int) -> Integer
getNum m (1,1) = 0
getNum m (1,2) = 1        
getNum m (2,1) = 1
getNum m (x,y) | re  == Nothing =  lef + abo
               | otherwise =  (\(Just x) -> x) re 
  where re = M.lookup (x,y) m
        lef =   getNum m ( (x - 1) , y ) 
        abo =   getNum m ( x, (y - 1) )

memorize :: Ma -> (Int, Int) -> Ma
memorize m (x,y) | M.lookup (x,y) m == Nothing = M.insert (x, y) re m
                 | otherwise = m
  where re = getNum m (x,y)

runMemo :: Ma -> (Int,Int) -> Ma      
runMemo m  (x, y)  = m   --f (m , (x,y))                     
  where xs = [1..x]
        ys = [1..y]
        
initiarize :: Ma -> Int -> Int -> Int -> Int -> Ma
--initiarize m h w a b = map ( M.insert undefined




