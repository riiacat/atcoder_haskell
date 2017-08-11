{-# LANGUAGE OverloadedStrings #-} 
import Data.Text.Lazy as T
import Data.Text.Lazy.IO as TIO
import Data.Monoid
import Data.Vector.Unboxed
import Data.List

main = do
  input <- TIO.getContents
  let ls = T.lines input 
      n = ls !! 0
      l = T.words $ ls !! 1
      (s0,s2,s4) = Data.List.foldl f (0,0,0) l
        where f  (s0,s2,s4) a  | ( (read $  T.unpack a) `mod` 4) == 0 = (s0, s2, s4 + 1)
                               | ( (read $  T.unpack a) `mod` 2 ) == 0 =  (s0, s2 + 1, s4)
                               | otherwise =  (s0 + 1, s2 , s4)
      getR (s0,s2,s4) | (s0<=s4) || ( s0 == s4+1 && s2 == 0 ) = "Yes"  
                      | otherwise = "No"
  Prelude.putStr $  getR (s0,s2,s4)
  
  --TIO.putStr $ h `cons`  len `snoc`  l 
