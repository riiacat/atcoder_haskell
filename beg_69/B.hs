{-# LANGUAGE OverloadedStrings #-} 
import Data.Text as T
import Data.Text.IO as TIO
import Data.Monoid

main = do
  input <- TIO.getLine
  let h = T.head input
      l = T.last input
      len = T.pack .show $ (T.length input - 2)
  TIO.putStr $ h `cons`  len `snoc`  l 
