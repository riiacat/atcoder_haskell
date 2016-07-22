
import Data.List

main = do
  p <- getLine
  let a = read $ words p !! 0 ::Integer
      b = read $ words p !! 1 ::Integer
      c = read $ words p !! 2 ::Integer
      x = a * b * c
  putStr $show $ x `mod` (10 ^ 9 + 7)

  