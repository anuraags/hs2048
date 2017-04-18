module Math where

absolute :: Int -> Int
absolute n
  | n < 0 = negate n
  | otherwise = n