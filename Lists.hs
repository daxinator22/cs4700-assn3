module Lists where

  countingNumbers = [1..]
  evenNumbers = [ x | x <- [1..], (mod x 2) == 0]
  isPrime k = [x | x <- [2..k - 1], k `mod` x == 0] 
  merge x y = x ++ y
  slice range x = take ((snd range) - (fst range) + 1) (drop ((fst range) - 1) x)

  -- I've left this sample definition in here, you should delete it
  oddNumbers :: Int -> [Int]
  oddNumbers n = [ x | x <- [1..n], (mod x 1) == 0] 

