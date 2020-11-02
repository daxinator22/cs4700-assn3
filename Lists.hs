module Lists where

  countingNumbers = [1..]

  evenNumbers = [ x | x <- [1..], (mod x 2) == 0]

  isPrime k = [x | x <- [2..k - 1], k `mod` x == 0] 
 
  merge x y = x ++ y
 
  wrap k xs = merge (snd (splitAt k xs)) (fst (splitAt k xs))
 
  slice range x = take ((snd range) - (fst range) + 1) (drop ((fst range) - 1) x)

  subLists :: [Integer] -> [[Integer]]
  subLists [] = [[]]
  subLists x = breakLists x 1
  breakLists sample stage | stage > (length sample) = []
  breakLists sample stage =
    do
      let recurse = breakLists sample (stage + 1)
      let result = [[]] ++ [take stage sample]
      tail (result ++ recurse)

  countElements [[]] = 0
  countElements lists = sumLists lists 0
  sumLists lists ele | ele >= length lists = 0
  sumLists lists ele = length (lists !! ele) + sumLists lists (ele + 1)

  -- I've left this sample definition in here, you should delete it
  oddNumbers :: Int -> [Int]
  oddNumbers n = [ x | x <- [1..n], (mod x 1) == 0] 

