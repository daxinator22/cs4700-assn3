module Lists where
  countingNumbers :: [Int]
  countingNumbers = [1..]

  evenNumbers :: [Int]
  evenNumbers = [ x | x <- [1..], (mod x 2) == 0]

  merge :: [a] -> [a] -> [a]
  merge x y = x ++ y
 
  wrap :: Int -> [a] -> [a]
  wrap k xs = merge (snd (splitAt k xs)) (fst (splitAt k xs))
 
  slice :: (Int, Int) -> [a] -> [a]
  slice range x = take ((snd range) - (fst range) + 1) (drop ((fst range) - 1) x)

  subLists :: [a] -> [[a]]
  subLists [] = [[]]
  subLists x = breakLists x 1
  breakLists :: [a] -> Int -> [[a]]
  breakLists sample stage | stage > (length sample) = []
  breakLists sample stage =
    do
      let recurse = breakLists sample (stage + 1)
      let result = [[]] ++ [take stage sample]
      tail (result ++ recurse)

  countElements :: [[Int]] -> Int
  countElements [[]] = 0
  countElements lists = countLists lists 0
  countLists :: [[Int]] -> Int -> Int
  countLists lists ele | ele >= length lists = 0
  countLists lists ele = length (lists !! ele) + countLists lists (ele + 1)

  sortSubLists :: [[Int]] -> [[Int]]
  sortSubLists [] = []
  sortSubLists lists = quickSort lists
  replaceList :: [[Int]] -> Int -> [Int]
  replaceList list ele | ele >= length list = [0]
  replaceList list ele = [sumLists (list !! ele) 0] ++ replaceList list (ele + 1)
  sumLists :: [Int] -> Int -> Int
  sumLists lists ele | ele >= (length lists) = 0
  sumLists lists ele = lists !! ele + sumLists lists (ele + 1)

  quickSort :: [[Int]] -> [[Int]]
  quickSort [] = []
  quickSort list =
    do
      let pivot = list !! 0
      let sliced = tail list
      let less = lessThan sliced pivot 0
      let greater = greaterThan sliced pivot 0
      quickSort less ++ [pivot] ++ quickSort greater

  lessThan :: [[Int]] -> [Int] -> Int -> [[Int]]
  lessThan list pivot ele | ele >= length list = []
  lessThan list pivot ele = if sumLists (list !! ele) 0 < sumLists pivot 0 then [list !! ele] ++ (lessThan list pivot (ele + 1)) else lessThan list pivot (ele + 1)
 
  greaterThan :: [[Int]] -> [Int] -> Int -> [[Int]]
  greaterThan list pivot ele | ele >= length list = []
  greaterThan list pivot ele = if sumLists (list !! ele) 0 >= sumLists pivot 0 then [list !! ele] ++ (greaterThan list pivot (ele + 1)) else greaterThan list pivot (ele + 1)
 
