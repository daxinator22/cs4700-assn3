module Lists where
  -- Generates an unbounded list of numbers
  countingNumbers :: [Int]
  countingNumbers = [1..]

  -- Generates an unbounded list of even numbers
  evenNumbers :: [Int]
  evenNumbers = [ x | x <- [1..], (mod x 2) == 0]

  -- Generates an unbounded list of prime numbers
  primeNumbers :: [Int]
  primeNumbers = [x | x <- [2..], isPrime x]
  -- Determines whether an integer is prime
  isPrime :: Int -> Bool
  isPrime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else False

  -- Merges any 2 arrays
  merge :: [a] -> [a] -> [a]
  merge x y = x ++ y
 
  -- Wraps an array a give amount of steps
  wrap :: Int -> [a] -> [a]
  wrap k xs = merge (snd (splitAt k xs)) (fst (splitAt k xs))

  -- Gets a slice of an array, takes a tuple as range
  slice :: (Int, Int) -> [a] -> [a]
  slice range x = take ((snd range) - (fst range) + 1) (drop ((fst range) - 1) x)
  
  -- Will return an array of lists, where the nth sub-list is element 1 to n of the original list
  subLists :: [a] -> [[a]]
  subLists [] = [[]]
  subLists x = breakLists x 1
  -- Gets the 1 to n elements of original list, recurse from 1 to length of list
  breakLists :: [a] -> Int -> [[a]]
  breakLists sample stage | stage > (length sample) = []
  breakLists sample stage =
    do
      let recurse = breakLists sample (stage + 1)
      let result = [[]] ++ [take stage sample]
      tail (result ++ recurse)

  -- Counts the elements in a list of lists 
  countElements :: [[Int]] -> Int
  countElements [[]] = 0
  countElements lists = countLists lists 0
  -- Recurses through a list of list and counts the elements in each list
  countLists :: [[Int]] -> Int -> Int
  countLists lists ele | ele >= length lists = 0
  countLists lists ele = length (lists !! ele) + countLists lists (ele + 1)

  -- Sort a list of lists of integers by sum of lists
  sortSubLists :: [[Int]] -> [[Int]]
  sortSubLists [] = []
  sortSubLists lists = quickSort lists
  -- Sums up the elements in list
  sumLists :: [Int] -> Int -> Int
  sumLists lists ele | ele >= (length lists) = 0
  sumLists lists ele = lists !! ele + sumLists lists (ele + 1)

  -- Performs a quick sort on a list of lists of integers
  quickSort :: [[Int]] -> [[Int]]
  quickSort [] = []
  quickSort list =
    do
      let pivot = list !! 0
      let sliced = tail list
      let less = lessThan sliced pivot 0
      let greater = greaterThan sliced pivot 0
      quickSort less ++ [pivot] ++ quickSort greater

  -- Returns all the elements less than a given pivot
  lessThan :: [[Int]] -> [Int] -> Int -> [[Int]]
  lessThan list pivot ele | ele >= length list = []
  lessThan list pivot ele = if sumLists (list !! ele) 0 < sumLists pivot 0 then [list !! ele] ++ (lessThan list pivot (ele + 1)) else lessThan list pivot (ele + 1)
 
  -- Returns all the elements greater than a given pivot
  greaterThan :: [[Int]] -> [Int] -> Int -> [[Int]]
  greaterThan list pivot ele | ele >= length list = []
  greaterThan list pivot ele = if sumLists (list !! ele) 0 >= sumLists pivot 0 then [list !! ele] ++ (greaterThan list pivot (ele + 1)) else greaterThan list pivot (ele + 1)
 
  -- Applies a function to the elements of a list
  listApply :: (Foldable t, Num t1) => (a -> t1 -> t1) -> [t a] -> [t1]
  listApply func [] = []
  listApply func (x:xs) = foldr func 0 x:listApply func xs
  
  -- Passes an argument to an array of functions
  composeList :: [t -> t] -> t -> t
  composeList [] arg = arg
  composeList list arg = doFunctions list arg 0
  -- Recurses through an array of functions and passes the argument
  doFunctions :: [t -> t] -> t -> Int -> t
  doFunctions list arg ele = if ele == (length list) - 1 then (list !! ele) arg else (doFunctions list ((list !! ele) arg) (ele + 1))
