max' :: (Ord a) => a -> a -> a
max' a b 
    | a > b = a
    | otherwise = b

sumSqRec :: (Num p, Eq p) => p -> p
sumSqRec 0 = 0
sumSqRec n = n ^ 2 + sumSqRec (n - 1)

sumSqMap :: (Num a) => [a] -> a
sumSqMap = sum . map (^2)

hanoi :: (Num a, Ord a) => a -> a
hanoi n 
    | n <= 0 = 0
    | otherwise = hanoi (n -  1) * 2 + 1

smallestFactor :: Integer -> Integer
smallestFactor = nextFactor 1

nextFactor k n 
    | k == n = k
    | mod n (k + 1) == 0 = k + 1
    | otherwise = nextFactor (k + 1) n

multiply :: (Num a) => [a] -> a
multiply = foldl1 (*)