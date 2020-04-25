length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

initials' :: String -> String -> String
initials' fullfirst@(c:cs) fulllast@(x:xs) = "Initials of " ++ fullfirst ++ " " ++ fulllast ++ " is: " ++ f ++ l
    where f = [c]
          l = [x]

nestleWhere :: Int
nestleWhere = a2
    where
        a2 = a * a
            where a = 2

bmiList :: (RealFloat a) => [(a,a)] -> [a]
bmiList xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]

chain :: (Integral a) => a -> [a]
chain x 
    | x == 1 = [x]
    | even x = x : chain (x `div` 2)
    | otherwise = x : chain (x * 3 + 1)