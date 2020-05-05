score :: Char -> Char -> Int
score _ '-' = -2
score '-' _ = -2
score x y 
    | x == y = 1
    | otherwise = -1

simScore fullX@(x:xs) fullY@(y:ys) = maximum [simScore xs ys + score x y,
                                    simScore xs fullY + score x '-',
                                    simScore  fullX ys + score '-' y]

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f xs = [x | x <- xs, f x == maximum(map f xs)]
