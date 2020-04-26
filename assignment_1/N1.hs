import Debug.Trace

debug = flip trace

-- Given utility functions
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a
    
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs


-- Own implemented functions
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wc (l:ls) sub
    | wc == l = sub ++ (substitute wc ls sub)
    | otherwise = l : (substitute wc ls sub)

-- match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing `debug` "match got empty p"
match _ _ [] = Nothing `debug` "match got empty s"

match wc (p:pp) (s:ss)
    | p == wc = if(singleWildcardMatch (p:pp) (s:ss) /= Nothing) then (singleWildcardMatch (p:pp) (s:ss)) else (longerWildcardMatch (p:pp) (s:ss))
    | p == s = match wc pp ss
    | otherwise = Nothing

singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)

longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)

transformationApply wc f w (p1, p2) = mmap (substitute wc p2 . f) (match wc p1 w)