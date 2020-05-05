type AlignmentType = (String,String)

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

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments (s:ps) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (p:ps) = attachHeads '-' y (optAlignments [] ys)
optAlignments (s:ss) (p:ps) = maximaBy (uncurry similiarityScore) branches
                           where branches = branch1 ++ branch2 ++ branch3
                                 branch1 = attachHeads '-' p (optAlignments (s:ss) ps)
                                 branch2 = attachHeads s '-' (optAlignments ss (p:ps))
                                 branch3 = attachHeads s p (optAlignments ss ps)

outputOptAlignments p s = showAlignments len alignments
                        where alignments = optAlignments p s
                              len        = length alignments

showAlignments :: Int -> [(String, String)] -> IO()
showAlignments n al = putStrLn $ formatAlignments al ++ "There are " ++ show n ++ " optimal alignment(s)!"

formatAlignments :: [AlignmentType] -> String
formatAlignments = concatMap (\(s,p) -> formatString s ++ "\n" ++ formatString p ++ "\n\n")

formatString :: String -> String
formatString "" = ""
formatString (s:ss) = s : " " ++ formatString ss

mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> Int
    mcsEntry _ 0 = 0
    mcsEntry 0 _ = 0
    mcsEntry i j
      | x == y    = 1 + mcsLen (i-1) (j-1)
      | otherwise = max (mcsLen i (j-1)) 
                        (mcsLen (i-1) j)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)