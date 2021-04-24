countInList:: (Eq a) => a -> [a] -> Int
countInList el [] = 0
countInList el (x:xs) 
    | (el == x) = 1 + countInList el xs
    | otherwise = countInList el xs

nonuniqueStrings:: [String] -> [String]
nonuniqueStrings [] = []
nonuniqueStrings (s:l)
    | (s `countInList` l) > 1 = s : nonuniqueStrings [x | x<-l, x/=s]
    | otherwise    = nonuniqueStrings l

main = interact (unlines. nonuniqueStrings . lines)