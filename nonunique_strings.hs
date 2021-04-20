inList:: (Eq a) => a -> [a] -> Bool
inList el [] = False
inList el (x:xs) = (el == x) || inList el xs

nonuniqueStrings:: [String] -> [String]
nonuniqueStrings [] = []
nonuniqueStrings (s:l)
    | s `inList` l = s : nonuniqueStrings [x | x<-l, x/=s]
    | otherwise    = nonuniqueStrings l

main = interact (unlines. nonuniqueStrings . lines)