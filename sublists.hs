-----------------------
-- getting all sublists
-----------------------
getSlicesOfSizeN:: [a] -> Int -> [[a]]
getSlicesOfSizeN [] n = []
getSlicesOfSizeN l  0 = [[]]
getSlicesOfSizeN (x:xs) n
    | n <= l_size = take n (x:xs) : getSlicesOfSizeN xs n
    | otherwise = []
    where l_size = length (x:xs)

unpackLists:: [[[a]]] -> [[a]]
unpackLists [] = []
unpackLists (x:xs) = x ++ unpackLists xs 

getAllSublists:: [a] -> [[a]]
getAllSublists [] = [[]]
getAllSublists l  = unpackLists [ getSlicesOfSizeN l n | n <- [0..length l] ]


------------------------
-- deleting all sublists
------------------------
findSublist:: (Eq) a => [a] -> [a] -> Int -> Int
findSublist []     sub_l i = -1
findSublist l      []    i = -1
findSublist (x:xs) sub_l i
    | length (x:xs) < length sub_l = -1
    | take (length sub_l) (x:xs) == sub_l = i
    | otherwise = findSublist xs sub_l (i+1)

delFromTo:: [a] -> Int -> Int -> [a]
delFromTo l from to = take from l ++ drop to l

delAllSublists:: (Eq) a => [a] -> [a] -> [a]
delAllSublists l [] = l
delAllSublists l sub_l
    | sub_l_ind >= 0 = delAllSublists (delFromTo l sub_l_ind to) sub_l
    | otherwise = l
    where sub_l_ind = findSublist l sub_l 0
          to = sub_l_ind + length sub_l