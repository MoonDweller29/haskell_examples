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
