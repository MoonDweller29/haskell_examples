delAllEq:: (Ord) a => [a]->a -> [a]
delAllEq [] el = []
delAllEq (x:xs) el
    | x == el = delAllEq xs el
    | otherwise = x : delAllEq xs el

elemExists:: (Ord) a => [a]->a->Bool
elemExists [] el = False
elemExists (x:xs) el
    | x == el = True
    | otherwise = elemExists xs el

delDuplicates:: (Ord) a => [a]->[a]
delDuplicates [] = []
delDuplicates (x:xs) = x : delDuplicates (delAllEq xs x)

getUniqueElems:: (Ord) a => [a]->[a]
getUniqueElems [] = []
getUniqueElems (x:xs)
    | elemExists xs x = getUniqueElems (delAllEq xs x)
    | otherwise = x : getUniqueElems xs