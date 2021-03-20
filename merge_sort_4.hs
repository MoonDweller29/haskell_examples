slice:: [a] -> Int -> Int -> [a]
slice l begin size = take size (drop begin l)

upper_div:: Int -> Int -> Int
upper_div x y = (x + y - 1) `div` y

split_4:: [a] -> ([a], [a], [a], [a])
split_4 l = (
        slice l 0 part_size,
        slice l part_size part_size,
        slice l (part_size*2) part_size,
        slice l (part_size*3) part_size
    ) where part_size = (length l) `upper_div` 4

minhead2:: (Ord) a => [a] -> [a] -> [a]
minhead2 [] [] = []
minhead2 l1 [] = l1
minhead2 [] l2 = l2
minhead2 (x:xs) (y:ys)
    | x < y     = x:xs
    | otherwise = y:ys

minhead4:: (Ord) a => ([a], [a], [a], [a]) -> a
minhead4 (l1, l2, l3, l4) = head (minhead2 (minhead2 l1 l2) (minhead2 l3 l4))

headIsEq:: (Ord) a => [a] -> a -> Bool
headIsEq [] el = False
headIsEq (x:xs) el = x == el

merge:: (Ord) a => ([a], [a], [a], [a]) -> [a]
merge ([],[],[],[]) = []
merge (l1,[],[],[]) = l1
merge (l1,l2,l3,l4)
    | headIsEq l1 min_head = head l1 : merge ((tail l1), l2, l3, l4)
    | headIsEq l2 min_head = head l2 : merge (l1, (tail l2), l3, l4)
    | headIsEq l3 min_head = head l3 : merge (l1, l2, (tail l3), l4)
    | headIsEq l4 min_head = head l4 : merge (l1, l2, l3, (tail l4))
    where min_head = minhead4 (l1, l2, l3, l4)

mergeSort4:: (Ord) a => [a] -> [a]
mergeSort4 [] = []
mergeSort4 [x] = [x]
mergeSort4 l = merge (mergeSort4 l1, mergeSort4 l2, mergeSort4 l3, mergeSort4 l4)
    where (l1, l2, l3, l4) = split_4 l