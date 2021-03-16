{-test1:: Int->Int
test1 x = x + x

max1:: (Ord) a => a -> a -> a
max1 x y
    | x <= y =y
    | otherwise = x

-- test4:: Int->String
-- test4 1 = "one"
-- test4 2 = "two"
-- test4 x = error "out of range"

max3:: (Ord) a => a -> a -> a -> a
max3 x y z
    | max x y <= z =z
    | max x z <= y =y
    | otherwise =x
-}

delLast:: [a] -> [a]
delLast [] = error "EMPTY LIST"
delLast [x] = []
delLast (x:xs) = x : delLast xs

swapHeadTail:: [a] -> [a]
swapHeadTail [] = error "EMPTY LIST"
swapHeadTail [x] = [x]
swapHeadTail [x, y] = [y, x]
swapHeadTail (x:xs) = last xs : delLast xs : [x]
