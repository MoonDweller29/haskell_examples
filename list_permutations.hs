in_list:: (Eq a) => a -> [a] -> Bool
in_list el [] = False
in_list el (x:xs) = el==x || el `in_list` xs

get_elem:: Int -> [a] -> a
get_elem i l = head (drop i l)

set_elem:: [a] -> Int -> a -> [a]
set_elem l i el = (take i l) ++ [el] ++ tail (drop i l)


--list
--elem to swap
--elem ind
--unique elems
swap_elem_if_not_in_list:: (Eq a) => [a] -> a -> Int -> [a] -> [(a, [a])]
swap_elem_if_not_in_list l el i elems
    | i >= length l        = []
    | el_i `in_list` elems = swap_elem_if_not_in_list l el (i+1) elems
    | otherwise = (el_i, (set_elem l i el)) : swap_elem_if_not_in_list l el (i+1) (el_i:elems)
    where
        el_i = head (drop i l)


swap_unique_heads:: (Eq a) => [a] -> [(a, [a])]
swap_unique_heads [] = []
swap_unique_heads l = (l_head, l_tail) : swap_elem_if_not_in_list l_tail l_head 0 [l_head]
    where
        l_head = head l
        l_tail = tail l

attach_head:: a -> [[a]] -> [[a]]
attach_head h xss = [h:xs | xs <- xss]

permutations':: (Eq a) => [(a,[a])] -> [[a]]
permutations' [] = []
permutations' ((h,[x]):xss) = [h,x] : permutations' xss
permutations' ((h,l):xss) = (attach_head h (permutations l)) ++ permutations' xss

permutations:: (Eq a) => [a] -> [[a]]
permutations [] = []
permutations [x] = [[x]]
permutations l = permutations' (swap_unique_heads l)