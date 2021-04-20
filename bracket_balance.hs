toCloseBracket :: Char -> Char
toCloseBracket '(' = ')'
toCloseBracket '{' = '}'
toCloseBracket '<' = '>'
toCloseBracket '[' = ']'

isOpenBracket :: Char -> Bool
isOpenBracket ch = ch == '(' || ch == '{' || ch == '<' || ch == '['

isCloseBracket :: Char -> Bool
isCloseBracket ch = ch == ')' || ch == '}' || ch == '>' || ch == ']'

maxDepth :: Int -> Int -> Int
maxDepth (-1) y = -1
maxDepth x (-1) = -1
maxDepth x y = max x y

--line
--stack of opening brackets
bracketBalance :: String -> [Char] -> Int
bracketBalance "" [] = 0
bracketBalance "" stack = -1
bracketBalance (ch:s) stack
    | isOpenBracket ch = bracketBalance s (toCloseBracket ch:stack)
    | isCloseBracket ch && empty_stack = -1
    | isCloseBracket ch && (ch /= head stack) = -1
    | isCloseBracket ch = maxDepth (length stack) (bracketBalance s (tail stack))
    | otherwise = bracketBalance s stack
    where
        empty_stack = (length stack) == 0

printAns :: String -> String
printAns s = show(bracketBalance s []) ++ "\n"

main = interact (printAns)