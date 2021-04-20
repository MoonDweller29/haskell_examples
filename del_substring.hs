import System.Environment

delSubStr:: String -> String -> String
delSubStr "" subStr = ""
delSubStr s subStr
    | (take subStr_len s) == subStr = delSubStr (drop subStr_len s) subStr
    | otherwise = (head s) : delSubStr (tail s) subStr
    where
        subStr_len = length subStr
    

delSubstring:: [String] -> String -> [String]
delSubstring [] s = []
delSubstring (line:xs) s
    | s == line || cleanLine == "" = delSubstring xs s
    | otherwise = cleanLine : delSubstring xs s
    where
        s_len     = length s
        cleanLine = delSubStr line s

main = do
    args <- getArgs
    text <- readFile (args !! 0)
    putStr (unlines ( delSubstring (lines text) (args !! 1)))

