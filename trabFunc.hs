module TrabFunc where 


-- The `whitespace' characters.

whitespace :: String
whitespace = ['\n','\t',' ']

-- Get a word from the front of a string.

getWord :: String -> String
getWord []    = [] 
getWord (x:xs) 
  | elem x whitespace   = []
  | otherwise           = x : getWord xs

-- In a similar way, the first word of a string can be dropped.

dropWord :: String -> String
dropWord []    = []
dropWord (x:xs) 
  | elem x whitespace   = (x:xs)
  | otherwise           = dropWord xs

-- To remove the whitespace character(s) from the front of a string.

dropSpace :: String -> String
dropSpace []    = []
dropSpace (x:xs) 
  | elem x whitespace   = dropSpace xs
  | otherwise           = (x:xs)

-- A word is a string.

type Mword = String

-- Splitting a string into words.

splitWords :: String -> [Mword]
splitWords st = split (dropSpace st)

split :: String -> [Mword]
split [] = []
split st
  = (getWord st) : split (dropSpace (dropWord st))

-- Splitting into lines of length at most lineLen

lineLen :: Int
lineLen = 80

-- A line is a list of words.

type Line = [Mword]

-- Getting a line from a list of words.

mGetLine :: Int -> [Mword] -> Line
mGetLine len []     = []
mGetLine len (w:ws)
  | length w <= len     = w : restOfLine  
  | otherwise           = []
    where
    newlen      = len - (length w + 1)
    restOfLine  = mGetLine newlen ws



-- Splitting into lines.

splitLines :: [Mword] -> [Line]
splitLines [] = []
splitLines ws
  = mGetLine lineLen ws
         : splitLines (dropLine lineLen ws)

-- To fill a text string into lines, we write

fill :: String -> [Line]
fill = splitLines . splitWords


dropLine:: Int -> [Mword] -> Line
dropLine len [] = []
dropLine len (x:xs) 
    | length x > len = (x:xs)
    | length x == len = (xs)
    | otherwise = dropLine lenAtualizado xs
        where
            lenAtualizado = len - (length (x) + 1)
            
            
            
joinLine:: Line -> String
joinLine [] = ""
joinLine (x:[]) = x
joinLine (x:xs) = x ++ ", " ++ joinLine xs

joinLines:: [Line] -> String
joinLines [] = ""
joinLines (x:[]) = joinLine x
joinLines (x:xs) = joinLine x ++ "\n" ++ joinLines xs


