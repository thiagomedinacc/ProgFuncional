module ProcessamentoDeTextos where

import Data.Char(toLower)
import Prelude hiding(Word)
--import Data.List.Split(splitOn)
import Data.List(elemIndex)
import Data.Sequence (update,fromList)
import Data.Foldable(toList)

whitespace:: String
whitespace = ['\n', '\t', ' ']

lineLen:: Int
lineLen = 50


getWord:: String -> String
getWord [] = []
getWord (x:xs)
        | x `elem` whitespace = []
        | otherwise = x : getWord xs

dropWord:: String -> String
dropWord [] = []
dropWord (x:xs)
        | x `elem` whitespace = (x:xs)
        | otherwise = dropWord xs

dropSpace:: String -> String
dropSpace [] = []
dropSpace (x:xs)
        | x `elem` whitespace = dropSpace xs
        | otherwise = (x:xs)

type Word = String
type Line = [Word]

splitWords:: String -> [Word]
splitWords st = split (dropSpace st)

split:: String -> [Word]
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))

getLine':: Int -> [Word] -> Line
getLine' len [] = []
getLine' len (w:ws)
        | length w <= len = w : restOfLine
        | otherwise = []
        where
          newLen  = len - (length w + 1)
          restOfLine = getLine' newLen ws

splitLines:: [Word] -> [Line]
splitLines [] = []
splitLines ws = getLine' lineLen ws : splitLines (dropLine lineLen ws)

fill:: String -> [Line]
fill = splitLines . splitWords

-- Exercício 7.27

dropLine:: Int -> [Word] -> Line
dropLine len [] = []
dropLine len (x:xs)
        | length x > len = (x:xs)
        | length x == len = (xs)
        | otherwise = dropLine lenAtualizado xs
        where
          lenAtualizado = len - ((length (x) + 1))

-- Exercício 7.28

joinLine:: Line -> String
joinLine [] = ""
joinLine (x:[]) = x
joinLine (x:xs) = x ++ " " ++ joinLine xs

-- Exercício 7.29

joinLines:: [Line] -> String
joinLines [] = ""
joinLines (x:[]) = joinLine x
joinLines (x:xs) = joinLine x ++ "\n" ++ joinLines xs

-- Exercício 7.30

getWord2:: String -> String
getWord2 [] = []
getWord2 (x:xs) = fst(splitAt (length (head(splitWords (x:xs)))) (x:xs))


dropWord2:: String -> String
dropWord2 [] = []
dropWord2 (x:xs) = snd(splitAt (length (head(splitWords (x:xs)))) (x:xs))


          


-- Exercício 7.31

justifica:: Line -> String
justifica (x:xs)
    | length (joinLine (x:xs)) < lineLen = aux (x:xs) (lineLen - ((length (joinLine (x:xs)))))
    | otherwise = joinLine (x:xs)
      where
        aux (x:[]) _ = x
        aux (x:xs) nro
          | nro < 1 = x ++ " " ++ aux xs 0
          | otherwise = x ++ " " ++ colocaEspacos (nro) ++ aux xs (nro-1)
        colocaEspacos nro
          | nro >= (length (x:xs)) = " " ++ colocaEspacos (nro - 2)
          | otherwise = " "

-- Exercício 7.32

wc:: String -> (Int, Int, Int)
wc "" = (0,0,0)
wc str = (a,b,c)
        where
            a = (length str) - b
            b = length (splitWords str)
            c = length (procuraNovaLinha str)
            procuraNovaLinha str = [x | x <- str, x == '\n']

-- Exercício 7.33

isPalindrome:: String -> Bool
isPalindrome str
      | lowAndFilter str == lowAndFilter (reverse str) = True
      | otherwise = False

lowAndFilter = filter (`elem` ['a'..'z']) . map toLower

--Exercício 7.34

subst:: String -> String -> String -> String
subst oldSub newSub str 
     | oldSubExiste oldSub (splitWords str) = troca newSub (splitWords str) (elemIndex oldSub (splitWords str))
     | otherwise = str
                where 
                   troca newSub lstStr pos = 
                       case pos of 
                            Just n ->  joinLine(toList(update n newSub (fromList lstStr)))

  
oldSubExiste:: String -> [Word] -> Bool
oldSubExiste str lst = str `elem` lst 

justificaTexto::[Line] -> String
justificaTexto (x:[]) = joinLine x
justificaTexto (x:xs) = (justifica x) ++ "\n" ++ (justificaTexto xs)



