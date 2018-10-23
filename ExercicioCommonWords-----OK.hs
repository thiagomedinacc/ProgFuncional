module ExercicioCommonWords where

import Sorting
import Data.Char

type Text = String

--words:: Text -> [String]
--words = do
--        text <- readFile "a.txt"

sortWords:: [String] -> [String]
sortWords = qSort


countRuns::[String] -> [(Int,String)]
countRuns [] = []
countRuns (x:xs) = 
    let prefix = takeWhile (x==) (x:xs)
        sufix = dropWhile (x==) (x:xs)
    in (length prefix,x): countRuns sufix



sortRuns::[(Int,String)] -> [(Int,String)]
sortRuns = reverse . qSort

showRun:: (Int, String) -> String
showRun (i,w) = w ++ " " ++ show i ++ "\n"



commonWords :: Int -> Text -> String
commonWords n =
    concat . map showRun . take n .
    sortRuns . countRuns . sortWords .
    words . map toLower 

commonWords2 :: Int -> Text -> String
commonWords2 n text = 
    concat ( map showRun ( take n
    (sortRuns ( countRuns ( sortWords
    (words ( map toLower text ))))))) 


main = do
    putStrLn "Nome do arquivo de entrada "
    infile <- getLine
    putStrLn "Quantidade de palavras "
    n <- getLine
    putStrLn "Nome do arquivo de saida "
    outfile <-getLine
    text <- readFile infile
    writeFile outfile (commonWords (read n) text)
    putStrLn "Feito!"










    
