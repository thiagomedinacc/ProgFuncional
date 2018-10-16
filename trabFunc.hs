module TrabFunc where 

import Data.Char (ord,chr,toLower)
import Data.List (elemIndex)
import Data.Sequence (update,fromList)
import Data.Foldable (toList)
--import Data.Text(justifyLeft)
import qualified Data.Text as T


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

justify :: String -> String
justify = joinLines . fill


--Exercicio 7.27

dropLine:: Int -> [Mword] -> Line
dropLine len [] = []
dropLine len (x:xs) 
    | length x > len = (x:xs)
    | length x == len = (xs)
    | otherwise = dropLine lenAtualizado xs
        where
            lenAtualizado = len - (length (x) + 1)
            

--Exercicio 7.28            
            
joinLine:: Line -> String
joinLine [] = ""
joinLine (x:[]) = x
joinLine (x:xs) = x ++ " " ++ joinLine xs

--Exercicio 7.29

joinLines:: [Line] -> String
joinLines [] = ""
joinLines (x:[]) = joinLine x
joinLines (x:xs) = joinLine x ++ "\n" ++ joinLines xs

--Exercicio 7.30


-- Get a word from the front of a string.

--getWord2 :: String -> String
--getWord2 []    = [] 
--getWord2 str =  ((head (splitAt 0 str)))


{-
-- In a similar way, the first word of a string can be dropped.

dropWord :: String -> String
dropWord []    = []
dropWord (x:xs) 
  | elem x whitespace   = (x:xs)
  | otherwise           = dropWord xs
  

dropLine:: Int -> [Mword] -> Line
dropLine len [] = []
dropLine len (x:xs) 
    | length x > len = (x:xs)
    | length x == len = (xs)
    | otherwise = dropLine lenAtualizado xs
        where
            lenAtualizado = len - (length (x) + 1)
            
            
-}

-- Exercicio 7.31                
lineLen :: Int
lineLen = 30
                
 
joinLine3:: Line -> String
joinLine3 [] = ""
joinLine3 linha = just linha
        where
            just (x:[]) =  x 
            just (x:xs) = x ++  " "  ++ colocaEspacos nroEspacos ++ colocaEspacos (correcao (x:xs)) ++  colocaEspacos(correcao2(x:xs)) ++ just xs
            colocaEspacos 0 = ""
            colocaEspacos 1 = " "
            colocaEspacos n = " " ++ colocaEspacos (n-1)
            nroEspacos 
                | verificaDisponibilidade >0 = verificaDisponibilidade
                | otherwise =  0                                                                                                   
            verificaDisponibilidade = ((lineLen) - (length(joinLine linha))) `div` ((length linha) - 1)
            correcao (x:xs) = if  verificaDisponibilidade `mod` 2 == 1 && (length (x:xs) `mod ` 2 == 1) then 1 else 0
            correcao2 (x:xs) =  if  verificaDisponibilidade `mod` 2 == 1 && (length (x:xs) `mod ` 2 == 0) then 1 else 0
            
--j:: Line -> String
--j (x:xs)
--   | length (joinLine (x:xs)) < 15 = j 
            
            
            

            


-- Exercicio 7.32

--wc:: String -> (Int,Int,Int)
--wc "" = (0,0,0)
--wc x = processamento x (a,b,c)
--        where
--            a=0
--            b=0
--            c=0
--            processamento x (a,b,c)
--                | length x == 0 = (a,b+1,c+1)
--                |  ord (head x)  > 32 && ord (head x) < 127 =  processamento   (tail x) (a+1,b,c)
--                |  ord (head x) == 32 = processamento (tail x) (a,b+1,c)
--                | head x == '\n' = processamento (tail x) (a,b+1,c+1)
                
--wcFormat::String -> (Int,Int,Int)
--wcFormat x = wc (joinLines(fill x))


wc2:: String -> (Int,Int,Int)
wc2 "" = (0,0,0)
wc2 str = (a,b,c)
        where
            a = (length str) - (b - 1)
            b = length (splitWords str)
            c =  length (procuraNovaLinha str) + 1
            procuraNovaLinha str = [x | x <- str, x == '\n']
            
main = do      
    putStrLn "Nome do arquivo de entrada:"
    inFile <- getLine
    putStrLn "Nome do Arquivo de Saída"
    outFile <- getLine
    text <- readFile inFile
    writeFile outFile ("Numero de caracteres/palavras/linhas: "++show( wc2 text) )           
    
main2 = do      
    putStrLn "Nome do arquivo de entrada:"
    inFile <- getLine
    putStrLn "Nome do Arquivo de Saída"
    outFile <- getLine
    text <- readFile inFile
    writeFile outFile  (linhasParaTexto(fill text)) 
    
main3 = do      
    putStrLn "Nome do arquivo de entrada:"
    inFile <- getLine
    putStrLn "Nome do Arquivo de Saída"
    outFile <- getLine
    text <- readFile inFile
    writeFile outFile  (show(T.justifyRight lineLen ' ' (T.pack text)))
    
main4 = do      
    putStrLn "Nome do arquivo de entrada:"
    inFile <- getLine
    putStrLn "Nome do Arquivo de Saída"
    outFile <- getLine
    text <- readFile inFile
    writeFile outFile  (show(isPalin2 text))   
    
            
            
--Exercicio 7.33

isPalin:: String -> Bool
isPalin "" = True
isPalin [_] = True
isPalin str = processa str
            where
                processa str = verifica(ignoraNaoLetras(stringParaLowCase str))
                verifica y = (head y == last y) && isPalin (init ( tail y))
                    
              
isPalin2:: String -> Bool
isPalin2 "" = True
isPalin2 [_] = True
isPalin2 str = processa str == reverse (processa str)
            where
                processa str = ignoraNaoLetras(stringParaLowCase str)
                
   
ignoraNaoLetras:: String -> String
ignoraNaoLetras str =  filter (`elem` ['a'..'z']) str
                            
    
stringParaLowCase:: String -> String   
stringParaLowCase str = map toLower str   

--Exercicio 7.34
subst:: String -> String -> String -> String
subst oldSub newSub str 
     | oldSubExiste oldSub (splitWords str) = troca newSub (splitWords str) (elemIndex oldSub (splitWords str))
     | otherwise = str
                where 
                   troca newSub lstStr pos = 
                       case pos of 
                            Just n ->  joinLine(toList(update n newSub (fromList lstStr)))

                   

  
oldSubExiste:: String -> [Mword] -> Bool
oldSubExiste str lst = str `elem` lst 

linhasParaTexto::[Line] -> String
linhasParaTexto (x:[]) = joinLine x
linhasParaTexto (x:xs) = (joinLine3 x) ++ "\n" ++ (linhasParaTexto xs)

joinAndJustifyLine :: Line -> Int -> String
joinAndJustifyLine line maximumLineLength
 |  length line == 1  = line !! 1 -- Only one word.
 |  otherwise         = justify lineAsString
 
 where
 
 lineAsString :: String
 lineAsString =  joinLine line


    
justif :: String -> String
justif "" = ""
justif string
  |  length string < lineLen  = justif (insertBlanks string (lineLen - length string))
  |  otherwise = string
  
insertBlanks :: String -> Int -> String
insertBlanks [] _ = []
insertBlanks ( character : remainingCharacters ) number
  |  number < 1        =        character : insertBlanks remainingCharacters  number
  |  character == ' '  =  ' ' : character : insertBlanks remainingCharacters (number - 1)
  |  otherwise         =        character : insertBlanks remainingCharacters  number



    
    
    