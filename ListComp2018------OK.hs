module ListComp where

import Data.Char (chr,ord)
{-
 Seção: Construção de Listas por Compreensão (Intenção)
 
  Uma compreensão de listas tem a forma:
  [e | Q], onde 
    e: uma expressão
    Q: qualificador, i.e., uma sequência de geradores e guardas 
       onde
    geradores: expressões x <- xs, onde x é uma variável ou tupla
    de variáveis e xs uma expressão cujos valores são listas.
    guardas: expressões booleanas
    
  
-}


 

lc01 = [x*x | x<-[1..5],odd x]
{-
x     = 1   2   3   4  5 
odd x = T   F   T   F  T
x*x  =  1   -   9   -  25
-}
bool = [True,False]
lco02 = [(x,y) | x<-bool,y<-bool]
{-
Avaliação:

[(True,y) | y <- bool] ++ [(False,y)| y <- bool]
=[(True,True)] ++ [(True,False)] ++ [(False,True)] ++ [(False,False)]
= ...
-}

exlist = [2,4,7]
lc02 = [2*n | n <- exlist]
lc03 = [even n | n <- exlist]
lc04 = [2*n| n <- exlist, even n, n>3]
pairList = [(2,3),(2,1),(7,8)]
lc05 = [m+n | (m,n) <- pairList]
addPairs xsPairs = [m+n | (m,n) <- xsPairs]
addOrdPairs xsPairs = [m+n | (m,n) <- xsPairs, m < n]

{- 
Define a function isDigit::Char -> Bool and then test the following definition:

digits str = [ ch | ch <- str, isDigit str]
-}

digits str = [ ch | ch <- str, isDigit ch]

isDigit::Char -> Bool 
isDigit a = a `elem` ['0'..'9']

allEven  xs = (xs == [x | x<- xs, even x])

{- Exercícios 5.18-5.26, Haskell Craft -}

doubleAll:: [Int] -> [Int]
doubleAll x = [a*2 | a <- x]

divisors:: Integer -> [Integer]
divisors x = [a | a <- [1..x], x `mod` a == 0]

isPrime:: Integer -> Bool
isPrime x = divisors x == [1,x]

matches:: Integer -> [Integer] -> [Integer]
matches x lst = [a | a <- lst, a == x]

melem:: Integer -> [Integer] -> Bool
melem x lst = length (matches x lst) > 0

onSeparateLines:: [String] -> String
onSeparateLines [] = ""
onSeparateLines (x:xs) = x ++ "\n" ++ onSeparateLines xs


duplicate:: String -> Integer -> String
duplicate str 0 = ""
duplicate str 1 = str
duplicate str x = str ++ (duplicate str (x-1))

{- exercício 5.19 -}

capitalize:: String -> String 
capitalize str = [chr(ord(ch) - (offset ch)) | ch <- str]
   where 
   	   -- deslocamento 
   	   offset::Char -> Int
   	   offset x = if islower x
   	      then 32 else 0
   	   --OU: islower x = ord(x) `elem` [97..122]
   	   islower:: Char -> Bool 
   	   islower x = x `elem` ['a'..'z']

toUpper:: Char -> Char
toUpper c = chr(ord(c)-32)

isLower:: Char -> Bool
isLower c = ord(c) >= 97 && ord(c) <= 122 

capitalize':: String -> String
capitalize' xs = [change x | x <- xs]
		where
		change k = if isLower(k) then toUpper(k)
				   else k

capitalizeL:: String -> String
capitalizeL xs = [x | x <- mword, x `elem`['A'..'Z']]
         where
         mword = [change x | x <- xs]
         change k = if isLower(k) then toUpper(k)
				   else k







{- Estudar seção 5.7 (Haskell Craft) e depois fazer
   Exercícios 5.28 e 5.32 -}

type Person = String
type Book = String

type Database = [(Person,Book)]    

borrowed:: Database -> Book -> Bool
borrowed dbase findbook = not(null foundBook)
	where
		foundBook = [book | (person,book) <- dbase, book==findbook]

-- Section: Basic Logic

infixr 1  ==> -- proridade:1-->9, infix rightassociative

(==>)::Bool->Bool->Bool
True ==> b = b
False ==> b = True

infixr 1 <==>
(<==>)::Bool->Bool->Bool
True <==> b = b
False <==> b = not b
form01::Bool -> Bool -> Bool
form01 p q =  (not p) && (p==>q) <==> not(q && not p)

form02:: Bool -> Bool -> Bool
form02 p q = (p ==> q) <==> (not(p) || q)

{- Exercício: Com base no tipo da fórmula, explique a diferença
   essencial formula1 e formula2.
-}

valid1:: (Bool->Bool)->Bool
valid1 bf = (bf True) && (bf False)

excluded_middle:: Bool -> Bool
excluded_middle p =  p || not p

valid01 = valid1 excluded_middle 

{-
  -- Exercícios sobre Lógica Proposicional
  
  Exercício: Utilizando o construtor de listas por compreensão 
  definir funções valid2 e valid3 que verificam se expressões
  booleanas de 2 e 3 variáveis são válidas.
  
  Exercício: Utilizando o construtor de listas por compreensão 
  definir funções valid2 e valid3 que verificam se expressões
  booleanas de 2 e 3 variáveis são contradições.
  
  Exercício: Utilizando o construtor de listas por compreensão 
  definir funções valid2 e valid3 que verificam se expressões
  booleanas de 2 e 3 variáveis são satisfatíveis.
  
  Exercício: Utilizando o construtor de listas por compreensão 
  definir definir uma função que computa a lista de modelos 
  de uma lista de fórmulas de duas variáveis. Um modelo de uma
  lista de fórmulas é uma valoração para as variáveis que satisfaz
  todas as fórmulas da lista.  
  
-}