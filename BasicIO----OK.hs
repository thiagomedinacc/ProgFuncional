module BasicIO where

import Data.Char 
{-
  Seção: Conceitos Básicos de IO
  
  O tipo polimórfico (IO a) pode ser entendido como um tipo de ações de tipo a. Um objeto 
  
      prg::IO a 
  
  é um programa que faz alguma tipo de I/O e retorna um valor do tipo a.
  Haskell possui o tipo unit, escrito (), que possui um único valor, também escrito como 
   (), que é a tupla vazia. Algumas funções básicas para I/O:
   
   getLine::IO String
   getChar:: IO Char
   putChar:: Char -> IO ()
   putStr:: String -> IO ()
   putStrLn:: String -> IO ()
   putStrLn = putStr . (++ "\n")
   
   print:: Show a => a -> IO ()
   print = putStrLn . show 
-}

{- Notação do                         
  
  A "notação do" concatena várias ações em uma só. O tipo da ação composta
  resultante é o tipo da última ação.
-}

main02::IO() 
-- o tipo do programa corresponde ao tipo da última ação
main02 = do
   putStrLn "Olá, como é seu nome?"
   name <- getLine -- retira o str
   putStrLn("Hey " ++ name ++ ", você é demais!")
 
main10::IO()  
-- com a utilização de parênteses e ";" 
-- a identação não é importante
main10 = do {
   putStrLn "Olá, como é seu nome?";
   name <- getLine;
   putStrLn("Hey " ++ name ++ ", você é demais!");
}

{-
 O signficado da expressão "name <- getLine"?

  é realizar a ação de IO, chamada getline, retornar um string e amarrar este 
  valor ao identificador name. Com a notação do, a única forma de acessar o 
  dado retornado por uma ação de IO é através do construtor "<-".
-}

{-
   Exercício: Porque a expressão "Hello, meu nome é" ++ getLine não é bem
   formada?
-}

main = do
   a <- putStrLn "Olá, como é seu nome?"
   name <- getLine
   putStrLn("Hey " ++ name ++ ", você é demais!. Sobrenome, please?")
  
{-
 Exercício:
 
 O comando runhaskell compila um módulo e executa o código gerado a partir
 da função com nome "main".  
 A partir do terminal, digitar 'runhaskell BasicIO.hs".
 O que é executado?
-}

main03 = do
  putStrLn "Olá, como é seu nome?"
  firstName <- getLine
  putStrLn "Olá, como é seu sobrenome?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn ("Hey " ++ bigFirstName ++ " "
     ++ bigLastName ++ ", como você está?")

{-
   O operador '$' também é o operador de aplicação de funções,
   com tipo ($)::(a->b)->a->b.
-}

exStr1 = putStrLn ("Programação" ++ "Funcional" ++ "em Haskell") 
exStr2 = putStrLn $ "Programação" ++ "Funcional" ++ "em Haskell"

{-
  Exercício:
-}

main04::IO() 
main04=  do
   putStrLn "Paro de processar quando você digitar a palavra vazia" 
   line <- getLine
   if null line
   then return ()
   else do
     putStrLn (reverseWords line)
     main04

reverseWords::String -> String
reverseWords str = unwords (map reverse (words str))

{- 
   Exercício: Como base no código abaixo, o que você entende sobre
   o comportamento de return?
  
-}

mputStr::String -> IO()
mputStr [] = return ()
mputStr (x:xs) = 
	do 
	  putChar x
	  mputStr xs

mputStrLn::String -> IO()
mputStrLn [] = do {
        mputStr "\n";
        return ()
        }
mputStrLn (x:xs) = 
	do 
	  putChar x
	  mputStrLn xs

main05 = do
   a <- return "Hell"
   b <- return "Yeah"
   putStrLn $ a ++ " "++b ++ "!"
   
{- Recursive Functions and IO -}

fatIO::Int -> IO Int 
fatIO n = 
        if n == 0 
            then return 1
        else do 
               j <-  fatIO (n-1)
               return (n * j)

fatDataIO::IO() 
fatDataIO = do 
    putStr "Digite o valor desejado: "
    kStr <- getLine
    let kNum = (read kStr)::Int -- read::String -> a 
    fat <- fatIO kNum 
    putStrLn ("Fatorial de " ++ kStr ++ ": " ++ show fat)
    
{- Exercise - Definir funções recursivas que recebem argumentos do teclado.
    potIO::IO Int 
    multIO: IO Int 
    ....
-}

potIO:: Int -> Int -> IO Int
potIO 0 _ = return 0
potIO _ 0 = return 1
potIO a b =
        do 
            x <- potIO a (b-1)
            return (a * x)

potDataIO:: IO()
potDataIO = do
    putStr "Digite base "
    n1 <- getLine
    putStr "Digite expoente "
    n2 <- getLine
    res <- potIO (read n1) (read n2)
    putStrLn ("Resultado " ++ (show res))
    
    
{- Exercício: Com o auxilio da função putChar, escreve duas funções,
   que tenham o mesmo comportamento, respectivamente,
   das funções putStr e putStrLn.
   
-}



{-
  Para testar os próximos programas, utilize um arquivo texto qualquer e
  dê o nome "entrata.txt"  a ele.
-}

main06 = do  
   text <- readFile "entrada.txt"
   writeFile "saida.txt" text
   putStrLn "Arquivos processados"
   
main07 = do  
   text <- readFile "entrada.txt"
   writeFile "saida.txt" (map toUpper text)
   putStrLn "Arquivos processados"
   
{-
   Exercício:
     - Qual o tipo das funções readFile e writeFile?
     - Qual o comportamento destas funções?
-}



{- Exercise
commonWords :: Int -> Text -> String
-}