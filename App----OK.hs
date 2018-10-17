module App where

import ProcessamentoDeTextos

main = do      
    putStrLn "Nome do arquivo de entrada:"
    inFile <- getLine
    putStrLn "Nome do Arquivo de Saída"
    outFile <- getLine
    text <- readFile inFile
    writeFile outFile ("Numero de caracteres/palavras/linhas: "++ show(wc text) )           
    
main2 = do      
    putStrLn "Nome do arquivo de entrada:"
    inFile <- getLine
    putStrLn "Nome do Arquivo de Saída"
    outFile <- getLine
    text <- readFile inFile
    writeFile outFile (justificaTexto(fill text)) 
    
    
main4 = do      
    putStrLn "Nome do arquivo de entrada:"
    inFile <- getLine
    putStrLn "Nome do Arquivo de Saída"
    outFile <- getLine
    text <- readFile inFile
    writeFile outFile  (show(isPalindrome text))
