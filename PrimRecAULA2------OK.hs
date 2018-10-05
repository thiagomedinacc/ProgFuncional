module PrimRec  where 

{- 
1. 
Uma função recursiva primitiva f::a->b é uma função definida sobre um tipo indutivo a.
Tipos indutivos serão vistos mais adiante. Aqui então introduziremos o conceito 
para inteiros e listas finitas através de exemplos. 
-}

{-1.1 Recursão Primitiva sobre Inteiros: para definir uma função recursiva primitiva
f::Integer -> b é suficiente :


 - Associar uma constante em b a f(0).
 - Definir o valor de um inteiro n>0 em função de f(n-1)
 - Definir o valor de um inteiro n<0 em função de f(n+1), caso a função esteja
   definida para valores negativos.
-}

fat::Integer -> Integer 
-- requer n>=0 
fat n 
    | n==0 = 1  -- fat01
    | n>0 = n*fat(n-1) -- fat02 
    | otherwise = error "Fatorial não definido para valores negativos."
    
{-
Exemplo de computação: Note que a avaliação da função auxiliar é 
feita de forma atômica.

fat 3 = 3 * fat 2 (fat02)
      = 3*2* fat 1 (fat02)
      = 3*2*1*fat 0 (fat02)
      = 3*2*1*1 (fat01)
      = 6 (definição (*))
-}

{- primeira versão recursiva da soma. Somente para o caso em que 
   o primeiro agumento é positivo -}
   
add::Integer -> (Integer -> Integer)
-- requer m>=0
add m n 
  | m==0 = n  -- add01
  | m>0 = 1+ add (m-1) n -- add02
  | m < 0 = error "Primeiro argumento deve ser positivo"
  
  
{- Exemplo de computação

add 3 (-6) = 1 + add 2 (-6) (add02)
           = 1 + 1 + add 1 (-6) (add02)
           = 1 + 1 + 1 + add 0 (-6) (add02)
           = 1 + 1 + 1 + (-6) (add01)
           = -3 (definição (+))
 -}

-- Multiplicaçao sobre inteiros

mult2:: Integer -> Integer -> Integer
mult2 a b 
    | a  == 0 || b == 0 = 0 
    | a > 0 = a + mult2 a (b-1)
    |otherwise = -500
	
multint:: Integer -> (Integer -> Integer)
--requer m>=0
multint m n
    | m == 0 = 0 -- multint01
    | m>1 = n + multint (m-1) n -- multint03
    | m<0 = error "primeiro argumento deve ser positivo"

{- Exemplo de computação
--corrigir

multint 3 4 = 4 + multint 2 4  (multint03)
            = 4 + 4 + multint 1 4  (multint03)
            = 4 + 4 + 4 (multint02)
            = 12 (definição +)

-}

-- função de potenciação

mpot:: Integer -> (Integer -> Integer)
-- requer n >= 0
mpot b e
  | e == 0 = 1
  | e > 0 = b * mpot b (e-1)
  | e < 0 = error "Expoente negativo" 

-- Exemplo de avaliação
{-
mpot 2 3 = 2 * mpot 2 2
         = 2 * 2 * mpot 1 2
         = 2 * 2 * 2 * mpot 0 2
         = 2 * 2 * 2 * 1
         = 8

-}

-- Função 1 + 2 + 3 + ... + k RECURSIVA

msoma:: Integer -> Integer
-- requer a>=0

msoma a
  | a == 0 = 1 -- msoma01
  | a > 0 = 2 * a + 1 + msoma(a-1) -- msoma02
  | a < 0 = error "Erro"

-- Exemplo de avaliação
{-
msoma 3 = 2 * 3 + 1 + msoma(2) -- (msoma02)
        = 2 * 3 + 1 + 2 * 2 + 1 + msoma(1)  --(msoma02)
        = 2 * 3 + 1 + 2 * 2 + 1 + 2 * 1 + 1 + msoma(0)  --(msoma02)
        = 2 * 3 + 1 + 2 * 2 + 1 + 2 * 1 + 1 + 1  --(msoma01)
        = 7 + 5 + 3 + 1  --(definição (+))
        = 16 -- (Definição (+))
-}

-- Função 1^2 + 2^2 + ... + k^2 RECURSIVA

msoma2:: Integer -> Integer
--requer a >= 0
msoma2 a
  | a == 0 = 1  --(msoma2A)
  | a > 0 = a * a * msoma2(a-1) -- (msoma2B)
  | a < 0 = error "Erro"

-- Exemplo de avaliação
{-
  msoma2 3 = 3 * 3 * msoma2(2)  (msoma2B)
           = 3 * 3 + 2 * 2 * msoma2(1)   (msoma2B)
           = 3 * 3 + 2 * 2 + 1 * 1 * msoma2(0)  (msomaB)
           = 3 * 3 + 2 * 2 + 1 * 1 * 1  (msomaA)
           = 9 + 4 + 1 (Definição (*))
           = 14 (Definição (+))
-} 




res:: Integer -> Integer -> Integer
res a b
	| a == 0 = b
	| b == 0 = error "nao existe div por zero"
    | a == b = 0
	| b > a = a
	| a > b = res (a-b) b 

-- função resto da divisao de inteiros

mmodulo:: Integer -> (Integer -> Integer)
-- requer n >= 0
mmodulo a b
  | a == 0 = 0
  | b == 0 = error "Não existe divisão por 0"
  | b > a = a
  | a > 0 = mmodulo (a-b) b
  | a < 0 = error "dividendo negativo" 

-- Exemplo de avaliação
{-
mdiv 5 2 = mdiv 3 2
         = mdiv 1 2
         = 1
          

-}
 
{-
Exercícios: Para cada um dos exercícios abaixo, definir a função primitiva recursiva 
pedida e apresentar um exemplo de avaliação no qual ocorra pelo menos três chamadas 
recursivas. Justicar cada igualdade (redução) da avaliação.

 - Função de multiplicação sobre inteiros (um dos argumentos deve ser positivo). OK
 - Função de potenciação, onde a potência é um inteiro positivo. OK
 - 1 + 3 + 5 + ... + k (duas versões: recursiva e através da forma fechada) OK
 - 1^2 + 2^2 + 3^2 + ... + k^2 (duas versões: recursiva e através da forma fechada) OK
 These ones are more difficult
  - Fundção de divisão inteira (inteiros positivos).
 - Função de resto da divisão inteira (inteiros positivos)
 - Função de soma para quaisquer inteiros.
 - Função de multiplicação para quaisquer inteiros.
 - Função de divisão inteira para quaisquer inteiros.

-}

{-
1.2 Recursão primitiva sobre listas.

Para definir uma função primitiva recursiva total f :[a]->b é suficiente:

- associar uma constante em b a f [] 
- definir f(x:xs) em função de f(xs)
-}
 
mlength::[a]->Integer 
mlength [] = 0 -- mlen01
mlength (x:xs) = 1 + mlength xs -- mlen02

{-
  Exemplo de computação:
  
  mlength [3,7,2] = mlength (3:[7,2]) (sintaxe)
                  = 1 + mlength [7,2] (mlen02)
                  = 1 + mlength (7:[2]) (sintaxe)
                  = 1 + 1 + mlength [2] (mlen02)
                  = 1 + 1 + mlength (2:[]) (sintaxe)
                  = 1 + 1 + 1 + mlength [] (mlen02)
                  = 1 + 1 + 1 + 0 (mlen01)
                  = 3 (definição (+))
                  
-}

mlast::[a]->a 
-- requer lista não vazia 
mlast (x:[])=x  -- mlast01
mlast (x:y:ys) = mlast(y:ys) -- mlast02

{-
mlast [3,2,1] = mlast (3:2:[1]) (sintaxe)
              = mlast (2:[1])  (mlast02)
              = mlast (2:1:[])  (sintaxe)
              = mlast (1:[]) (mlast02)
              = 1  (mlast01)
-}

{-
Exercícios: Para cada uma das funções abaixo, as quais estão definidas no prelúdio, 
definir sua própria versão primitiva recursiva e apresentar um exemplo de avaliação no qual ocorra 
pelo menos três chamadas  recursivas. Justicar cada igualdade (redução) da avaliação.

- concatenação de listas
- prefixo da lista (take)
- sufixo da lista (drop)
- parte inicial da lista (init)
- indexação de listas (!!)
- reversão de listas
-}

mcon:: [a] -> [a] -> [a]
mcon [] ys = ys
mcon xs [] = xs
mcon (x:xs) ys = x: mcon xs ys


mtake:: [a] -> [a]
mtake [] = []
mtake (x:[]) = []
mtake (x:y:ys) = x: mtake (y:ys)

mdrop:: [a] -> [a]
mdrop [] = []
mdrop (x:[]) = []
mdrop (x:y:ys) = (y:ys)

minicial:: [a] -> Int -> [a]
minicial [] a = []
minicial (x:xs) 0 = [] 
minicial (x:xs) a 
    | length (x:xs) < a = error "index mt grande"
    | otherwise = x: minicial xs (a-1)
    
    
mindex:: [a] -> Int -> a
mindex [] a = error "lista vazia"
mindex (x:xs) 0 = x 
mindex (x:xs) a 
    | length (x:xs) <= (a) = error "index mt grande"
    | otherwise = mindex (xs) (a-1)
    
    
mreverse:: [a] -> [a]
mreverse [] = []
mreverse (x:[]) = [x] 
mreverse (x:ys) = (mreverse ys) ++ [x]  




















