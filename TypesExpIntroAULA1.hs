module TypesExpIntro where

import Data.Char (ord,chr)
 
{- Seção: Haskell Script
 Um programa  Haskell consiste em uma sequência de definições.
 Uma definição associa um nome a um valor de um tipo específico
 
 name::type
 name = exp
-}

{-  Seção Tipos Primitivos: Bool,Integer,Int, Float, Char, String
-}

{-
  Tipo Bool é formado somente pelas constantes True e False else
  equipado com as funções booleanas tradicionais:
  
  &&:: Bool -> (Bool -> Bool) (conjunção)
  ||:: Bool -> (Bool -> Bool) (disjunção)
  not:: Bool -> Bool (negação)
  relações sobre ordens: ==, <,<=,>,>=,/=
-}

bool1 = True && False 
bool2 = (&&) True True 
bool3 = not True || False 
bool4 = 4 < 3 
bool5 = 2 /= 2
bool6 = True && False 

size::Int  
-- inteiros limitados em complemento de 2
size = 12

size'::Integer
-- inteiros ilimitados
size'= 2^20

char::Char
char = 'a'

asc_a = ord char
char_a = chr asc_a

message::String 
message = "Hello World!"

 

{-
  Seção: Definição de Funções 
  - f::x -> y "f recebe argumentos do tipo x e retorna resultados 
   do tipo y"
  - x  é chamado tipo domínio e y o tipo Codomínio.
  - (->) é um construtor de tipo para funções. 
  -  a -> b representa o tipo de todas as funções de a para b, onde
     a e b são tipos.
  - Funções são em geral funções parciais, pois podem não estar definidas para
  - todos os valores. Funções que estão definidas para todos os valores dos
  - tipo domínio são chamadas de funções totais.
-}

sqr:: Int -> Int
sqr x = x*x

times4::Int -> Int 
times4 x = double(double x)

-- versão anônima 
afour:: Int->Int
afour x =    double(double x)

double:: Int -> Int 
double n = 2*n

-- aqui os dois primeiros tipos representam
-- o tipo dos argumentos. 
add:: Int -> Int -> Int 
add m n = m + n -- os argumentos m e n são separados por espaços

silly_sum::(Int,Int)-> Int 
silly_sum(x, y) = x+y

msign::Int -> Int

-- Funões definidas por casos
-- com o condicional
msign x = if x>0 then 1
           else if x==0 then 0
                else -1
                
msign'::Int -> Int 

-- Funções definidas por casos
-- com equações guardadas (por expressões booleanas)
msign' x | x > 0  = 1
msign' x | x == 0 = 0 
msign' x | x < 0 = -1

-- Forma abreviada da solução acima.
msign'' x | x > 0  = 1
          | x == 0 = 0 
          | x < 0 = -1
{-
  Funções sobre Booleans
-}

-- com o condicional
mand:: Bool -> Bool -> Bool
mand b c = if b then c else False 

-- por indução em Bool
mand':: Bool -> Bool -> Bool
mand' True b = b 
mand' False b = False

-- com equações guardadas 
mand'':: Bool -> Bool -> Bool
mand'' b c | b == True = c
           | otherwise = False
           
{-
  Exercício: Definir suas próprias versões dos outros operadores 
  booleanos utilizando as três técnicas apresentadas acima: mor, mnot,
  mimp,meqv, mnor, mand, mxor.
-}

--tabela verdade OU

--A|B|S
--V|V|V
--V|F|V
--F|V|V
--F|F|F

--Com condicional
mor::Bool -> Bool -> Bool
mor a b = if a then True else b

-- por indução em Bool
mor':: Bool -> Bool -> Bool
mor' True b = True
mor' False b = b

-- com equações guardadas 
mor'':: Bool -> Bool -> Bool
mor'' a b  | a == True = True
           | otherwise = b
		   


--tabela verdade MNOT

--A|S
--V|F
--F|V

--Com condicional
mnot::Bool -> Bool 
mnot a = if a then False else True

--por indução em Bool
mnot':: Bool -> Bool
mnot' True = False
mnot' False = True

-- com equações guardadas 
mnot'':: Bool -> Bool
mnot'' a  | a == True = False
         | otherwise = True

--tabela verdade Imp

--A|B|S
--V|V|V
--V|F|F
--F|V|V
--F|F|V

--Com condicional
mimp::Bool -> Bool -> Bool
mimp a b = if b then True else not a

--por indução em Bool
mimp':: Bool -> Bool -> Bool
mimp' a True = True
mimp' a False = not a

-- com equações guardadas 
mimp'':: Bool -> Bool -> Bool
mimp'' a b | a == True = b
           | otherwise = True
		   
		 --tabela verdade eqv

--A|B|S
--V|V|V
--V|F|F
--F|V|F 
--F|F|V

--Com condicional
meqv::Bool -> Bool -> Bool
meqv a b = if a then b else mnot b 

--por indução em Bool
meqv':: Bool -> Bool -> Bool
meqv' a True = a
meqv' a False = not a

-- com equações guardadas 
meqv'':: Bool -> Bool -> Bool
meqv'' a b | a == True = b
           | otherwise = not b
		   
		   --tabela verdade nor

--A|B|S
--V|V|F
--V|F|F
--F|V|F 
--F|F|V

--Com condicional
mnor::Bool -> Bool -> Bool
mnor a b = if a then False else not b 

--por indução em Bool
mnor':: Bool -> Bool -> Bool
mnor' a True = False 
mnor' a False = not a

-- com equações guardadas 
mnor'':: Bool -> Bool -> Bool
mnor'' a b | a == True = False
           | otherwise = not b
		   
		   --tabela verdade xor

--A|B|S
--V|V|F
--V|F|V
--F|V|V 
--F|F|F

--Com condicional
mxor::Bool -> Bool -> Bool
mxor a b = if a then not b else b 

--por indução em Bool
mxor':: Bool -> Bool -> Bool
mxor' a True = not a 
mxor' a False =  a

-- com equações guardadas 
mxor'':: Bool -> Bool -> Bool
mxor'' a b | a == True = not b
           | otherwise =  b


{-
   Seção: Abstrações com let e where 
-}

sum_sqr::(Float,Float) -> Float
-- requer True
-- garante sum_sqr = x^2 + y^2
sum_sqr(x,y) = let t1 = x*x 
                   t2 = y*y 
               in t1+t2 
          
sum_sqr'::(Float,Float)-> Float 
sum_sqr'(x,y)= t1 + t2 where
    t1 = x*x 
    t2 = y*y 
    
{-
   Definir uma função que recebe os coeficientes de um polinômio
   de segundo grau e retorna a tupla de valores que resolvem
   a expressão, i.e., o valores que zeram o polinômio.
   
   Sugestão: solveQuadrEq::(Float,Float,Float)->(Float,Float)
-}
 
solveQuadrEq::(Float,Float,Float) -> (Float,Float)
solveQuadrEq(a,b,c) = ((baskara1 a b c), baskara2 a b c )
	where
		baskara1 a b c = (-b + delta a b c) / (2*a)
		baskara2 a b c = (-b - delta a b c ) / (2*a)
		delta a b c = sqrt((b*b) - (4*a*c))
		





{-
 Abreviações de Tipo: Utilizadas para melhorar a legibilidade dos programas. 
 Não acrescentam nada a portabilidade dos programas.
-}

 
{-
   Seção: Funções Parciais
-}

bottom:: a
bottom = error "Função não-definida. Observar pré-condição"

msqrt::Float -> Float 
-- requer x >= 0 
-- garante msqrt = raiz quadrada de x 
msqrt x | x>=0 = sqrt x 
        | otherwise = bottom 

-- undefined é pré-definido no Prelúdio.
msqrt'::Float -> Float 
-- requer x >= 0 
-- garante msqrt = raiz quadrada de x 
msqrt' x | x>=0 = sqrt x 
         | otherwise = undefined  -- undefined is pré-definida em Haskell      
