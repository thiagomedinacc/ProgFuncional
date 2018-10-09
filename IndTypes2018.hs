module IndTypes2018 where

{-
  Tipos indutivos (Recursivos, Algébricos) são definidos com a utilização de construtores de dados.
  Construtores de dados são operadores cujo tipo de retorno é o tipo
  que está sendo definido. Construtores de dados com aridade 0 são
  chamados de construtores base. Do contrário, são chamados de construtores
  indutivos. Os construtores são funções injetores.
  
  Tipos Indutivos são também chamados de tipos recursivos, tipos 
  algébricos.  
-}


data Person = Mary | John | Newton | Anne

{-
  no tipo Person acima, todos os construtores são nulários e retornam um valor
  do tipo Person.
  
  Mary :: Person, John:: Person, Newton::Person, Anne:: Person
-}


instance Show Person where
    show p = case p of 
       Mary -> "Mary"
       Newton  -> "Newton"
       John -> "John"
       Anne -> "Anne"
    
{-
  Exercício: Instanciar Person nas classes Eq, Ord e Enum.
-}
 

data MNat = Z | Suc MNat 

{-
  No tipo Nat acima, temos os construtores de dados
       Z::MNat,  Suc::MNat -> MNat 
       Valores do tipo incluem
         Z, Suc Z, Suc (Suc Z), etc..
-}

instance Show MNat where 
   show Z = "Z"
   show (Suc x) = "Suc(" ++ show x ++")"

{- instance Eq MNat where {- ugly -}
    Z == Z = True
    Z == (Suc x) = False 
    (Suc x) == Z = False 
    (Suc x) == (Suc y) = (x==y) 
-}

 

instance Eq MNat where {- better -}
    Z == x = case x of
    	        Z -> True
    	        (Suc k) -> False 
    (Suc x) == y = case y of 
   	                 Z -> False 
   	                 (Suc k) -> (x == k)
    x /= y = not (x==y)

{- Exercício: Instanciar MNat na classe Ord -}
    
{-  Exercícios:
    - Corrija a instanciação de MNat na classe show acima. Por exemplo,
      o termo "Suc (Suc Z)" está sendo impresso como "Suc(Suc(Z))".
    - Instancie Nat nas seguintes typclasses:
Eq OK
Ord 
Num (+,-, *, negate, abs, signum, fromIntegr)
Enum.
-}

pred1::MNat -> MNat
pred1 Z = Z
pred1 (Suc x) = x

    

instance Num MNat where
    -- (+)
    x + Z = x
    x + Suc y = Suc (x+y)

    -- (-)
    x - Z = x
    x - (Suc y) = pred1(x - y)


instance Ord MNat where
    -- implementacao de (<=) :: MNat -> MNat -> Bool
    Z <= Z = True
    Z <= (Suc x) = True
    (Suc x) <= Z = False
    (Suc x) <= (Suc y) = x <= y

    -- implementacao de (<) :: MNat -> MNat -> Bool
    x < y = (x <= y) && (x /= y)


    -- implementacao de (>=) :: MNat -> MNat -> Bool

    Z >= Z = True
    Z >= (Suc x) = False
    (Suc x) >= Z = True
    (Suc x) >= (Suc y) = x >= y

     -- implementacao de (>) :: MNat -> MNat -> Bool
    x > y = (x >= y) && (x /= y)


    -- implementacao de (max) :: MNat -> MNat -> MNat

    Z `max` Z = Z
    (Suc Z) `max` Z = (Suc Z) 
    (Suc x) `max` (Suc y) = x `max` y









{-
  Definir funções recursivas natToInt::MNat -> Int e intToNat::Int -> MNat 
-}

natToInt::MNat -> Integer
natToInt Z = 0
natToInt (Suc x) = 1 + natToInt Z


intToNat:: Integer -> MNat
intToNat x
        | x <= 0 = Z
        | x > 0 = Suc (intToNat (x-1))
        	


{-
   Exercícios: Instanciar MNat nas classes Show, Eq, Ord, Enum.
-}

{- 
Listas também poder definidas com tipos recursivos.
-}

data List a = Nil | Cons a (List a)

l01 = Cons 1 (Cons 2 (Cons 3 Nil))
l02 = Cons "Hello" (Cons "World" (Cons "!" Nil))







{-
 Definir funções recursivas de conversão de (List a) para [a] e vice_versa.
-}
mlTohl:: List a -> [a]
mlTohl Nil = []
mlTohl (Cons x xs) = x:(mlTohl xs)


hlToml:: [a] -> List a
hlToml [] = Nil
hlToml (x:xs) = Cons x (hlToml xs)

instance Show a => Show (List a) where
    show Nil = "Nil"
    show (Cons x Nil) = "Cons " ++ show x ++ " Nil"
    show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"

{-
   Exercícios: Instanciar List nas classes Show, Eq, Ord, Enum.
-}
{-
    
-}



instance Eq a => Eq (List a) where
    --implementacao (==)
    Nil == Nil = True
    (Cons x xs) == (Cons y ys) =  (x==y) && (xs==ys)  
    _==_ = False       

    --implementacao (/=)
    Nil /= Nil = False
    (Cons x xs) /= (Cons y ys) =  (x/=y) || (xs/=ys)  
    _/=_ = True                         


--instance Ord a => Ord (List a) where
    -- implementacao (<)
--    Nil < Nil = False
--    Cons (x xs) < Nil = False
--    Nil < Cons (x xs) = True
                            


{-
  Exercícios: Definir as funções usuais de lista sobre List:
  comprimento, concatenação, reversão, map, filter, take, drop,
  etc.
  
  - Exercícios: definir função de conversão entre [a] e List a.
-}

{-
 - Defina um novo tipo para listas, RList a, onde o construtor 
  indutivo insere um novo elemento à direita da lista.
 - Instanciar RList nas classes de tipo Show, Eq, Ord.
 - Definir funções de conversão entre [a] e RList
-}


data Expr = L Int | Sub Expr Expr | 
            Mult Expr Expr |
            Add Expr Expr 
            
{-
  Os construtores definidos acima representam as seguintes operações:
  
  L::Int -> Expr, Sub::Expr Expr -> Expr, Add::Expr Expr -> Expr 

-}

--  (3+4)*5 - 16
expex = Sub (Mult (Add (L 3) (L 4)) (L 5)) (L 16)

{-
 Instanciar Expr na classe Show
-}

instance Show Expr where
	show (L k) = show k
	show (Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
	show (Sub e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
	show (Mult e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

{- 
  Definir uma função recursiva eval::Exp -> Int 
-}

eval:: Expr -> Int
eval (L k) = k
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) =  (eval e1) - (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)



{-
Estender  o tipo recursivo VExpr de forma que seja possível incluir variáveis nas expressões. 
-}

data VExpr = V String| C Int | VSub VExpr VExpr | 
            VMult VExpr VExpr |
            VAdd VExpr VExpr 


vexpex = VMult (V "x") (VMult (VAdd (V "x") (V "y")) (C 15 )) 

instance Show VExpr where
	show (C k) = show k
 	show (V k) = k
	show (VAdd e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
	show (VSub e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
	show (VMult e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"


type Store = String -> Int

stoExemplo::Store
stoExemplo "x" = 10
stoExemplo "y" = -5
stoExemplo "z" = 2
stoExemplo _ = 0

vEval:: VExpr -> Store -> Int
vEval (C k) store = k
vEval (V k) store = store k
vEval (VSub e1 e2) store = (vEval e1 store) - (vEval e2 store)
vEval (VAdd e1 e2) store = (vEval e1 store) + (vEval e2 store)
vEval (VMult e1 e2) store = (vEval e1 store) * (vEval e2 store)



data NExp = K Int | NExp :+: NExp |
		NExp :-: NExp | NExp :*: NExp deriving (Show, Eq, Ord)

nEval:: NExp -> Int
nEval (K n) = n
nEval (e1 :+: e2) =  (nEval e1) + (nEval e2) 
nEval (e1 :-: e2) =  (nEval e1) - (nEval e2)
nEval (e1 :*: e2) =  (nEval e1) * (nEval e2)


--  (3+4)*5 - 16
newExpEx = (((K 3) :+: (K 4)) :*: (K 5)) :-: (K 16)


data VNExp = X Int | A String | VNExp :++: VNExp |
		VNExp :--: VNExp | VNExp :**: VNExp deriving (Show, Eq, Ord)

vnEval:: VNExp -> Store -> Int
vnEval (X k) store = k
vnEval (A k) store = store k
vnEval (e1 :--: e2) store = (vnEval e1 store) - (vnEval e2 store)
vnEval (e1 :++: e2) store = (vnEval e1 store) + (vnEval e2 store)
vnEval (e1 :**: e2) store = (vnEval e1 store) * (vnEval e2 store)

--  (3+4)*5 - 16
newExpEx2 = (((A "x") :++: (X 4)) :**: (X 5)) :--: (X 16)


{-
 Definir agora a nova função de avaliação sobre VExpr. Qual o novo parâmetro (tipo) que é necessário?
-}


{-
  Árvores Binárias (Recursivamente)
-}
data BArv a = Leaf  | Br a (BArv a) (BArv a)

data BTree a = Node a| Fork a (BTree a) (BTree a) | Empty

tree01= Br 1 (Br 2 Leaf Leaf) (Br 3 Leaf Leaf) 
tree02= Fork 1 (Node 2)  (Node 3)

{-

Exercícios
- Instanciar BArv, BTree nas classes Show, Eq, Ord.
- Escrever funções de conversão entre BArv e BTree.
- Escrever as funções de caminhamento usuais sobre os
 dois tipo de árvores binárias: inorder, preorder e postorder.
-}

{- Com notação de registro -}
data AddrDB = Addr {
                           fame:: String,
                           lame::String,
                           address:: String,
                           gender:: Char,
                           birthdate::(Int,Int,Int),
                           city::String
                         }
                         
addrDB01 = Addr {
                   fame = "Isac",
                   lame = "Newton",
                   address = "Trinity College",
                   gender = 'M',
                   birthdate = (4,1,1643),
                   city= "Cambrige"}

{-
A definição acima declara implicitamente as seguintes funções:

fame:: Addr -> String 
lame:: Addr  -> String 
address:: Addr -> String
gender:: Addr -> String 

-}
type Baddr = [AddrDB]                   
{-
  Exercício:
  Escrever funções para as operações usuais de consulta, remoção else
  atualização sobre o tipo Baddr.
  
  - Escrever uma função recursiva que recebe um string e verifique
    se o string é um cpf válido.
-}
newtonCity = city addrDB01


{-
Dicionários também podem ser implementados  através do construtor 
Map k v que implementa mapeamentos de chaves do tipo k para valores
do tipo v.

- Exercício: Reimplemente o banco de dados acima utilizando o construtor de 
tipos Map (ver em Learn yourself Haskell for a greater good).
Defina as operações usuais de consulta, remoção e atualização.
-}

 


{-
  Quando tipos algébricos possuem somente um construtor podemos
  usar newtype ao invés de data. Esta construção é muito útil 
  quando queremos definir um novo tipo a partir de um outro
  pré-existente.  Por exemplo, podemos utilizar os inteiros
  para implementar os naturais.
-}

data Nat = MkNat Integer 

instance Show Nat where
  show (MkNat n) = show n 
  
instance Eq Nat  where 
  (MkNat x) == (MkNat y) = x == y 
  
instance Ord Nat where 

   (MkNat x) <= (MkNat y) = x <= y 
   
{-
  Instanciar Nat na classe Num.
-}

{-
  Utilizando a instanciação acima, definir funções recursivas 
  usuais sobre Nat. 
  
-}


 

