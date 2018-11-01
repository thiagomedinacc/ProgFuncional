module FoldingNatural where

import IndTypes2018

{-
f:: MNat -> a
f Z = c
f (Suc x) = h(f x)

por exemplo, f mapeia Suc(Suc(Suc Z)) para h (h (h (c)))


-}

foldn::(a->a) -> a -> MNat -> a
foldn h c Z = c
foldn h c (Suc n) = h (foldn h c n)

{-
assumimos que MNat esta instanciado em Show, Eq, Ord e Num.
utilizando foldn, definir as funções:
-n2i, i2n, soma, multiplicação, potenciação
-avaliar manualmente um exemplo de cada definição

-}

n2i = foldn (+1) 0 

i2n = foldn (+1) Z

soma = (\m n -> foldn (+1) m n) -- soma MNat com integer  
soma' = (\m n -> foldn (Suc) m n) -- Soma 2 MNats

{-
soma' (Suc Z) (Suc Z) =   
-}

mul m n = foldn  (+n) Z m

pot m n = foldn (mul m) (Suc Z) n


{-
-definir *length e *map utlizandando FOLD
-definir:
    b2i: recebe string de numeros binarios e converte para um natural/INTEIRO?
    dci: recebe string de digitos e converte para um numero correspondente
-}

