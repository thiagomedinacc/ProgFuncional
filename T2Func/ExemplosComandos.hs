module ExemplosComandos where

import Interpretador
import Store

-- Primeiro comando: Fatorial de um numero:
{-
res=1;
nro=4;
while(nro > 1) {
   res = res*nro;
   nro--;
}
-}

memoriaComValores =   update (update initial "res" 1) "nro" 5

verificaLoop = MaiorQue (V "nro") (L 1)
atribMult = Atrib "res" (Mult (V "res") (V "nro"))
decrementaNro = Atrib "nro" (Sub (V "nro") (L 1)) 

fatorial = While verificaLoop (Sequencia atribMult decrementaNro) --(evalComando atribResENro initial )
resFinal = value (evalComando fatorial memoriaComValores) "res" 


-- Segundo comando: Multiplicaçao com auxilio de somas
{-
nro1 = 4;
nro2 = 6;
res = 0;
while (nro2 > 0){
    res = res + nro1;
    nro2--;
}
-}

memoriaComValores2 = update (update (update initial "nro1" 4) "nro2" 6) "res" 0

verificaLoop2 = MaiorQue (V "nro2") (L 0)
atribSoma = Atrib "res" (Add (V "res") (V "nro1"))
decrementaNro2 = Atrib "nro2" (Sub (V "nro2") (L 1))

multComSomas = While verificaLoop2 (Sequencia atribSoma decrementaNro2 )
resFinal2 = value (evalComando multComSomas memoriaComValores2) "res"


-- Terceiro comando: Potencia com expoentes naturais:
{-
base = 3;
exp = 4
res = 1;
while (exp > 0){
    res = res * base;
    exp--;
}
  
-}

memoriaComValores3 = update (update (update initial "base" 3) "exp" 4) "res" 1

verificaLoop3 = MaiorQue (V "exp") (L 0)
atribMult2 = Atrib "res" (Mult (V "res") (V "base"))
decrementaExp = Atrib "exp" (Sub (V "exp") (L 1))

potComExpNat = While verificaLoop3 (Sequencia atribMult2 decrementaExp)
resFinal3 = value (evalComando potComExpNat memoriaComValores3) "res"


--Quarto comando: Divisão inteira:
{-
dividendo = 15;
divisor = 4;
res = dividendo / divisor; (/ é o `div` do haskell)
-}
memoriaComValores4 = update (update initial "dividendo" 15) "divisor" 4 

atribDivisao = Atrib "res" (Div (V "dividendo") (V "divisor"))
resFinal4 = value (evalComando atribDivisao memoriaComValores4) "res"


-- Quinto comando: Máximo divisor comum (MDC)

{-
n1 = 35
n2 = 25
while (n1 != n2){
    if (n1 > n2){
        n1 -= n2
    }
    else 
        n2 -=n1
}
res = n1;
-}

memoriaComValores5 = update (update initial "n1" 360) "n2" 80

verificaLoop5 = Diferente (V "n1") (V "n2")
verificaQualOMaior = MaiorQue (V "n1") (V "n2") -- Retorna True se "n1" maior que "n2", ou False, caso contrário.
decrementaN1 = Atrib "n1" (Sub (V "n1") (V "n2"))
decrementaN2 = Atrib "n2" (Sub (V "n2") (V "n1"))
comandoDeDecisao = Escolha verificaQualOMaior  decrementaN1 decrementaN2

mdc = While verificaLoop5 comandoDeDecisao

resFinal5 = value (evalComando mdc memoriaComValores5) "n1"










