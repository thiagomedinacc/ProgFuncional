module Interpretador where

import Store

data ExprArit = L Integer | V String | Sub ExprArit ExprArit | Mult ExprArit ExprArit |
            Add ExprArit ExprArit | Div ExprArit ExprArit deriving (Show)
            
data ExprLogica = B Bool | And ExprLogica ExprLogica | Or ExprLogica ExprLogica | 
            Equal ExprArit ExprArit | MaiorQue ExprArit ExprArit | MenorQue ExprArit ExprArit | Diferente ExprArit ExprArit deriving (Show)
            
            
data Comando = Atrib String ExprArit | Sequencia Comando Comando | Escolha ExprLogica Comando Comando | 
                Neutro | While ExprLogica Comando | DoWhile Comando ExprLogica deriving (Show)
                

evalExprArit:: ExprArit -> Store -> Integer
evalExprArit (L nro) _ = nro
evalExprArit (V var) sto = value sto var
evalExprArit (Sub expr1 expr2) sto = evalExprArit expr1 sto - evalExprArit expr2 sto
evalExprArit (Mult expr1 expr2) sto = evalExprArit expr1 sto * evalExprArit expr2 sto
evalExprArit (Add expr1 expr2) sto = evalExprArit expr1 sto + evalExprArit expr2 sto
evalExprArit (Div expr1 expr2) sto = evalExprArit expr1 sto `div` evalExprArit expr2 sto

evalExprLogica:: ExprLogica -> Store -> Bool
evalExprLogica (B b) _ = b
evalExprLogica (And expL1 expL2) sto = evalExprLogica expL1 sto && evalExprLogica expL2 sto              
evalExprLogica (Or expL1 expL2) sto = evalExprLogica expL1 sto || evalExprLogica expL2 sto                
evalExprLogica (Equal exp1 exp2) sto = evalExprArit exp1 sto == evalExprArit exp2 sto
evalExprLogica (MaiorQue exp1 exp2) sto = evalExprArit exp1 sto > evalExprArit exp2 sto
evalExprLogica (MenorQue exp1 exp2) sto = evalExprArit exp1 sto < evalExprArit exp2 sto
evalExprLogica (Diferente exp1 exp2) sto = evalExprArit exp1 sto /= evalExprArit exp2 sto

evalComando:: Comando -> Store -> Store
evalComando (Atrib var expr) sto = update sto var (evalExprArit expr sto)
evalComando (Sequencia priCom segCom) sto = evalComando segCom (evalComando priCom sto)
evalComando (Escolha expLogica com1 com2) sto 
    | evalExprLogica (expLogica) sto == True = evalComando com1 sto
    | otherwise = evalComando com2 sto
evalComando Neutro sto = sto
evalComando (While expLogica com) sto = loopWhile expLogica com sto
evalComando (DoWhile com expLogica) sto = loopWhile expLogica com (evalComando com sto)

loopWhile:: ExprLogica -> Comando -> Store -> Store
loopWhile expLogica com sto
    | evalExprLogica expLogica sto == True = loopWhile expLogica com (evalComando com sto)
    | otherwise = sto
            
            
 
