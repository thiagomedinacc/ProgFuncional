module Trab2 where

import Store

data Expr = L Integer | V String | Sub Expr Expr | Mult Expr Expr |
            Add Expr Expr | Div Expr Expr deriving (Show)
            
data ExprLogica = B Bool | And ExprLogica ExprLogica | Or ExprLogica ExprLogica | 
            Equal Expr Expr | MaiorQue Expr Expr | MenorQue Expr Expr | Diferente Expr Expr deriving (Show)
            
            
data Comando = Atrib String Expr | Sequencia Comando Comando | Escolha ExprLogica Comando Comando | 
                Neutro | While ExprLogica Comando | DoWhile Comando ExprLogica deriving (Show)
                

evalExpr:: Expr -> Store -> Integer
evalExpr (L nro) _ = nro
evalExpr (V var) sto = value sto var
evalExpr (Sub expr1 expr2) sto = evalExpr expr1 sto - evalExpr expr2 sto
evalExpr (Mult expr1 expr2) sto = evalExpr expr1 sto * evalExpr expr2 sto
evalExpr (Add expr1 expr2) sto = evalExpr expr1 sto + evalExpr expr2 sto
evalExpr (Div expr1 expr2) sto = evalExpr expr1 sto `div` evalExpr expr2 sto

evalExprLogica:: ExprLogica -> Store -> Bool
evalExprLogica (B b) _ = b
evalExprLogica (And expL1 expL2) sto = evalExprLogica expL1 sto && evalExprLogica expL2 sto              
evalExprLogica (Or expL1 expL2) sto = evalExprLogica expL1 sto || evalExprLogica expL2 sto                
evalExprLogica (Equal exp1 exp2) sto = evalExpr exp1 sto == evalExpr exp2 sto
evalExprLogica (MaiorQue exp1 exp2) sto = evalExpr exp1 sto > evalExpr exp2 sto
evalExprLogica (MenorQue exp1 exp2) sto = evalExpr exp1 sto < evalExpr exp2 sto
evalExprLogica (Diferente exp1 exp2) sto = evalExpr exp1 sto /= evalExpr exp2 sto

evalComando:: Comando -> Store -> Store
evalComando (Atrib var expr) sto = update sto var (evalExpr expr sto)
evalComando (Sequencia priCom segCom) sto = evalComando segCom (evalComando priCom sto)
evalComando (Escolha expLogica com1 com2) sto 
    | evalExprLogica (expLogica) sto == True = evalComando com1 sto
    | otherwise = evalComando com2 sto
evalComando Neutro sto = sto
evalComando (While expLogica com) sto = loopWhile expLogica com sto
evalComando (DoWhile com expLogica) sto = loopWhile expLogica com (evalComando com sto)

loopWhile:: ExprLogica -> Comando -> Store -> Store
loopWhile expLogica com sto
	|evalExprLogica expLogica sto == True = loopWhile expLogica com (evalComando com sto)
	| otherwise = sto
            
            
 
