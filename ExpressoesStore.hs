module ExpressoesStore where

import Store

data Expr = V String| L Int |  Expr  :-: Expr | 
            Expr :*: Expr |
            Expr :+: Expr deriving (Show)

teste = evExpr (V "x" :+: V "y") store

store::Store String
store = update (update initial "x" 1) "y" 2

evExpr::Expr -> Store String -> Int
evExpr(L n) store = n 
evExpr(V var) store = value store var
evExpr (n :+: m) store = evExpr n store + evExpr m store
evExpr (n :-: m) store = evExpr n store - evExpr m store
evExpr (n :*: m) store = evExpr n store * evExpr m store


