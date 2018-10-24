module ExpressoesBetterStore where

import BetterStore

data Expr = V String| L Int | Expr  :-: Expr | 
            Expr :*: Expr |
            Expr :+: Expr deriving (Show,Eq,Ord)



teste = evExpr (V "x" :+: V "y" ) store

store::BetterStore String
store = update (update initial "x" 1) "y" 2

evExpr::Expr -> BetterStore  String-> Int
evExpr(L n) store = n
evExpr(V var) store = value store var
evExpr (n :+: m) store = evExpr n store + evExpr m store
evExpr (n :-: m) store = evExpr n store - evExpr m store
evExpr (n :*: m) store = evExpr n store * evExpr m store
