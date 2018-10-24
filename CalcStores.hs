module CalcStores where

data VExpr = V String| C Int | VSub VExpr VExpr | 
            VMult VExpr VExpr |
            VAdd VExpr VExpr 
