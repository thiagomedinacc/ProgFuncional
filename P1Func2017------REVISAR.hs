module P1Func2017 where

import Data.Char

merge:: Ord a => [a] -> [a] -> [a]
merge lst1 [] = lst1
merge [] lst2 = lst2
merge [x] [y] 
    |  x < y = [x] ++ [y]
    | otherwise = [y] ++ [x]
merge (x:xs) (y:ys)
    | x < y = x: merge xs (y:ys)
    | otherwise = y: merge (x:xs) ys
    
    
--data Prop = Const Bool
--    | Var Char
--    | Not Prop
--    | And Prop Prop
--    | Imp Prop Prop
--    deriving Show
    
    
--vars:: Prop -> [Char]
--vars prop =

pairs:: [a] -> [(a,a)]
pairs [x] = []
pairs (x:y:[]) = [(x,y)] 
pairs (x:y:xs) =[(x,y)] ++ pairs (y:xs) 