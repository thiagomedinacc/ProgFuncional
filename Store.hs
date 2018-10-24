-------------------------------------------------------------------------
--  
--     Store.hs
--  
--         An abstract data type of stores of integers, implemented as
--         a list of pairs of variables and values.         
--                                  
--         (c) Addison-Wesley, 1996-2011.                   
--  
-------------------------------------------------------------------------

module Store 
   ( Store, 
     initial,     -- Store
     update,       -- Store -> String-> Int -> Store
     value       -- Store -> String-> Int
    ) where

data Store = Sto [(String,Int)]

initial::Store
initial = Sto []

update:: Store -> String -> Int -> Store
update (Sto sto) v n = Sto ((v,n):sto)

value::Store -> String -> Int
value (Sto []) v = 0
value (Sto ((v,n):sto)) w
	|v==w = n
	|otherwise = value (Sto sto) w

