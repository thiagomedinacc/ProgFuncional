module BetterStore 
   ( BetterStore, 
     initial,     -- Store
     update,       -- Store -> a -> Int -> Store
     value       -- Store -> a-> Int
    ) where

data BetterStore a = Sto [(a,Int)]

initial::BetterStore a
initial = Sto []

update:: BetterStore a -> a -> Int -> BetterStore a
update (Sto sto) v n = Sto ((v,n):sto)

value::Eq a => BetterStore a -> a -> Int
value (Sto []) v = 0
value (Sto ((v,n):sto)) w
	|v==w = n
	|otherwise = value (Sto sto) w
