module FunStore 
	( Store,
		initial,
		update,
		value
	) where
	
	
--newtype Store = Store (Var -> Integer)
data Store a = Sto (a -> Int) -- construtor

initial :: Store a
initial = Store (\x -> 0) 

update:: (Eq a) => Store a -> a -> Int -> Store a
update (Sto sto) v n = Sto (\x -> if x == v then n else sto x)
--update (Sto memoria) "z" 10
--retorna memoria'.
-- na memoria' o value "z" = 10
-- se pesquisar na memoria' o valor de 'y', ele retorna memoria 


value:: (Eq a) => Store a -> a -> Int


