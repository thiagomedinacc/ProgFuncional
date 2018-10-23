module P1Func where

matches:: Integer -> [Integer] -> [Integer]
matches nro lst = [a | a <- lst , a == nro]

--maxF:: Int -> (Int -> Int) -> Int
--maxF n f  = ret f n 
--    where 
--        ret func nro 
--

swmap:: (a->b) -> (a->b) -> [a] -> [b]
swmap f1 f2 [] = []
swmap f1 f2 (x:[]) = [f1 x]
swmap f1 f2 (x:y:[]) = [f1 x] ++ [f2 y]
swmap f1 f2 (x:y:xs) = f1 x : f2 y : swmap f1 f2 xs


prime:: Int -> [Int]
prime k = [x | x <- [2..k], divisors x == [1,x]]

divisors:: Int -> [Int]
divisors nro = [x | x <- [1..nro],  nro `mod` x  == 0 ]


quoR:: Int -> Int -> (Int,Int)
quoR 0 _ = (0,0)
quoR _ 0 = error "nao existe divisao por zero"
quoR a b = aux a b 0
    where 
        aux a b nro
            | a >= b = aux (a-b) b (nro+1)
            | otherwise = (nro,a)


