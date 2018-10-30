module Folding where

{-foldr1:: (a -> a -> a) -> [a] -> a -- non-empty lists
foldr1 f [x0, x1, x2, x3] = x0 `f` (x1 `f` (x2 `f` x3))

foldr:: (a -> a -> a) -> a -> [a] -> a
foldr f e [x0, x1, x2, x3] = x0 `f` (x1 `f` (x2 `f` (x3 `f` e)))

mfand:: [Bool] -> Bool
mfand [] = True
mfand (x:xs) = x && (mfand xs)
-}

mfandex1 xs = foldr (&&) True xs  -- a = Bool
mfandex2 = foldr (&&) True (map even [0,2..100])

{-
mfmax [x] = x
mfmax (x:y:ys) = max x (mfmax (y:ys))
-}

mfandex3 = foldr1 max (reverse [-10..10])

{-
The more general version:
foldr1:: (a -> b -> b) -> [a] -> b -- non-empty lists
foldr1 f [x0, x1, x2, x3]  = x0 `f` (x1 `f` (x2 `f` x3))
-}

{-
foldr:: (a -> b -> b) -> b -> [a] -> b
-- Second argument is start value
foldr f e [x0, x1, x2, x3] = x0 `f` (x1 `f` (x2 `f` (x3 `f` e)))
-}

{-
  Exercício: Programar insertion sort com foldr/foldr1
  inSort [x1,x2,x3] = x1 insE (x2 insE (x3 insE []))
  insE:: a -> [a] -> [a]

  Exercício: Programar soma de listas de inteiros com foldr/foldr1

  Exercício: Programar reversão de listas com foldr/foldr1

  Exercício: Programar função last com foldr/foldr1

  Exercício: Programar função init com foldr/foldr1
-}

inSort:: Ord a => [a] -> [a]
inSort xs = foldr insE [] xs 

insE:: Ord a => a -> [a] -> [a]
insE x [] = [x]
insE x (y:ys) = if x <= y then x:y:ys else y:(insE x ys)

sumList:: [Integer] -> Integer
sumList xs = foldr (+) 0 xs


--revertList:: [a] -> [a]
--revertList xs = foldr (\x y -> y ++ [x]) [] xs
revertList:: [a] -> [a]
revertList xs = foldr radd [] xs
          where 
            radd:: a -> [a] -> [a]
            radd x xs = xs ++ [x]
            --radd = (\x xs -> x ++ xs)
-- let radd x xs = xs ++ [x] in foldr radd [] [1,2,3]

--last':: [a] -> a
