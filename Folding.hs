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

rev:: [Integer] -> [Integer]
rev lst = foldr (\x y -> y ++ [x]) [] lst


--revertList:: [a] -> [a]
--revertList xs = foldr (\x y -> y ++ [x]) [] xs
revertList:: [a] -> [a]
revertList xs = foldr radd [] xs
          where 
            radd:: a -> [a] -> [a]
            radd x xs = xs ++ [x]
            --radd = (\x xs -> x ++ xs)
-- let radd x xs = xs ++ [x] in foldr radd [] [1,2,3]

mlast:: [a] -> a
mlast [x] = x
mlast (x:y:ys) = foldr1 (\x y -> y) (y:ys)

minit:: [x] -> [x]
minit xs =   mfoldr1' (:) xs

--4 versoes com recursao primitiva



mfoldrt:: (a->a->a) -> a -> [a] -> a
mfoldrt f e (x:xs) = f x (foldr f e xs)

mfoldrt1:: (a->a->a) -> [a] -> a
mfoldrt1 f [x] = x
mfoldrt1 f (x:xs) = f x (mfoldrt1 f xs)

mfoldlt:: (a->a->a) -> a-> [a] -> a
mfoldl f e [] = e
mfoldlt f e (x:xs) = mfoldlt f (f x e) xs

mfoldlt1:: (a->a->a) -> [a] -> a
mfoldlt1 f [x] = x
mfoldlt1 f (x:y:ys) =   f (f x y) (mfoldlt1 f ys)


--print foldr
psumr = foldr
            (\x y -> concat ["(",x,"+",y,")"])
            "0" (map show [1..5])


--print foldl
psuml = foldl (\x y -> concat ["(",x,"+",y,")" ])
               "0" (map show [1..5]) 






