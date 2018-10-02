module Sorting where




ins:: Ord a => a -> [a] -> [a]
ins x [] = [x]
ins x (y:ys) = if x <= y then x:y:ys
			else y:(ins x ys)


iSort::Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

qSort:: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort [y| y<-xs, y<=x] ++ [x] ++ qSort [y| y <- xs, y>x]




-- FAZER mergesort



--FAZER selection sort



--FAZER bubble sort


