module Queue
    ( Queue ,
    emptyQ , --Queue a
    isEmptyQ , -- Queue a -> Bool
    addQ , -- a -> Queue a -> Queue a
    remQ -- Queue a -> (a, Queue a)
    ) where


data Queue a = Queue [a]
    deriving (Show)

emptyQ:: Queue a
emptyQ = Queue []   

isEmptyQ:: Queue a -> Bool
isEmptyQ (Queue []) = True
isEmptyQ _ = False

addQ:: a -> Queue a -> Queue a
addQ x (Queue xs) = Queue (xs++[x])

remQ:: Queue a -> (a, Queue a)
remQ (Queue xs)
    | not (isEmptyQ (Queue xs)) = (head xs, Queue (tail xs))
    | otherwise = error "remQ"	



-- SEGUIR FAZENDO PAGINA 408
