module ExerciciosDatabase where

{- Exercícios 5.18-5.26, Haskell Craft -}

{- Estudar seção 5.7 (Haskell Craft) e depois fazer
   Exercícios 5.28-5.32 -}
   
   
type Person = String
type Book = String

data Loan = Loan Person Book

type Database = [(Person, Book)]

bd:: Database
bd = [("Marcos", "l1"), ("Pedro", "l2"), ("Marcos" , "l3")]


books:: Database -> Person -> [Book] --dada uma pessoa, retorna os livros que essa pessoa locou
books bd pessoa = [l |(p,l) <- bd, p == pessoa]

borrowers:: Database -> Book -> [Person] -- dado um livro, retorna as pessoas que o locaram
borrowers bd livro = [pessoas | (pessoas,livros) <- bd, livros == livro]

borrowed:: Database -> Book -> Bool -- dado um livro, retorna se ele foi locado
borrowed bd livro = length (borrowers bd livro) > 0

numBorrowed:: Database -> Person -> Int -- dada uma pessoa, retorna o numero de livros que essa pessoa locou
numBorrowed bd pessoa = length (books bd pessoa)

makeLoan :: Database -> Person -> Book -> Database
makeLoan bd pessoa livro = bd ++ [(pessoa, livro)]

returnLoan::Database -> Person -> Book -> Database
returnLoan bd pessoa livro = [(p,l) | (p,l) <- bd, (pessoa,livro) /= (p,l)]






