module ExerciciosDatabase where

{- Exercícios 5.18-5.26, Haskell Craft -}

{- Estudar seção 5.7 (Haskell Craft) e depois fazer
   Exercícios 5.28-5.32 -}
   
   
type Person = String
type Book = String

data Loan = Loan Person Book

type Database = [(Person, Book)]
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   ------------------------------------------------------------------------------------------------------------------------

type Person = String
type Book = String

type DataBase = [(Person,Book)]

bancoDeDadosExemplo:: DataBase
bancoDeDadosExemplo = [("Thiago","Codigo da Vinci"), ("Maria", "Simbolo Perdido"), ("Joao", "Fortaleza Digital"), ("Thiago", "Ponto de Impacto"), ("Sergio","Codigo da Vinci")]

books:: DataBase -> Person -> [Book]
books dBase findPerson = [book | (person,book) <- dBase, person==findPerson]

borrowers:: DataBase -> Book -> [Person]  
borrowers dBase findBook = [person | (person,book) <- dBase, book==findBook]

borrowed:: DataBase -> Book -> Bool

borrowed dBase findBook = not(null livroExiste)
	where
		livroExiste = [book | (person,book) <- dBase, book==findBook]
		

--numBorrowed:: DataBase -> Person -> Int
numBorrowed dBase findPerson = length (numBorrowed' dBase findPerson)

numBorrowed' dBase findPerson = [person | (person,book) <- dBase, findPerson==person]
	


makeLoan:: DataBase -> Person -> Book -> DataBase
makeLoan dBase pers bk = [(pers,bk)] ++ dBase

--returnLoan:: DataBase -> Person -> Book -> DataBase







