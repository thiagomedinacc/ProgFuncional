module NotaFiscal where 

type Name = String
type Price = Int
type BarCode = Int


type Database = [(BarCode,Name,Int)]

bd:: Database
bd = [ (01, "Alface" ,200),
        (02, "Banana" ,400),
		(03, "Caramelo",600),
		(04, "Doritos",800),
		(05, "Esmalte",1000),
		(06, "Fogao",1200)
        ]
        
type TillType = [BarCode]
type BillType = [(Name,Price)]


lineLength:: Int
lineLength = 30

makeBill:: TillType -> BillType
makeBill tt = procuraP bd tt
                where
                    procuraP bd tt = [(b,c)| (a,b,c) <- bd, (d) <- tt, d==a] 
                    
                    
                    
formatPence:: Price -> String
formatPence num = show (num `div` 100) ++ "," ++ (verifica num)
                            where
                                verifica num 
                                 | num `mod` 100 < 10 = "0" ++ show (num `mod` 100)
                                 | otherwise = show (num `mod` 100)
                                 
                                 
formatLine:: (Name,Price) -> String
formatLine (n,p) =  show (n ++ inserePontos lineLength ++ formatPence p ) ++ "\n"
                            where 
                                inserePontos lineLength
                                    | length (formatPence p) + (length n) < lineLength = "." ++ inserePontos (lineLength-1)
                                    | otherwise = "."
                              

formatLines:: [(Name,Price)] -> String
formatLines (x:[]) = formatLine x
formatLines (x:xs) = formatLine x ++ formatLines xs



makeTotal:: [(Name,Price)] -> Price
makeTotal [] = 0
makeTotal lst = calculaPreco lista
                    where   
                        lista = [p | (n,p) <-lst]
                        calculaPreco [] = 0
                        calculaPreco (x:xs) = x + calculaPreco xs
                        
                        
formatTotal:: Price -> String
formatTotal n = formatLine ("Total",n)


formatBill::BillType -> String
formatBill lst = formatLines lst ++ formatTotal (makeTotal lst)


look::Database -> BarCode -> (Name,Price)
look bd bc = evazio (procuraBC bd bc)
                where
                    procuraBC bd bc = [(n,p) | (a,n,p) <- bd, a==bc]
                    --evazio [] = ("Unknown Item", 0)
                    evazio x
                        | x == [] = ("Unknown Item" , 0)
                        | otherwise = head (x)
                        

mlookup:: BarCode -> (Name,Price)
mlookup bc = look bd bc 


                           
                              
                              
                              
                              
                              
                              
                              
                              
                              