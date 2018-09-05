module ExercicioNotaFiscal where

type Name = String
type Price = Int
type BarCode = Int

type DataBase = [(BarCode, Name, Price)]

bdExemplo:: DataBase
bdExemplo = [ (4719, "Fish Fingers" , 121),
		(5643, "Nappies" , 1010),
		(3814, "Orange Jelly", 56),
		(1111, "Hula Hoops", 21),
		(1112, "Hula Hoops (Giant)", 133),
		(1234, "Dry Sherry, lit", 540)]


type TillType = [BarCode]
type BillType = [(Name,Price)]

--makeBill :: TillType -> BillType -- which takes a list of bar codes to a list of name/price pairs

--formatBill :: BillType -> String -- which takes a list of name/price pairs into a formatted bill

--produceBill :: TillType -> String -- which will combine the effects of makeBill and f ormatBill

lineLength :: Int
lineLength = 30

formatPence :: Price -> String
formatPence price =  pounds ++ "." ++ fpence   
		where
		pounds =show (price `div` 100)
		pence =show (price `mod` 100)
		fpence
		    | length pence == 1 = "0" ++ pence
		    | otherwise = pence


formatLine :: (Name, Price) -> String	
formatLine (nome,preco) =  nome ++ fazPontos (tamanho) ++ formatPence preco ++ "\n"	
		where 
		tamanho = lineLength - (length (nome) + length (formatPence preco))
		fazPontos tamanho 
			 |tamanho > 0 =  "." ++ fazPontos (tamanho-1)
			 |otherwise = ""


formatLines :: [ (Name,Price) ] -> String
formatLines [] = ""
formatLines (x:xs) =  formatLine x ++ formatLines xs

-- putStrLn (formatLines [ (name,price) | (_,name,price) <- codeIndex])

makeTotal :: BillType -> Price
makeTotal bill = total priceList
	where
		priceList = [price | (_,price)<- bill]
		total:: [Int] -> Int
		total [] = 0
		total (x:xs) = x + total xs

formatTotal :: Price -> String
formatTotal preco = formatLine ("Total",preco)

--formatBill :: BillType -> String

