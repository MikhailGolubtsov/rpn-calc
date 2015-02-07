import Data.List
import Data.Map(Map,(!),member,fromList)
import Data.Char(isDigit)
import Control.Monad(foldM,(>>=),(<=<))

type RpnExpr = String
type Result = Double
type Stack = [Result]
type Token = String
type Error = String
type OperationF = Result -> Result -> Result
data Statement = Operation OperationF | Operand Result

rpn :: RpnExpr -> Either Error Result
rpn = getResult <=< executeStatements <=< toStatements  

ops :: Map Token OperationF
ops = fromList[("+", (\x1 x2 -> x1 + x2)),
		("-", (\x1 x2 -> x2 - x1)),
		("*", (\x1 x2 -> x1 * x2)),
		("/", (\x1 x2 -> x2 / x1))]

getResult :: Stack -> Either Error Result
getResult [r] = Right r
getResult s = Left $ "Bad final stack state " ++ show s

executeStatements :: [Statement] -> Either Error Stack
executeStatements = foldM (flip execute) initStack 
  where initStack = [] :: Stack

execute :: Statement -> Stack -> Either Error Stack
execute (Operation f) (x1:x2:xs) = Right $ (f x1 x2):xs
execute (Operation _) s = Left $ "Invalid stack state " ++ show s
execute (Operand v) s = Right (v:s)

toStatements :: RpnExpr -> Either Error [Statement]
toStatements rpn = sequence $ map toStatement tokens 
  where tokens = filter (not . null) $ splitBy ' ' rpn

toStatement :: Token -> Either Error Statement
toStatement t
  | t `member` ops = Right $ Operation (ops ! t)
  | all (`elem` '.':['0'..'9']) t = Right $ Operand (read t :: Result)
  | otherwise = Left $ "Could not parse '" ++ t ++ "'"

splitBy :: Char -> [Char] -> [[Char]]
splitBy delim s = foldr (\c acc@(x:xs) -> if (c == delim) then "":acc else (c:x):xs) [""] s
