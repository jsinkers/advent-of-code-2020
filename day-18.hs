import Data.Char (digitToInt, isDigit)
import Data.Maybe (isNothing)

parseExpression :: String -> Int
parseExpression inp = snd $ parseExpr (inp, 0, Nothing)

parseExpr :: (String, Int, Maybe Char) -> (String, Int)
parseExpr (' ':xs, v, op) = parseExpr (xs, v, op)
parseExpr ('(':xs, v, op) = parseExpr (xs', evalExpr op v v', Nothing)
    where (xs', v') = parseExpr (xs, 0, Nothing)
parseExpr (')':xs, v, _) = (xs, v)
parseExpr ('*':xs, v, Nothing) = parseExpr (xs, v, Just '*')
parseExpr ('+':xs, v, Nothing) = parseExpr (xs, v, Just '+')
parseExpr (l@(x:_), _, Nothing)
    | isDigit x = parseExpr (xs', v', Nothing)
    | otherwise = error "invalid expression"
    where (v', xs') = strToInt l
parseExpr (l@(x:_), v, op)
    | isDigit x = parseExpr (xs', evalExpr op v v', Nothing)
    | otherwise = error "invalid expression"
    where (v', xs') = strToInt l
parseExpr ([], v, Nothing) = ([], v)
parseExpr ([], _, Just _) = error "invalid expression"

evalExpr :: Maybe Char -> Int -> Int -> Int
evalExpr op v1 v2
  | op == Just '*' = v1*v2
  | op == Just '+' = v1+v2
  | isNothing op = v2
  | otherwise = error "unknown operation " 
  
strToInt :: String -> (Int, String)
strToInt xs = (foldl (\x y -> x*10 + y) 0 $ map digitToInt v, s)
    where (v, s) = span isDigit xs

pt1 :: IO Int
pt1 = do input <- lines <$> readFile "day-18.txt"
         let vals = map parseExpression input
         return $ sum vals

-- part 2 working
data Expr = Empty | Val Int | Plus Expr Expr | Mult Expr Expr
    deriving Show

parseExpression2 :: String -> Int
parseExpression2 inp = evalExpr2 . fst $ parseExpr2 (Empty, inp)

parseExpr2 :: (Expr, String) -> (Expr, String)
parseExpr2 (e, []) = (e, [])
parseExpr2 (e, ' ':xs) = parseExpr2 (e, xs)
parseExpr2 (Mult e1 (Val v), '+':xs) = (Mult e1 e2', xs')
    where (e2', xs') = parseExpr2 (Plus (Val v) Empty, xs)
parseExpr2 (Mult e1 (Mult e f), '+':xs) = (Mult e1 e2', xs')
    where (e2', xs') = parseExpr2 (Plus (Mult e f) Empty, xs)
parseExpr2 (e, '+':xs) = parseExpr2 (Plus e Empty, xs)
parseExpr2 (e, '*':xs) = parseExpr2 (Mult e Empty, xs)
parseExpr2 (Plus e1 Empty, l@(x:_))
  | isDigit x = parseExpr2 (Plus e1 (Val v'), xs')
  where (v', xs') = strToInt l
parseExpr2 (Mult e1 Empty, l@(x:_))
  | isDigit x = parseExpr2 (Mult e1 (Val v'), xs')
  where (v', xs') = strToInt l
parseExpr2 (Mult e1 Empty, l@(x:_))
  | isDigit x = parseExpr2 (Plus e1 (Val v'), xs')
  where (v', xs') = strToInt l
parseExpr2 (Empty, x:xs) 
  | isDigit x = parseExpr2 (Val v, xs')
  where (v, xs') = strToInt (x:xs)
parseExpr2 (Empty, '(':xs) = parseExpr2 (e2, xs')
    where (e2, xs') = parseExpr2 (Empty, xs)
parseExpr2 (Mult e1 Empty, '(':xs) = parseExpr2 (Mult e1 e2, xs')
  where (e2, xs') = parseExpr2 (Empty, xs)
parseExpr2 (Plus e1 Empty, '(':xs) = parseExpr2 (Plus e1 e2, xs')
  where (e2, xs') = parseExpr2 (Empty, xs)
parseExpr2 (e, ')':xs) = (e,xs)--(Val (evalExpr2 e), xs)
parseExpr2 (e, xs) = error ("invalid: " ++ show e ++ ", " ++ show xs)

evalExpr2 :: Expr -> Int
evalExpr2 Empty = 0
evalExpr2 (Val x) = x
evalExpr2 (Plus e1 e2) = evalExpr2 e1 + evalExpr2 e2
evalExpr2 (Mult e1 e2) = evalExpr2 e1 * evalExpr2 e2

n = Mult 
        (Mult 
            (Plus (Val 2) (Val 4)) 
            (Val 9)) 
         (Mult 
            (Plus 
                (Plus 
                    (Plus 
                        (Mult 
                            (Plus 
                                (Val 6) 
                                (Val 9)) 
                            (Plus 
                                (Val 8) 
                                (Val 6))) 
                        (Val 6)) 
                    (Val 2)) 
                (Val 4)) 
            (Val 2))

showExpr :: Expr -> Int -> String
showExpr (Val v) n = replicate n ' ' ++ show v ++ "\n"
showExpr Empty n = replicate n ' ' ++ show Empty ++ "\n"
showExpr (Plus e1 e2) n = replicate n ' ' ++ "+\n" ++ showExpr e1 (n+1) ++ showExpr e2 (n+1)
showExpr (Mult e1 e2) n = replicate n ' ' ++ "*\n" ++ showExpr e1 (n+1) ++ showExpr e2 (n+1)

pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-18.txt"
         let vals = map parseExpression2 input
         return $ sum vals
