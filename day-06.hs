import Data.List (nub)

type PersonAnswers = [Char]
type GroupAnswers = [PersonAnswers]

pt1 :: IO Int
pt1 = do input <- lines <$> readFile "day-06.txt"
         let answers = readAnswers input
         return $ length $ concatMap (nub . concat) answers

pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-06.txt"
         let answers = readAnswers input
         return $ sum $ map countGroupYes answers

readAnswers :: [String] -> [GroupAnswers]
readAnswers xs = readAnswers' xs [] []

readAnswers' :: [String] -> GroupAnswers -> [GroupAnswers] -> [GroupAnswers]
readAnswers' [] g gs = g:gs
readAnswers' (x:xs) g gs
    | x == "" = readAnswers' xs [] (g:gs)
    | otherwise = readAnswers' xs (x:g) gs



countGroupYes :: GroupAnswers -> Int
countGroupYes answers = length $ filter (\y -> all (elem y) answers) ['a'..'z']



