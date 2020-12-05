import Data.List (sort)

type Row = Int
type Column = Int
type BoardingPass = (Row, Column) 

pt1 :: IO Int
pt1 = do input <- lines <$> readFile "day-05.txt"
         return $ maximum $ map (seatID . parseBoardingPass) input

pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-05.txt"
         let seatIDs = sort $ map (seatID . parseBoardingPass) input
         return $ findMissingSeat seatIDs

parseBoardingPass :: String -> BoardingPass
parseBoardingPass = parseBoardingPass' (0,127,0,7)

parseBoardingPass' :: (Int, Int, Int, Int) -> String -> BoardingPass
parseBoardingPass' (rl, ru, cl, cu) [] = (min rl ru, max cl cu)
parseBoardingPass' (rl, ru, cl, cu) (x:xs)
    | x == 'F' = parseBoardingPass' (rl, (ru-rl) `div` 2 + rl, cl, cu) xs
    | x == 'B' = parseBoardingPass' (rl + (ru-rl) `div` 2 + 1,  ru, cl, cu) xs
    | x == 'R' = parseBoardingPass' (rl, ru, cl + (cu-cl) `div` 2 + 1,  cu) xs
    | x == 'L' = parseBoardingPass' (rl, ru, cl,  cl + (cu-cl) `div` 2) xs

seatID :: BoardingPass -> Int
seatID (r, c) = r*8 + c

findMissingSeat :: [Int] -> Int
findMissingSeat (x:y:xs)
  | y /= x + 1 = x + 1
  | otherwise = findMissingSeat (y:xs)
