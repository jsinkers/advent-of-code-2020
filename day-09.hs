import Data.List (sort)

pt1 :: IO (Maybe Int)
pt1 = do input <- lines <$> readFile "day-09.txt"
         let nums = map read input :: [Int]
         let(h, t) = splitAt 25 nums
         return $ invalidSum h t

summands :: [Int] -> Int -> Bool
summands xs = summands' (sort xs)

summands' :: [Int] -> Int -> Bool
summands' [] _ = False
summands' (x:xs) y 
  | (y-x) `elem` xs = True 
  | otherwise = summands' xs y

invalidSum :: [Int] -> [Int] -> Maybe Int
invalidSum _ [] = Nothing
invalidSum l1@(_:xs) (y:ys) = if summands l1 y 
                                    then invalidSum (xs ++ [y]) ys 
                                    else Just y
    
invalidNum = 26796446

pt2 :: IO (Maybe [Int])
pt2 = do input <- lines <$> readFile "day-09.txt"
         let nums = map read input :: [Int]
         return $ contigNums invalidNum [] nums

-- find list of contiguous numbers that add to sum
contigNums :: Int -> [Int] -> [Int] -> Maybe [Int]
contigNums _ _ [] = Nothing
contigNums x [] (y:ys) = contigNums x [y] ys
contigNums x l1@(_:ss) l2@(y:ys)
  | sumL1 == x = Just l1
  | sumL1 > x = contigNums x ss l2
  | sumL1 < x = contigNums x (l1 ++ [y]) ys
      where sumL1 = sum l1

