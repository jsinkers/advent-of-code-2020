import Data.List (sort, group)

pt1 :: IO [(Int, Int)]
pt1 = do input <- lines <$> readFile "day-10.txt"
         let adapters = map read input :: [Int]
         let adapters' = sort $ 0:(maximum adapters + 3):adapters
         let distrib = sort $ zipWith (-) (tail adapters') adapters'
         let vals = map (\xs -> (length xs, head xs)) $ group distrib
         return vals

pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-10.txt"
         let adapters = map read input :: [Int]
         let  adapters' = sort $ 0:adapters
         let distrib = zipWith (-) (tail adapters') adapters'
         let adapters'' = zip adapters' (0:distrib)
         let lists = splitLists adapters''
         print lists
         return $ product $ map adapterSeq lists

splitLists :: [(Int, Int)] -> [[Int]]
splitLists xs = splitLists' xs []

splitLists' :: [(Int, Int)] -> [Int] -> [[Int]]
splitLists' [] ys = [ys]
splitLists' ((v, d):xs) ys
    | d < 3 = splitLists' xs (ys ++ [v])
    | otherwise = ys:splitLists' xs [v]

adapterSeq :: [Int] -> Int
adapterSeq [_] = 1
adapterSeq (i:xs) = adapterSeq' i l (reverse vs)
    where (l:vs) = reverse xs

adapterSeq' :: Int -> Int -> [Int] -> Int
adapterSeq' inp target [] = if (target - inp) <= 3 then 1 else 0
adapterSeq' inp target (a:as) 
    -- continue without a and continue with a
    -- target - inp <= 3 = 1 + adapterSeq' a target as
    | a - inp <= 3 = adapterSeq' a target as + adapterSeq' inp target as
    | otherwise = 0

main :: IO ()
main = do num <- pt2
          print num
