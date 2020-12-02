type MinChar = Int
type MaxChar = Int

data Password = Password MinChar MaxChar Char String

pt1 :: IO Int
pt1 = do input <- lines <$> readFile "day-2.txt" 
         let passwords = map parsePassword input
         return $ sum $ map (\x -> if validPassword x then 1 else 0) passwords

pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-2.txt" 
         let passwords = map parsePassword input
         return $ sum $ map (\x -> if validPasswordPt2 x then 1 else 0) passwords

parsePassword :: String -> Maybe Password
parsePassword input = do let [counts, [letter, ':'], password] = words input
                         [minCount,maxCount] <- getCounts counts
                         return $ Password minCount maxCount letter password

getCounts :: String -> Maybe [Int]
getCounts counts = case length w of
                     2 -> Just (map read w :: [Int])
                     _ -> Nothing
    where w = wordsWhen (=='-') counts

splitString :: Char -> String -> [String]
splitString c ss = splitString' c ss [[]]

splitString' :: Char -> String -> [String] -> [String]
splitString' _ [] acc = reverse acc
splitString' c (s:ss) (a:acc)
  | c == s = splitString' c ss ([]:a:acc)
  | otherwise = splitString' c ss ((a ++ [s]):acc)


validPassword :: Maybe Password -> Bool
validPassword Nothing = False
validPassword (Just (Password minCount maxCount c pw)) = minCount <= count && count <= maxCount
    where count = length $ filter (==c) pw 

validPasswordPt2 :: Maybe Password -> Bool 
validPasswordPt2 Nothing = False
validPasswordPt2 (Just (Password ind1 ind2 c pw)) = (pw !! (ind1-1) == c) `xor` (pw !! (ind2-1) == c)

-- from stack overflow
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x
