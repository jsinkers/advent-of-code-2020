import Data.Char (isDigit, digitToInt)

type Passport = [(String, String)]

pt1 :: IO Int
pt1 = do input <- lines <$> readFile "day-04.txt"
         -- split into passport sequences
         let passports = parsePassports input
         -- count valid passports
         return $ length $ filter validPassport passports

pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-04.txt"
         -- split into passport sequences
         let passports = parsePassports input
         -- count valid passports
         return $ length $ filter (\x -> validPassport x && validateFields x) passports

splitString :: Char -> String -> [String]
splitString c ss = splitString' c ss [[]]

splitString' :: Char -> String -> [String] -> [String]
splitString' _ [] acc = reverse acc
splitString' c (s:ss) (a:acc)
  | c == s = splitString' c ss ([]:a:acc)
  | otherwise = splitString' c ss ((a ++ [s]):acc)

validPassport :: Passport -> Bool
validPassport kvs
  | len == 8 = validateFields kvs
  | len < 7 = False
  | len == 7 = case lookup "cid" kvs of
                 Just _ -> False
                 Nothing -> validateFields kvs
    where len = length kvs 

parsePassports :: [String] -> [Passport]
parsePassports xs = parsePassports' xs []

parsePassports' :: [String] -> Passport -> [Passport]
parsePassports' [] pp = [pp]
parsePassports' (x:xs) pp
    | x == "" = pp:parsePassports xs
    | otherwise = parsePassports' xs (pp ++ kvs')
    where kvs = splitString ' ' x
          kvs' = parseKVs kvs

parseKVs :: [String] -> Passport
parseKVs [] = []
parseKVs (kv:kvs) = (k,v):parseKVs kvs
    where [k, v] = splitString ':' kv

validateFields :: Passport -> Bool
validateFields = all validateField 

validateField :: (String, String) -> Bool
validateField ("byr", byr) = validYear byr 1920 2002
validateField ("iyr", iyr) = validYear iyr 2010 2020
validateField ("eyr", eyr) = validYear eyr 2020 2030
validateField ("hgt", hgt) = validHeight hgt
validateField ("hcl", hcl) = validHairColour hcl
validateField ("ecl", ecl) = validEyeColour ecl
validateField ("pid", pid) = validPid pid
validateField ("cid", _)   = True
validateField (_,_)   = True

validYear :: String -> Int -> Int -> Bool
validYear yr minYear maxYear = (length yr == 4) && all isDigit yr && between yr' minYear maxYear
    where yr' = strToInt yr

strToInt :: String -> Int
strToInt xs = foldl (\x y -> x*10 + y) 0 $ map digitToInt xs

validHeight :: String -> Bool
validHeight hgt
  | length hgt <= 2 = False
  | not (all isDigit hgt') = False
  | unit == "cm" = between hgt'' 150 193
  | unit == "in" = between hgt'' 59 76
  | otherwise = False
    where (hgt', unit) = splitAt (length hgt-2) hgt
          hgt'' = strToInt hgt'

between :: Int -> Int -> Int -> Bool
between v minV maxV = v >= minV && v <= maxV

validHairColour :: String -> Bool
validHairColour ('#':xs) = length xs == 6 && all (\x -> isDigit x || elem x ['a'..'f']) xs
validHairColour _ = False

validEyeColour :: String -> Bool
validEyeColour "amb" = True
validEyeColour "blu" = True
validEyeColour "brn" = True
validEyeColour "gry" = True
validEyeColour "grn" = True
validEyeColour "hzl" = True
validEyeColour "oth" = True
validEyeColour _     = False

validPid :: String -> Bool
validPid xs = length xs == 9 && all isDigit xs


