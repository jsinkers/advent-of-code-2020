import Data.List.Split (splitOn)
import Data.List (transpose, permutations, isPrefixOf, sort)
import Data.Maybe (fromJust)

-- Field "fieldname" [accepted values]
data Field = Field String [Int]

instance Show Field where
    show (Field n _) = "Field " ++ n

type Ticket = [(Maybe String, Int)]

pt1 :: IO Int
pt1 = do input <- lines <$> readFile "day-16.txt"
         let (fields, input') = parseFields ([], input)
         let (_, input'') = parseYourTicket input'
         let tickets = parseOtherTickets input''
             scanningError = sum $ concatMap (invalidFields fields) tickets
         return scanningError

parseFields :: ([Field], [String]) -> ([Field], [String])
parseFields (fs, "":xs) = (fs, xs)
parseFields (fs, x:xs) = parseFields (Field name acceptedVals:fs, xs)
    where [name, vals] = splitOn ":" x
          vals' = splitOn "or" vals
          vals'' = map (splitOn "-") vals'
          vals''' = map (map read) vals'' :: [[Int]]
          acceptedVals = concatMap (\[x,y]-> [x..y]) vals'''

parseFields2 :: ([Field2], [String]) -> ([Field2], [String])
parseFields2 (fs, "":xs) = (fs, xs)
parseFields2 (fs, x:xs) = parseFields2 (Field2 name min1 max1 min2 max2:fs, xs)
    where [name, vals] = splitOn ":" x
          vals' = splitOn "or" vals
          vals'' = map (splitOn "-") vals'
          vals''' = map (map read) vals'' :: [[Int]]
          [min1, max1, min2, max2] = concat vals'''

parseYourTicket :: [String] -> ([Int], [String])
parseYourTicket (_:t:_:xs) = (parseTicket t, xs)

parseOtherTickets :: [String] -> [[Int]]
parseOtherTickets (_:xs) = map parseTicket xs


parseTicket :: String -> [Int]
parseTicket xs = map read (splitOn "," xs) :: [Int] 

invalidFields :: [Field] -> [Int] -> [Int]
invalidFields fs = filter (\t -> not $ any (\(Field _ f) -> t `elem` f) fs) 

invalidFields2 :: [Field2] -> [Int] -> [Int]
invalidFields2 fs = filter (\t -> not $ any (inField t) fs)

pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-16.txt"
         let (fields, input') = parseFields2 ([], input)
         let (yourTicket, input'') = parseYourTicket input'
         putStr "Your ticket: "
         print yourTicket
         let tickets = parseOtherTickets input''
             validTickets = filter (null . invalidFields2 fields) tickets
         putStr "Num valid tickets: " 
         print $ length validTickets
         let fieldMap = determineFields2 fields validTickets
         putStr "Ticket Fields: "
         print fieldMap
         let depFields = filter (\(Field2 f _ _ _ _, _) -> "departure" `isPrefixOf` f) fieldMap
         putStr "Departure Fields: "
         print depFields 
         let depInds = map snd depFields
             yourFields = map (yourTicket !!) depInds
         putStr "Your fields: "
         print yourFields
         return $ product yourFields


-- determineFields :: [Field] -> [[Int]] -> [(Field,Int)]
-- determineFields fs ts = zip asst [0..]
--     where tFields = transpose ts
--           -- generate all possible assignments of fields to ticket numbers
--           -- unsurprisingly - extraordinarily slow
--           fieldAssignments = permutations fs
--           -- test the assignment
--           -- asst = filter (\( all (\(f,t) -> (validAssignment f t)) fa tFields)) fieldAssignments
--           asst = head $ filter validAssignments fieldAssignments
--           validAssignments f = all (\(f,t) -> validAssignment t f) fts
--               where fts = zip f tFields

determineFields2 :: [Field2] -> [[Int]] -> [(Field2,Int)]
determineFields2 fs ts = recursiveFields [] fs tFields tMinMax
    where tFields = transpose ts
          tMinMax = map (\xs -> (minimum xs, maximum xs)) tFields

-- take 3
-- rather than generating all permutations, it would make more sense to find a matching column, fix it, then repeat for the next column?
-- recursively generate: main challenge is keeping track of what to test and so on
recursiveFields :: [(Field2, Int)] -> [Field2] -> [[Int]] -> [(Int, Int)] -> [(Field2, Int)]
recursiveFields assigned [] _ _ = assigned
recursiveFields assigned (f:fs) tFields tMinMax = case length fields of 
                                                    -- if we found a unique possibility, assign that as the answer
                                                    1 -> let (_,_,n) = head fields in recursiveFields ((f,n):assigned) fs tFields tMinMax
                                                    -- if there are multiple options, let's save it for later
                                                    _ -> recursiveFields assigned (fs ++ [f]) tFields tMinMax
    where keys = map snd assigned 
          candidates = filter (\(_,_,n) -> n `notElem` keys) (zip3 tFields tMinMax [0..])
          fields = filter (\(t, mm, _) -> validAssignment2 t mm f) candidates 


generateAssignments :: [Field2] -> [[Int]] -> [[Field2]]
generateAssignments fs valid = filter allowed perms
    where lu = zip fs valid
          perms = permutations fs 
          allowed xs = all (\(f,n) -> n `elem` (fromJust $ lookup f lu)) (zip xs [0..])

validAssignment :: [Int] -> Field -> Bool
validAssignment vals (Field _ accepted) = all (`elem` accepted) vals

testAssignment :: (Int, Int) -> Field2 -> Bool
testAssignment (l,h) f = l `inField` f && h `inField` f 

validAssignment2 :: [Int] -> (Int, Int) -> Field2 -> Bool
validAssignment2 vals (l,h) f = l `inField` f && h `inField` f && all (`inField` f) vals


main :: IO ()
main = do x <- pt2
          print x

data Field2 = Field2 String Int Int Int Int
    deriving Eq

instance Show Field2 where
    show (Field2 n _ _ _ _) = "Field2 " ++ n

inField :: Int -> Field2 -> Bool
inField v (Field2 _ min1 max1 min2 max2) = (v >= min1 && v <= max1)||(v >= min2 && v <= max2)



