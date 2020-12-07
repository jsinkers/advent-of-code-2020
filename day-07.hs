import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import Data.List (nub)

type Qty = Int
type Colour = String
data BagContainer = BagContainer Colour [(Colour, Qty)]
    deriving (Show)

pt1 :: IO Int -- [String]
pt1 = do input <- lines <$> readFile "day-07.txt"
         let bags = parseBagRules input
         -- return $ map show bags
         return $ length $ nub $ map (\(BagContainer c _) -> c) $ bagsContaining "shinygold" bags

parseBagRules :: [String] -> [BagContainer]
parseBagRules = map parseBagRule

parseBagRule :: String -> BagContainer
parseBagRule xs = BagContainer container' containees'
    where (container:containees:_) = splitOn "contain" xs
          container' = parseContainer $ words container
          containees' = parseContainees containees

parseContainer :: [String] -> Colour
parseContainer (w1:w2:_) = w1 ++ w2 
parseContainer container = error "couldn't parse container: " ++ show container

parseContainees :: String -> [(Colour, Qty)]
parseContainees " no other bags." = []
parseContainees xs = map parseBag $ splitOn "," xs

parseBag :: String -> (Colour, Qty)
parseBag xs = (c1 ++ c2, strToInt qty)
    where (qty:c1:c2:_) = words xs

strToInt :: String -> Int
strToInt xs = foldl (\x y -> x*10 + y) 0 $ map digitToInt xs

bagsContaining :: String -> [BagContainer] -> [BagContainer]
bagsContaining c bags = containers ++ concatMap (\(BagContainer col _) -> bagsContaining col bags) containers
    where containers = filter (bagCanCarry c) bags


bagCanCarry :: String -> BagContainer -> Bool
bagCanCarry bag (BagContainer _ containees) = case lookup bag containees of
                                              Nothing -> False
                                              Just _ -> True

pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-07.txt"
         let bags = parseBagRules input
         --return bags
         -- return $ findBag "shinygold" bags
         return $ containedBags "shinygold" bags

containedBags :: String -> [BagContainer] -> Int
containedBags bag bagContainers = case findBag bag bagContainers of 
                                    Just (BagContainer _ bags) -> (sum $ map (\(c, q) -> q*(1+containedBags c bagContainers)) bags)
                                    Nothing -> 0

--containedBags :: String -> [BagContainer] -> [(BagContainer, String)]
--containedBags bag bagContainers = case findBag bag bagContainers of 
                                    --Just (BagContainer _ bags) -> sum $ map (\(c, q) -> q*containedBags c bagContainers) bags
                                    --Nothing -> 0

findBag :: String -> [BagContainer] -> Maybe BagContainer
findBag _ [] = Nothing
findBag bag (c@(BagContainer colour _):cs)
    | colour == bag = Just c
    | otherwise = findBag bag cs

test_b :: [BagContainer]
test_b = [BagContainer "lightred" [("brightwhite",1),("mutedyellow",2)],BagContainer "darkorange" [("brightwhite",3),("mutedyellow",4)],BagContainer "brightwhite" [("shinygold",1)],BagContainer "mutedyellow" [("shinygold",2),("fadedblue",9)],BagContainer "shinygold" [("darkolive",1),("vibrantplum",2)],BagContainer "darkolive" [("fadedblue",3),("dottedblack",4)],BagContainer "vibrantplum" [("fadedblue",5),("dottedblack",6)],BagContainer "fadedblue" [],BagContainer "dottedblack" []]
