import Data.List.Split (splitOn)
--import Data.Char (digitToInt)
import qualified Data.Map as Map

data Rule = Terminal Int Char | NonTerminal Int [[Int]]
    deriving Show

parseRule :: String -> Rule
parseRule inp  
  | '\"' `elem` rest = Terminal n' (head $ filter (`elem` ['a'..'z']) rest)
  | '|' `elem` rest = NonTerminal n' (readWords rest)
  | otherwise = NonTerminal n' (readWords2 rest)
    where [n, rest] = splitOn ":" inp
          n' = read n :: Int
          ruleLists x = splitOn "|" x
          readWords x = map (\x -> map read (words x)) $ ruleLists x :: [[Int]]
          readWords2 x = [map read $ words x] :: [[Int]]

getRuleNum :: Rule -> Int
getRuleNum (Terminal n _) = n
getRuleNum (NonTerminal n _) = n

pt1 :: IO Int 
pt1 = do input <- lines <$> readFile "day-19-test.txt"
         let [ruleInput, dataInput] = splitOn [""] input 
             rules = map parseRule ruleInput
             rules' = zip (map getRuleNum rules) rules 
             rulesMap = Map.fromList rules'
             validStrings = filter (`validInput` rulesMap) dataInput
         print rulesMap
         print dataInput
         print validStrings
         return $ length validStrings

validInput :: String -> Map.Map Int Rule -> Bool
validInput xs rulesMap = applyRule xs 0 rulesMap == Just "" 
    
applyRule :: String -> Int -> Map.Map Int Rule -> Maybe String
applyRule [] n rules = Nothing
applyRule inp@(x:xs) n rules = case rule of
     Just (Terminal _ c) -> if x == c 
                               then Just xs 
                               else Nothing
     Just (NonTerminal _ rules') -> case rules' of
                                      [rs] -> ar rs
                                      [r1,r2] -> if ar r1 == Just ""
                                                    then Just ""
                                                    else ar r2
         --foldl arStep Nothing rules'
         -- map ar rules'
         -- test each set of rules
         -- for each set, fold until it fails or succeeds
     Nothing -> error ("unknown rule " ++ show n)
    where rule = Map.lookup n rules
          step (Just xs) n = applyRule xs n rules
          step Nothing _ = Nothing
          ar rs = foldl step (Just inp) rs
          arStep (Just "") rs = Just ""
          arStep _ rs = ar rs 
