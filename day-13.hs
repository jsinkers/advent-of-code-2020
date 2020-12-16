import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)

pt1 :: IO Integer
pt1 = do input <- lines <$> readFile "day-13.txt"
         let earliestTimestamp = read $ head input :: Integer
             buses = filter isJust $ map readBus (splitOn "," (head $ tail input))
             buses' = map fromJust buses
             bus = closestDeparture buses' earliestTimestamp
         return $ bus*(nextMultiple earliestTimestamp bus - earliestTimestamp)

readBus :: String -> Maybe Integer
readBus "x" = Nothing
readBus xs = Just (read xs :: Integer)

closestDeparture :: [Integer] -> Integer -> Integer
closestDeparture xs t = case ind of 
                          Nothing -> error "invalid departure time"
                          Just bus -> xs !! bus
    where times = map (nextMultiple t) xs
          ind = elemIndex (minimum times) times

nextMultiple :: Integer -> Integer -> Integer
nextMultiple t x = ceiling (realToFrac t/realToFrac x)*x


pt2 :: IO Integer
pt2 = do input <- lines <$> readFile "day-13.txt"
         let buses = map readBus (splitOn "," (head $ tail input))
             buses' = filter (isJust . fst) $ zip buses [0..]
             buses'' = map (\(x,y) -> (fromJust x, y)) buses'
         return $ chineseRemainderTheorem (map fst buses'') (map snd buses'')

findTime :: [(Integer, Integer)] -> Integer
findTime buses = until (validTimes buses) (+x) (nextMultiple 100000000000000 x)
    where (x,_) = head buses

findTime2' :: [(Integer, Integer)] -> IO Integer
findTime2' buses = do findTime2 buses x
    where (x,_) = head buses

findTime2 :: [(Integer, Integer)] -> Integer -> IO Integer
findTime2 buses t = do print t
                       if not $ validTimes buses t 
                          then findTime2 buses (t+x)
                          else return t 
    where (x,_) = head buses

validTimes :: [(Integer, Integer)] -> Integer -> Bool
validTimes buses t = all (\(x,y) -> nextMultiple t x - t == y) buses
    
main :: IO ()
main = do x <- pt2
          print x
          return ()

-- https://crypto.stanford.edu/pbc/notes/numbertheory/crt.html
-- https://www.reddit.com/r/haskell/comments/kck870/day_13_advent_of_code_2020/
chineseRemainderTheorem :: [Integer] -> [Integer] -> Integer
chineseRemainderTheorem ms as = sum (map (\(a,b,b') -> -a*b*b') (zip3 as bs bs')) `mod` m
    where m = product ms
          bs = map (\x -> m `div` x) ms
          bs' = map (\(b, mi) -> b `invMod` mi) (zip bs ms)

invMod :: Integer -> Integer -> Integer
invMod a m = i `mod` m
    where (_,i,_) = gcd' a m

gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

gcd' 0 b = (b, 0, 1)
gcd' a b = (g, t - (b `div` a) * s, s)
  where
    (g, s, t) = gcd' (b `mod` a) a

crt :: [(Integer, Integer)] -> Integer
crt eqs = foldr (\(a, m) acc -> acc + (a * b m * (b m `modInv` m))) 0 eqs `mod` terms
  where
    terms = product $ map snd eqs
    b m = terms `div` m
    -- Modular Inverse
    a `modInv` m = let (_, i, _) = gcd a m in i `mod` m
    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd (b `mod` a) a

