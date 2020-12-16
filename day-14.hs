import Data.List (deleteBy)
import Data.List.Split (splitOn)
import Data.Function (on)
import Data.Maybe (isNothing, fromJust)

type BitList = [Int]
type BitMask = [Maybe Int]
type Address = Int
type Value = Int
type Memory = [(Address, Value)]
type State = (Memory, BitMask)

newtype MemUpdate = Address Value

toBinary :: Int -> BitList
toBinary 0 = [0]
toBinary 1 = [1]
toBinary n
    | even n = toBinary (n `div` 2) ++ [0]
    | odd n = toBinary (n `div` 2) ++ [1]

toDecimal :: BitList -> Int
toDecimal = foldl (\a v -> 2*a + v) 0 

padBinary :: Int -> BitList -> BitList
padBinary n xs = replicate (n - length xs) 0 ++ xs

writeToMemory :: State -> (Address, Value) -> State
writeToMemory (mem, bm) (add, v) = (replaceByFst (add, applyBitmask v bm) mem, bm)

updateBitmask :: State -> BitMask -> State
updateBitmask (mem, _) bm = (mem, bm)

-- from Data.List.Util
-- | Replace the first occurrence in a lookup table.
replaceByFst :: Eq a
             => (a, b)   -- ^ The replacement key and value.
             -> [(a, b)] -- ^ The lookup table.
             -> [(a, b)] -- ^ The updated table.
replaceByFst x = (x :) . deleteBy ((==) `on` fst) x

applyBitmask :: Value -> BitMask -> Value
applyBitmask v bm = toDecimal $ zipWith (curry bitValue) bv bm
    where bv = padBinary 36 (toBinary v)

bitValue :: (Int, Maybe Int) -> Int
bitValue (x, Nothing) = x
bitValue (_, Just x) = x

parseBitMask :: Char -> Maybe Int
parseBitMask 'X' = Nothing
parseBitMask '0' = Just 0
parseBitMask '1' = Just 1

parseMem :: String -> (Address, Value)
parseMem xs = (parseAdd add, read val :: Int)
    where [add, val] = splitOn "=" xs

parseAdd :: String -> Address
parseAdd xs = read (filter (`notElem` "[]me") xs) :: Int

pt1 :: IO Int
pt1 = do input <- lines <$> readFile "day-14.txt"
         let program = map parseProgram input
             final = foldl runInstruction ([],[]) program
         print program
         print final
         return $ sum $ map snd (fst final)


parseProgram :: String -> Either BitMask (Address, Value)
parseProgram ('m':'a':'s':'k':' ':'=':' ':xs) = Left (map parseBitMask xs)
parseProgram ('m':'e':'m':'[':xs) = Right (parseMem xs)

runInstruction :: State -> Either BitMask (Address, Value) -> State
runInstruction s (Left bm) = updateBitmask s bm
runInstruction s (Right m) = writeToMemory s m

pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-14.txt"
         let program = map parseProgram input
             -- final = foldl runInstruction2 ([],[]) program
         runProgram ([], []) program
         --print program
         --print final

-- interestingly - due to lazyness - all evaluation is deferred until the sum is requested
runProgram :: State -> [Either BitMask (Address, Value)] -> IO Int
runProgram s [] = return $ sum $ map snd (fst s)
runProgram s (p:ps) = do print $ length ps
                         runProgram (runInstruction2 s p) ps

runInstruction2 :: State -> Either BitMask (Address, Value) -> State
runInstruction2 s (Left bm) = updateBitmask s bm
runInstruction2 s (Right m) = writeToMemory2 s m

writeToMemory2 :: State -> (Address, Value) -> State
writeToMemory2 (mem, bm) (add, val) = foldl (\(m,b) a -> writeToMemory3 (m, b) (a,val)) (mem, bm) adds
    where adds = applyBitmask2 add bm
-- writeToMemory (mem, bm) (add, v) = (replaceByFst (add, applyBitmask v bm) mem, bm)

writeToMemory3 :: State -> (Address, Value) -> State
writeToMemory3 (mem, bm) (add, v) = (replaceByFst (add, v) mem, bm)

bitValue2 :: (Int, Maybe Int) -> Maybe Int
bitValue2 (_, Nothing) = Nothing
bitValue2 (x, Just 0) = Just x
bitValue2 (_, Just 1) = Just 1

applyBitmask2 :: Address -> BitMask -> [Address]
applyBitmask2 v bm = map toDecimal adds
    where bv = padBinary 36 (toBinary v)
          floatingAdd = zipWith (curry bitValue2) bv bm
          adds = unfloatAdd floatingAdd

-- unfloat addresses - i.e. produce the corresponding list of addresses from a floating address
unfloatAdd :: [Maybe Int] -> [[Int]]
unfloatAdd = foldl (\acc v -> if isNothing v 
                                    then concatMap floating acc 
                                    else map (++[fromJust v]) acc) [[]] 
          
-- Any floating bits get replaced with 2 lists, the Cartesian product of the list with [0],[1]
floating :: [Int] -> [[Int]]
floating xs = (++) <$> [xs] <*> [[0],[1]]

--bitValue2 :: (Int, Maybe Int) -> Maybe Int

bm :: [Maybe Int]
bm = [Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Just 0,Nothing,Just 1,Just 0,Just 0,Just 1,Nothing]
add :: Int
add = 42

main :: IO ()
main = do x <- pt2
          print x
