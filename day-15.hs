import Data.List (elemIndex)
import Data.Map (insert, Map)
import qualified Data.Map as Map

game :: [Integer] -> [Integer]
game l@(x:xs) = case elemIndex x xs of
                  Nothing -> 0:l
                  Just i -> (fromIntegral i+1):l

--startNums = reverse [0,3,6]
startNums = reverse [8,0,17,4,1,12]
startNums2 = [8,0,17,4,1]

playGame :: Integer -> [Integer]
playGame n = until (\xs -> length xs == fromIntegral n) game startNums


-- the approach for pt 1 above is much too slow to determine the 30,000,000th
-- number in the game.  This is because lookup of each number is O(n).  If we
-- use a tree-based map, we can got O(log n) lookup and insertion.  The dictionary will store
-- the number as a key, and the turn number as a value.  Each time the number
-- is played, the number will be updated
game2 :: (Integer, Integer, Map Integer Integer) -> (Integer, Integer, Map Integer Integer)
game2 (last, turn, m) = case Map.lookup last m of
                          Nothing -> (0, turn + 1, insert last turn m)
                          Just lastTurn -> (turn-lastTurn, turn+1, insert last turn m)

playGame2 :: Integer -> (Integer, Integer, Map Integer Integer)
playGame2 n = until (\(_, x, _) -> x >= n) game2 (12, 6, startMap)

startMap = Map.fromList $ zip startNums2 [1..]

showGame :: (Integer, Integer, Map Integer Integer) -> String
showGame (last, turn, m) = "last: " ++ show last ++ "\n" ++ 
                           "turn: " ++ show turn ++ "\n" ++ 
                           "map: " ++ show m ++ "\n"
main :: IO ()
main = do let (x,t,_) = playGame2 30000000
          putStr "Last: "
          print x
          putStr "Turn: "
          print t

-- should also investigate hashtables in Haskell and profile the difference
