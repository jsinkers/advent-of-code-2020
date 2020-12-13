import Data.List (intercalate)
import Data.Maybe (fromMaybe)

data Seat = Emp | Flo | Occ
    deriving (Show, Eq)

pt1 :: IO Int
pt1 = do input <- lines <$> readFile "day-11.txt"
         let seats = readFlo input
         putStrLn "Original seats:"
         putStr $ showSeats seats
         nextStep seats
         

nextStep :: [[Seat]] -> IO Int
nextStep seats = do let neighbours = adjacentSeats seats
                    --putStrLn "Adjacent seats:"
                    --print neighbours
                    let newSeats = map (map rule) neighbours
                    --putStrLn "New Seats:" 
                    --putStr $ showSeats newSeats
                    if newSeats == seats 
                       then return $ numOccupied newSeats
                       else nextStep newSeats

numOccupied :: [[Seat]] -> Int
numOccupied seats = sum $ map (length . filter (==Occ)) seats

readSeat :: Char -> Seat
readSeat 'L' = Emp
readSeat '#' = Occ
readSeat '.' = Flo

showSeats :: [[Seat]] -> String
showSeats seats = (intercalate "\n" $ map show seats) ++ "\n"

readFlo :: [String] -> [[Seat]]
readFlo = map (map readSeat) 

-- windowing.  using indexing which is pretty ugly.  can't think of a better way
--adjacentSeats :: (Int, Int) -> [[Seat]] -> [Seat]
--adjacentSeats (i, j) seats = 
    --where minI = max i-1 0
          --minJ = max j-1 0
          --len = length $ head seats
          --maxI = min i+1 
          --maxJ = min j+ 1
          --row1 = seats !! minI

adjacentSeats :: [[Seat]] -> [[(Seat, Int)]]
-- add rows to start/end, columns at start end to simplify windowing
adjacentSeats seats = adjacentSeats' seats''
    where len = length $ head seats
          emptyRow = replicate len Emp 
          seats' = emptyRow:seats ++ [emptyRow]
          -- add empty column
          seats'' = map (\xs -> Emp:xs ++ [Emp]) seats'

adjacentSeats' :: [[Seat]] -> [[(Seat, Int)]]
adjacentSeats' (r1:r2:r3:rs) = adjacentSeatRow r1 r2 r3:adjacentSeats' (r2:r3:rs)
adjacentSeats' _ = []

adjacentSeatRow :: [Seat] -> [Seat] -> [Seat] -> [(Seat, Int)]
adjacentSeatRow (r11:l1@(r12:r13:r1s)) (r21:l2@(r22:r23:r2s)) (r31:l3@(r32:r33:r3s)) = (r22, numOcc):adjacentSeatRow l1 l2 l3
    where numOcc = length $  filter (==Occ) [r11, r12, r13, r21, r23, r31, r32, r33]
adjacentSeatRow _ _ _ = []

rule :: (Seat, Int) -> Seat
rule (Emp, 0) = Occ
rule (Occ, neighbours) = if neighbours >= 4 then Emp else Occ
rule (x, _) = x


pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-11.txt"
         let seats = readFlo input
         putStrLn "Original seats:"
         putStr $ showSeats seats
         return 1
         nextStep2 seats

nextStep2 :: [[Seat]] -> IO Int
nextStep2 seats = do let neighbours = adjacentSeats2 seats
                     --putStrLn "Adjacent seats:"
                     --print neighbours
                     let newSeats = map (map rule2) neighbours
                     --putStrLn "New Seats:" 
                     --putStr $ showSeats newSeats
                     if newSeats == seats 
                        then return $ numOccupied newSeats
                        else nextStep2 newSeats

adjacentSeats2 :: [[Seat]] -> [[(Seat, Int)]]
adjacentSeats2 seats = map (map (flip neighbours2 seats)) indexes
    where rows = length seats - 1 
          cols = length (head seats) - 1
          indexes = [[(i,j)| j <- [0..cols]]| i <- [0..rows]]

neighbours2 :: (Int, Int) -> [[Seat]] -> (Seat, Int)
neighbours2 (i, j) seats = (fromMaybe Flo (seatIJ (i, j) seats), length $ filter (==Occ) neighbs)
    where eNeighb = neighb (i, j) (\(i,j)->(i,j+1)) seats
          wNeighb = neighb (i, j) (\(i,j)->(i,j-1)) seats
          nNeighb = neighb (i, j) (\(i,j)->(i-1,j)) seats
          sNeighb = neighb (i, j) (\(i,j)->(i+1,j)) seats
          neNeighb = neighb (i, j) (\(i,j)->(i-1,j+1)) seats
          nwNeighb = neighb (i, j) (\(i,j)->(i-1,j-1)) seats
          swNeighb = neighb (i, j) (\(i,j)->(i+1,j-1)) seats
          seNeighb = neighb (i, j) (\(i,j)->(i+1,j+1)) seats
          neighbs = [eNeighb,wNeighb,nNeighb,sNeighb,neNeighb,nwNeighb,swNeighb,seNeighb]

neighb :: (Int, Int) -> ((Int, Int) -> (Int, Int)) -> [[Seat]] -> Seat
neighb (i, j) f seats = case seatIJ (i', j') seats of
                          Nothing -> Flo
                          (Just Emp) -> Emp
                          (Just Occ) -> Occ
                          (Just Flo) -> neighb (i', j') f seats
    where (i', j') = f (i, j)

seatIJ :: (Int, Int) -> [[Seat]] -> Maybe Seat
seatIJ (i, j) seats 
  | i < 0 || j < 0 || i >= length seats || j >= length (head seats) = Nothing
  | otherwise = Just $ (seats !! i) !! j

seats :: [[ Seat ]]
seats = [[Emp,Flo,Emp,Emp,Flo,Emp,Emp,Flo,Emp,Emp] ,
        [Emp,Emp,Emp,Emp,Emp,Emp,Emp,Flo,Emp,Emp] ,
        [Emp,Flo,Emp,Flo,Emp,Flo,Flo,Emp,Flo,Flo] ,
        [Emp,Emp,Emp,Emp,Flo,Emp,Emp,Flo,Emp,Emp] ,
        [Emp,Flo,Emp,Emp,Flo,Emp,Emp,Flo,Emp,Emp] ,
        [Emp,Flo,Emp,Emp,Emp,Emp,Emp,Flo,Emp,Emp] ,
        [Flo,Flo,Emp,Flo,Emp,Flo,Flo,Flo,Flo,Flo] ,
        [Emp,Emp,Emp,Emp,Emp,Emp,Emp,Emp,Emp,Emp] ,
        [Emp,Flo,Emp,Emp,Emp,Emp,Emp,Emp,Flo,Emp] ,
        [Emp,Flo,Emp,Emp,Emp,Emp,Emp,Flo,Emp,Emp]] 

rule2 :: (Seat, Int) -> Seat
rule2 (Emp, 0) = Occ
rule2 (Occ, neighbours) = if neighbours >= 5 then Emp else Occ
rule2 (x, _) = x
