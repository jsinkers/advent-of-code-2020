data ShipAction = N Int 
                | S Int 
                | E Int 
                | W Int 
                | L Int
                | R Int
                | F Int

type Posn = Int
type Direction = Int
type ShipState = (Posn, Posn, Direction)

type WaypointState = (Posn, Posn, Posn, Posn)

pt1 :: IO Int
pt1 = do input <- lines <$> readFile "day-12.txt"
         let actions = map toAction input
             (x, y, _) = foldl takeAction (0,0,0) actions
         return $ manhattanDistance x y

takeAction :: ShipState -> ShipAction -> ShipState
takeAction (x, y, d) (N v) = (x, y+v, d)
takeAction (x, y, d) (S v) = (x, y-v, d)
takeAction (x, y, d) (E v) = (x+v, y, d)
takeAction (x, y, d) (W v) = (x-v, y, d)
takeAction (x, y, d) (L v) = (x, y, d+v)
takeAction (x, y, d) (R v) = (x, y, d-v)
takeAction (x, y, d) (F v) = case d `mod` 360 of
                               0 -> takeAction (x, y, d) (E v)
                               90 -> takeAction (x, y, d) (N v)
                               180 -> takeAction (x, y, d) (W v)
                               270 -> takeAction (x, y, d) (S v)

manhattanDistance :: Int -> Int -> Int
manhattanDistance x y = abs x + abs y

toAction :: String -> ShipAction
toAction ('N':xs) = N (read xs :: Int)
toAction ('S':xs) = S (read xs :: Int)
toAction ('E':xs) = E (read xs :: Int)
toAction ('W':xs) = W (read xs :: Int)
toAction ('L':xs) = L (read xs :: Int)
toAction ('R':xs) = R (read xs :: Int)
toAction ('F':xs) = F (read xs :: Int)
toAction _ = error "Unknown Action"


pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-12.txt"
         let actions = map toAction input
             (x, y, _, _) = foldl takeAction2 (0,0,10,1) actions
         return $ manhattanDistance x y

takeAction2 :: WaypointState -> ShipAction -> WaypointState
takeAction2 (x, y, s, t) (N v) = (x, y, s, t+v)
takeAction2 (x, y, s, t) (S v) = (x, y, s, t-v)
takeAction2 (x, y, s, t) (E v) = (x, y, s+v, t)
takeAction2 (x, y, s, t) (W v) = (x, y, s-v, t)
takeAction2 (x, y, s, t) (L v) = (x, y, s', t')
    where (s', t') = rotateCW (s,t) v
takeAction2 (x, y, s, t) (R v) = (x, y, s', t')
    where (s', t') = rotateCCW (s,t) v
takeAction2 (x, y, s, t) (F v) = (x + s*v, y+t*v, s, t)

rotateCW :: (Int, Int) -> Int -> (Int, Int)
rotateCW (x, y) 0 = (x,y)
rotateCW (x, y) 90 = (-y, x)
rotateCW (x, y) 180 = (-x, -y)
rotateCW (x, y) 270 = (y, -x)
rotateCW _ _ = error "Unknown angle"

rotateCCW :: (Int, Int) -> Int -> (Int, Int)
rotateCCW (x, y) 0 = (x, y)
rotateCCW (x, y) 90 = rotateCW (x, y) 270
rotateCCW (x, y) 180 = rotateCW (x, y) 180
rotateCCW (x, y) 270 = rotateCW (x, y) 90
rotateCCW _ _ = error "Unknown angle"

