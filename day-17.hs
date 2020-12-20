data Cube = Active | Inactive
    deriving (Eq)

type Grid = [[[Cube]]]
type Grid4 = [[[[Cube]]]]

instance Show Cube where
    show Active = "#"
    show Inactive = "."

-- create a 3D inactive cube
initCube :: Int -> Int -> Int -> Grid
initCube x y z = replicate x $ replicate y $ replicate z Inactive 

readCube :: Char -> Cube
readCube '#' = Active
readCube '.' = Inactive
readCube _ = error "unknown cube character"

pt1 :: IO Int 
pt1 = do input <- lines <$> readFile "day-17.txt"
         let startLayer = map (map readCube) input
             startGrid = [startLayer]
         endGrid <- playConway startGrid 6
         return $ numActive (concat (concat endGrid))
         

playConway :: Grid -> Int -> IO Grid
playConway cube 0 = do print cube
                       return cube
playConway cube n = do print cube
                       playConway (conwayStep cube) (n-1)

addShell :: Grid -> Grid
addShell cube = c3
    where (_, y, z) = cubeSize cube
          rowX = replicate y $ replicate z Inactive
          rowY = replicate z Inactive

          -- add to x 
          c1 = rowX:cube ++ [rowX]
          -- add to y
          c2 = map (\y -> rowY:y ++ [rowY]) c1
          c3 = map (map (\z -> Inactive:z ++ [Inactive])) c2

cubeSize :: Grid -> (Int, Int, Int)
cubeSize cube = (x,y,z)
    where x = length cube
          y = length $ head cube
          z = length $ head $ head cube

-- take a step in conway
conwayStep :: Grid -> Grid
conwayStep cube = [[[nextState (cube' !! i !! j !! k) (neighbours (i,j,k) cube') 
                    |k <- [0..lz-1]]
                    |j <- [0..ly-1]]
                    |i <- [0..lx-1]]
    where cube' = addShell cube
          (lx, ly, lz) = cubeSize cube'

-- find the neighbours of a given grid position
neighbours :: (Int, Int, Int) -> Grid -> [Cube]
neighbours (x,y,z) cube = [((cube !! i) !! j) !! k | 
                            i <- [x-1..x+1], i >= 0, i < lx, 
                            j <- [y-1..y+1], j >= 0, j < ly, 
                            k <- [z-1..z+1], k >= 0, k < lz,
                            i /= x || j /= y || k /= z]
    where (lx, ly, lz) = cubeSize cube

-- determine the next state of a cube based on its current state and its neighbours
nextState :: Cube -> [Cube] -> Cube
nextState Active ns
  | numActive ns == 2 = Active
  | numActive ns == 3 = Active
  | otherwise = Inactive
nextState Inactive ns
  | numActive ns == 3 = Active
  | otherwise = Inactive

numActive :: [Cube] -> Int
numActive ns = length $ filter (==Active) ns

pt2 :: IO Int
pt2 = do input <- lines <$> readFile "day-17.txt"
         let startLayer = map (map readCube) input
             startGrid = [[startLayer]]
         endGrid <- playConway2 startGrid 6
         return $ numActive (concat (concat (concat endGrid)))

conwayStep2 :: Grid4 -> Grid4
conwayStep2 cube = [[[[nextState (cube' !! i !! j !! k !! l) (neighbours2 (i,j,k,l) cube') 
                    |k <- [0..lz-1]]
                    |j <- [0..ly-1]]
                    |i <- [0..lx-1]]
                    |l <- [0..lw-1]]
    where cube' = addShell2 cube
          (lx, ly, lz, lw) = cubeSize2 cube'

-- find the neighbours of a given grid position
neighbours2 :: (Int, Int, Int, Int) -> Grid4 -> [Cube]
neighbours2 (x,y,z,w) cube = [cube !! i !! j !! k !! l | 
                            i <- [x-1..x+1], i >= 0, i < lx, 
                            j <- [y-1..y+1], j >= 0, j < ly, 
                            k <- [z-1..z+1], k >= 0, k < lz,
                            l <- [w-1..w+1], l >= 0, l < lw,
                            i /= x || j /= y || k /= z || l /= w]
    where (lx, ly, lz, lw) = cubeSize2 cube

playConway2 :: Grid4-> Int -> IO Grid4
playConway2 cube 0 = do --print cube
                        return cube
playConway2 cube n = do --print cube
                        playConway2 (conwayStep2 cube) (n-1)

addShell2 :: Grid4 -> Grid4
addShell2 cube = c4
    where (_, y, z, w) = cubeSize2 cube
          rowX = replicate y $ replicate z $ replicate w Inactive
          rowY = replicate z $ replicate w Inactive
          rowZ = replicate w Inactive

          -- add to x 
          c1 = rowX:cube ++ [rowX]
          -- add to y
          c2 = map (\y -> rowY:y ++ [rowY]) c1
          -- add to z
          c3 = map (map (\z -> rowZ:z ++ [rowZ])) c2
          -- add to w
          c4 = map (map (map (\w -> Inactive:w ++ [Inactive]))) c3

cubeSize2 :: Grid4 -> (Int, Int, Int,Int)
cubeSize2 cube = (x,y,z,w)
    where x = length cube
          y = length $ head cube
          z = length $ head $ head cube
          w = length $ head $ head $ head cube

