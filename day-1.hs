
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile


pt1 :: IO Int
pt1 = do lines <- readLines "day-1.txt" 
         let expenses = map expense lines
         let sums = [(x,y) | x <- expenses, y <- expenses, x + y == 2020]
         let (x,y) = head sums
         return $ x*y

pt2 :: IO Int 
pt2 = do lines <- readLines "day-1.txt" 
         let expenses = map expense lines
         let sums = [(x,y,z) | x <- expenses, y <- expenses, z <- expenses, x + y + z== 2020]
         let (x,y,z) = head sums
         return $ x*y*z

expense :: String -> Int
expense x = read x :: Int 
