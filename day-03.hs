
pt1 :: IO Int
pt1 = do course <- lines <$> readFile "day-03.txt" 
         return $ traverseCourse course

type Index = Int
type NumTrees = Int
type Course = [String]

traverseCourse :: Course -> NumTrees
traverseCourse course = traverseCourse' (0, 0, course)

traverseCourse' :: (Index, NumTrees, Course) -> NumTrees
traverseCourse' (_, numTrees, []) = numTrees
-- handle first line separately
traverseCourse' (0, numTrees, _:cs) = traverseCourse' (3, numTrees, cs)
traverseCourse' (n, numTrees, c:cs) = traverseCourse' (n + 3, numTrees', cs)
    where numTrees' = if cycle c !! n == '#' 
                         then numTrees + 1 
                         else numTrees
    
slopes = [(1,1), (3,1), (5,1), (7, 1), (1, 2)]
--slopes = [(3,1)]

pt2 :: IO Int
pt2 = do course <- lines <$> readFile "day-03.txt" 
         return $ product $ map (traverseCourseParam course) slopes

traverseCourseParam :: Course -> (Int, Int) -> NumTrees
traverseCourseParam course = traverseCourseParam' (0, 0, course) 

traverseCourseParam' :: (Index, NumTrees, Course) -> (Int, Int) -> NumTrees
traverseCourseParam' (_, numTrees, []) _ = numTrees
traverseCourseParam' (0, numTrees, cs) (numRight, numDown) = traverseCourseParam' (numRight, numTrees, drop numDown cs) (numRight, numDown)
traverseCourseParam' (n, numTrees, c:cs) (numRight, numDown) = traverseCourseParam' (n + numRight, numTrees', drop numDown (c:cs)) (numRight, numDown)
    where numTrees' = if cycle c !! n == '#' 
                         then numTrees + 1 
                         else numTrees
