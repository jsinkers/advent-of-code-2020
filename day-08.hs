
data Instruction = Nop Int | Acc Int | Jmp Int
    deriving (Show)

pt1 :: IO Int
pt1 = do input <- lines <$> readFile "day-08.txt"
         let instructions = map readInstruction input
         return $ runProgramNoLoop instructions

readInstruction :: String -> Instruction
readInstruction input 
  | word == "nop" = Nop arg'
  | word == "acc" = Acc arg'
  | word == "jmp" = Jmp arg'
  | otherwise = error "invalid input"
  where (word:arg:_) = words input
        arg' = (read $ filter (/= '+') arg) :: Int

-- returns accumulator value
runProgramNoLoop :: [Instruction] -> Int
runProgramNoLoop instructions = runProgramNoLoop' instructions ([], 0, 0)

runProgramNoLoop' :: [Instruction] -> ([Int], Int, Int) -> Int
runProgramNoLoop' is (evals, acc, next) = if next `elem` evals 
                                             then acc 
                                             else runProgramNoLoop' is (next:evals, acc', next')
    where nextInst = is !! next
          (acc', next') = evalInstruction (nextInst, acc, next)


evalInstruction :: (Instruction, Int, Int) -> (Int, Int)
evalInstruction (Nop _, acc, n) = (acc, n+1)
evalInstruction (Acc v, acc, n) = (acc+v, n+1)
evalInstruction (Jmp v, acc, n) = (acc, n+v)

pt2 :: IO [Maybe Int]
pt2 = do input <- lines <$> readFile "day-08.txt"
         let instructions = map readInstruction input
             instructions' = modifyInstructions instructions
         return $ map runProgram instructions'

runProgram :: [Instruction] -> Maybe Int
runProgram is = runProgram' is ([], 0, 0)

-- returns accumulator value if it succeeds, otherwise returns nothing
runProgram'  :: [Instruction] -> ([Int], Int, Int) -> Maybe Int
runProgram' is (evals, acc, next) 
    | next >= length is = Just acc
    | otherwise = if next `elem` evals 
                     then Nothing 
                     else runProgram' is (next:evals, acc', next')
     where nextInst = is !! next
           (acc', next') = evalInstruction (nextInst, acc, next)

modifyInstructions :: [Instruction] -> [[Instruction]]
modifyInstructions is = [modifyProgram n is | n <- [0..length is]]

modifyProgram :: Int -> [Instruction] -> [Instruction]
modifyProgram n is = h ++ t'
    where (h, t) = splitAt (n+1) is
          t' = if null t 
                  then [] 
                  else modifyInstruction (head t):tail t

modifyInstruction :: Instruction -> Instruction
modifyInstruction (Nop v) = Jmp v
modifyInstruction (Jmp v) = Nop v
modifyInstruction i@(Acc _) = i
