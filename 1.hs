{-# OPTIONS_GHC -Wall -Werror #-}

import Data.List.Split (splitOn)
-- import Data.Sequence (chunksOf, Seq)

input1 :: IO [Int]
input1 = map read . lines <$> readFile "/home/nwest/Projects/AoC/1"

fuel :: Int -> Int
fuel = flip (-) 2 . flip quot 3

allFuel :: Int -> Int
allFuel i | f > 0 = f + allFuel f
          | otherwise = f
        where f = fuel i 

num1 :: IO Int
num1 = sum . map fuel <$> input1

num1b :: IO Int
num1b = sum . map allFuel <$> input1

-------------------

cleanCSV :: String -> [Int]
cleanCSV = map read . splitOn "," 

-- input2 :: IO [Int]
-- input2 =  cleanCSV . head . lines <$> readFile "/home/nwest/Projects/AoC/2"

crunch2 :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
crunch2 f [a, b, c] xs = let a' = head . drop a $ xs
                             b' = head . drop b $ xs
                         in take c xs ++ f a' b' : drop (succ c) xs
crunch2 _     _     xs = xs

compute2 :: Int -> [Int] -> [Int]
compute2 o xs | a == 1  = compute2 (succ o) (crunch2 (+) inst xs)
              | a == 2  = compute2 (succ o) (crunch2 (*) inst xs)
              | a == 99 = xs
              | otherwise = xs
        where a:inst = take 4 . drop (4 * o) $ xs

resetComp :: Int -> Int -> [Int] -> [Int]
resetComp _  _ [] = []
resetComp a b (x:xs) = x : a : b : drop 2 xs

test2 :: [Int]
test2 = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]

num2 :: Int
num2 = head . compute2 0 . resetComp 12 2 $ input2

comp2b :: [Int] -> (Int, Int) -> ((Int, Int), Int)
comp2b xs (a, b) = ((a, b), head (compute2 0 (resetComp a b xs)))

num2b :: Int
num2b = let possible = (,) <$> [0..99] <*> [0..99] -- [(x, y) | x <- [0..99], y <- [0..99]]
            allMap = map (comp2b input2) possible
            ((n, v), _) = head . filter (\(_, a) -> a == 19690720)  $ allMap
        in (100 * n) + v

input2 :: [Int]
input2 = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,19,5,23,2,13,23,27,1,10,27,31,2,6,31,35,1,9,35,39,2,10,39,43,1,43,9,47,1,47,9,51,2,10,51,55,1,55,9,59,1,59,5,63,1,63,6,67,2,6,67,71,2,10,71,75,1,75,5,79,1,9,79,83,2,83,10,87,1,87,6,91,1,13,91,95,2,10,95,99,1,99,6,103,2,13,103,107,1,107,2,111,1,111,9,0,99,2,14,0,0]

-------------------


