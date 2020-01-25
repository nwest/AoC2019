{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')


input1 :: IO [Int]
input1 = map read . lines <$> readFile "./1"

fuel :: Int -> Int
fuel = (2 `subtract`) . (`quot` 3)

allFuel :: Int -> Int
allFuel (fuel -> f) | f > 0 = f + allFuel f
                    | otherwise = f

num1 :: IO Int
num1 = sum . map fuel <$> input1

num1b :: IO Int
num1b = sum . map allFuel <$> input1

-------------------

cleanCSV :: String -> [Int]
cleanCSV xs = read . concat $ [ "[", xs, "]" ]

input2 :: IO [Int]
input2 =  cleanCSV . head . lines <$> readFile "./2"

crunch2 :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int] --maybe use Vector
crunch2 f [a, b, c] xs = let g x = head . drop x $ xs
                         in take c xs ++ f (g a) (g b) : drop (succ c) xs
crunch2 _     _     xs = xs

compute2 :: Int -> [Int] -> [Int]
compute2 o xs | a == 1  = f (+)
              | a == 2  = f (*)
              | a == 99 = xs
              | otherwise = xs
        where a:inst = take 4 . drop (4 * o) $ xs
              f g = compute2 (succ o) . crunch2 g inst $ xs

resetComp :: Int -> Int -> [Int] -> [Int]
resetComp a b (x:_:_:xs) = x : a : b : xs
resetComp _ _ _ = []

test2 :: [Int]
test2 = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]

num2 :: IO Int
num2 = head . compute2 0 . resetComp 12 2 <$> input2

comp2b :: [Int] -> (Int, Int) -> ((Int, Int), Int)
comp2b xs (a, b) = ((a, b), head . compute2 0 . resetComp a b $ xs)

num2b :: IO Int
num2b = do let possible = (,) <$> [0..99] <*> [0..99] -- [(x, y) | x <- [0..99], y <- [0..99]]
           input2' <- input2
           let allMap = map (comp2b input2') possible
               ((n, v), _) = head . filter (\(_, a) -> a == 19690720)  $ allMap
           return $ (100 * n) + v

-------------------

input3 :: IO [[WireRoute]]
input3 =  map (map instr . splitOn ",") . lines <$> readFile "./3"

splitOn :: String -> String -> [String]
splitOn c = map T.unpack . T.splitOn (T.pack c) . T.pack

type WireRoute = (Char, Int)
type WireCoords = Set (Int, Int)

instr :: String -> WireRoute
instr (x:xs) = (x, read xs)
instr []     = ('X', 0)

route :: (Int, Int) -> WireRoute -> [(Int, Int)]
route start (dir, moves) | dir == 'U' = f up
                         | dir == 'D' = f down
                         | dir == 'L' = f left
                         | dir == 'R' = f right
                         | otherwise = []
                         where f g = start : reverse (g moves start)
                               up    n (a, b) = foldl' (\xs x -> (a, b + x): xs) [] [1..n]
                               down  n (a, b) = foldl' (\xs x -> (a, b - x): xs) [] [1..n]
                               left  n (a, b) = foldl' (\xs x -> (a - x, b): xs) [] [1..n]
                               right n (a, b) = foldl' (\xs x -> (a + x, b): xs) [] [1..n]

wireCoords :: (Int, Int) -> [WireRoute] -> WireCoords
wireCoords c = Set.fromList . foldl' (\xs x -> xs ++ route (last xs) x) [c]

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a, b) (c, d) = abs(a - c) + abs(b - d)

num3 :: IO Int
num3 = do
       wireA <- wireCoords (0,0) . head <$> input3
       wireB <- wireCoords (0,0) . last <$> input3
       let intersections = Set.delete (0,0) (Set.intersection wireA wireB)
           distances = Set.map (manhattan (0,0)) intersections
       return $ Set.findMin distances
