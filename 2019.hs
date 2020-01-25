{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl', group, sort)


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
                         where f g = reverse (g moves start)
                               move h n = foldl' h [] [1..n]
                               up    n (a, b) = move (\xs x -> (a, b + x) : xs) n
                               down  n (a, b) = move (\xs x -> (a, b - x) : xs) n
                               left  n (a, b) = move (\xs x -> (a - x, b) : xs) n
                               right n (a, b) = move (\xs x -> (a + x, b) : xs) n

wireCoords :: (Int, Int) -> [WireRoute] -> [(Int, Int)]
wireCoords c = foldl' (\xs x -> xs ++ route (last xs) x) [c]

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a, b) (c, d) = abs(a - c) + abs(b - d)

distTo :: Eq a => a -> [a] -> Int
distTo _ [] = 0
distTo a (x:xs) | a == x    = 0
                | otherwise = 1 + distTo a xs

num3 :: IO Int
num3 = do
       wireA <- Set.fromList . wireCoords (0,0) . head <$> input3
       wireB <- Set.fromList . wireCoords (0,0) . last <$> input3
       let intersections = Set.delete (0,0) (Set.intersection wireA wireB)
           distances = Set.map (manhattan (0,0)) intersections
       return $ Set.findMin distances

num3b :: IO Int
num3b = do
        wireA <- wireCoords (0,0) . head <$> input3
        wireB <- wireCoords (0,0) . last <$> input3
        let intersections = Set.delete (0,0) $ Set.intersection (Set.fromList wireA) (Set.fromList wireB)
            lengths = Set.map (\coord -> distTo coord wireA + distTo coord wireB) intersections
        return $ Set.findMin lengths
            
-------------------

input4 :: [Int]
input4 = [246515..739105]

increasing :: Int -> Bool
increasing x = x == (read . sort . show $ x)

firstSort4 :: [Int]
firstSort4 = filter increasing . filter (\n -> (length . group . show $ n) < 6) $ input4

num4 :: Int
num4 = length firstSort4

num4b :: Int
num4b = length . filter (\x -> 2 `elem` (map length . group . show $ x) ) $ firstSort4

-------------------
