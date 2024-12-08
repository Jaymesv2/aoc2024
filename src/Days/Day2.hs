module Days.Day2 (day2) where

import Solver

import Control.Arrow
import Data.List (foldl')

day2 :: Solver
day2 = mkSolver 2 "Red-Nosed Reports" $ (part1 &&& part2) . fmap (fmap read . words) . lines

-- -- this function is bad :(
-- windows :: Int -> [a] -> [[a]]
-- windows n  = filter ((==n). length ) . inner
--     where 
--         inner lst = case lst of
--             [] -> []
--             _:xs -> take n lst : inner xs

pairs :: [a] -> [(a,a)]
pairs = zip <*> tail -- what the fuck

-- pairs (a:b:xs) = (a,b):pairs (b:xs)
-- pairs _ = []

-- what should it return for an empty list?
allEq :: Eq a => [a] -> Bool
allEq xs = fst $ foldl' (\(acc, prev) x -> (prev == x && acc, x)) (True, head xs) xs
-- allEq = isJust . listToMaybe . group



simplifyPair :: (Int, Int) -> Int
simplifyPair (a,b) = if abs (a-b) > 0 && abs (a-b) < 4
    then if a-b > 0 then 1 else -1
    else 0

isValidReport :: [Int] -> Bool
isValidReport  = allEq . fmap simplifyPair . pairs

part1 :: [[Int]] -> Int
part1 inp = length $ filter isValidReport inp


missingOne :: [a] -> [[a]]
missingOne [] = []
missingOne (x:xs) = xs:( (x:) <$> missingOne xs )


-- part2 :: [[Int]] -> Int
-- part2 inp = length $ filter (uncurry (||) . (any isValidReport . missingOne &&& isValidReport)) inp --fmap simplifyPair . pairs <$> inp
part2 inp = length . filter f $ inp
    where
        f report = isValidReport report || any  isValidReport (missingOne report)
-- part2 inp = tail $ init $ missingOne [1,2,3,4,5,6,7] 


-- part2 inp = length $ filter (\x -> any isValidReport $ (x:missingOne x) ) inp --fmap simplifyPair . pairs <$> inp


-- 665 too high
-- 646 too high
        
-- day2 = mkParsecSolver 2 "Cube Conundrum" () parseInput ((part1 &&& part2) )
-- 	where 
-- 		part1 = sum . map (\(x,_)-> x) . filter (\(_, hands) -> all (\(x,y,z) -> x <= 12 && y <= 13 && z <= 14) hands)
-- 		part2 = sum . map (\(x,y,z) -> x*y*z) . fmap (foldl' (\(a,b,c) (x,y,z)  -> (max a x, max b y, max c z)) (0,0,0) . snd)
--
-- parseInput :: Parser [(Int, [(Int, Int, Int)])]
-- parseInput = parseGame `sepEndBy1` string "\n"
-- 	where
-- 		parseGame :: Parser (Int, [(Int, Int, Int)])
-- 		parseGame = liftA2 (,) (string "Game " *> parseNum) (string ": " *> sepBy1 parseRound (string "; "))
-- 	
-- 		parseRound :: Parser (Int, Int, Int)
-- 		parseRound = foldl' (\(x,y,z) (a,b,c) -> (x+a, y+b, z+c)) (0,0,0) <$> parseBlock `sepBy1` string ", "
--
-- 		parseBlock ::  Parser  (Int, Int, Int)
-- 		parseBlock = (parseNum <* string " ") <**> choice [string "red" $> (\x -> (x,0,0)), string "green" $> (\x -> (0,x,0)), string "blue" $> (\x -> (0,0,x))]
--
-- 		parseNum :: Parser Int
-- 		parseNum = read <$> many1 (satisfy isDigit)
