module Days.Day02 (day2) where

import Solver

import Data.List (partition)

day2 :: Solver
day2 = mkSolver 2 "Red-Nosed Reports" $ solution . fmap (fmap read . words) . lines

allUp :: [Int] -> Bool
allUp (x:y:xs) = ((y-x)>0 && (y-x)<4) && allUp (y:xs)
allUp _ = True

allDown :: [Int] -> Bool
allDown (x:y:xs) = ((x-y)>0 && (x-y)<4) && allDown (y:xs)
allDown _ = True

isValidReport :: [Int] -> Bool
-- isValidReport  = allEq . fmap simplifyPair . windows2
isValidReport  l = allUp l || allDown l 


solution :: [[Int]] -> (Int, Int)
solution inp = (part1, part2)
    where
        part1 = length valid
        part2 = part1 + length ( filter f invalid )

        (valid, invalid) = partition isValidReport inp

        f report = any isValidReport (missingOne report)


missingOne :: [a] -> [[a]]
missingOne [] = []
missingOne (x:xs) = xs:( (x:) <$> missingOne xs )


{- windows2 :: [a] -> [(a,a)]
windows2 = zip <*> tail -- what the fuck

allEq :: Eq a => [a] -> Bool
-- allEq xs = fst $ foldl' (\(acc, prev) x -> (prev == x && acc, x)) (True, head xs) xs
allEq (x:y:ys) = x == y && allEq (y:ys)
allEq _ = True

simplifyPair :: (Int, Int) -> Int
simplifyPair (a,b) = if abs (a-b) > 0 && abs (a-b) < 4 then clamp (-1,1) (a-b) else 0 -}


-- isValidReport2 :: Bool -> [Int] -> Bool
-- isValidReport2 b report = allUp b report || allDown b report
