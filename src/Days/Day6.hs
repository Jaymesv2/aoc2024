{-# LANGUAGE TupleSections #-}
module Days.Day6 (day6) where

import Solver

import Control.Arrow
-- import Data.Text (Text)
-- import Data.Either
-- import Data.Functor
import Data.Maybe
import Data.List (foldl', unfoldr, sort, inits, find)
import qualified Data.List.NonEmpty as NonEmpty
import Debug.Trace
import Data.Array.IArray
import Data.Array.ST qualified as STA
import Control.Monad.ST
import Data.Array.MArray

type Coordinate = (Int,Int)

data Direction = U | D | L | R deriving stock (Show, Eq, Ord)

-- instance Show Direction where
--     show U = "U"
--     show D = "D"
--     show L = "L"
--     show R = "R"

directionToCoord U = (-1,0)
directionToCoord D = (1,0)
directionToCoord R = (0,1)
directionToCoord L = (0,-1)

rotateRight U = R
rotateRight R = D
rotateRight D = L
rotateRight L = U

directionToInt U = 0
directionToInt D = 1
directionToInt L = 2
directionToInt R = 3

direction2ToCoords i = (if even i then (0,) else (,0)) (if i > 1 then -1 else 1)

{-
(x,y)
<-x-> 
*---* ^
|   | |
|   | y
*---*

-}



nub :: Ord a => [a] -> [a]
nub = fmap NonEmpty.head . NonEmpty.group . sort


addCoord :: Coordinate -> Coordinate -> Coordinate
addCoord (x1, y1) (x2, y2) = (x1+x2, y1+y2)


-- part1 :: (((Int,Int), [Coordinate]), (Coordinate, Direction)) -> Int
solution ((bounds@(boundX,boundY), obstacles), initState) = (part1, part2)
    where 
        obstaclesArray :: Array (Int, Int) Bool
        obstaclesArray = array ((-1,-1), bounds) ([((x,y), (x,y) `elem` obstacles) | x <- [-1..boundX], y <- [-1..boundY]])

        oob (x,y) = x >= boundX || y >= boundY || x < 0 || y < 0

        printGrid :: [Coordinate] -> String
        printGrid visited = 
            [  if y == boundY then '\n'
                else if (x,y) `elem` obstacles then '#'
                    else if (x,y) `elem` visited then 'X'
                        else '.'
                | x <- [0..boundX-1], y <- [0..boundY]]

        -- printGridx :: (Array (Int,Int) Bool) -> String
        -- printGridx visited = 
        --     [  if y == boundY then '\n'
        --         else if (x,y) `elem` obstacles then '#'
        --             else if visited ! (x,y)  then 'X'
        --                 else '.'
        --         | x <- [0..boundX-1], y <- [0..boundY]]


        printGrid2 :: [Coordinate] -> String
        printGrid2 visited = 
            [  if y == boundY then '\n'
                else if (x,y) `elem` obstacles then '#'
                    else if (x,y) `elem` visited then 'O'
                        else '.'
                | x <- [0..boundX-1], y <- [0..boundY]]

        part1 = length . nub . fmap fst $ states

        -- states = unfoldr (simStep bounds obstacles) initState
        states = unfoldr (simStep2 bounds obstaclesArray) initState
        
        -- -- I love O(n^2) solutions :)
        -- wouldNextLoop s@(loc, direction) = 
        --     (loc, rotateRight direction) `elem` states && -- the next point being an obstacle would create a loop
        --         nextCoord s `notElem` obstacles -- the next point isn't already an obstacle


        
        -- part2 = (\x -> trace (printGrid x) x) . fmap fst  $ res
        part2 = length . nub .  (\x -> trace (printGrid x) (trace (show x) x)) . fmap nextCoord  $ res
        res = filter (not . oob . fst) . filter wouldNextCauseLoop2 $ states

        wouldNextCauseLoop2 s = oob (nextCoord s) || oob (fst s) ||  hasDuplicateBefore2 bounds (unfoldr (simStep2 bounds obstacles') s)
            where
                obstacles' = obstaclesArray // [(nextCoord s, True)]
        

        -- -- wouldNextCauseLoop s@(loc,direction) = helper [] s{-isJust $ find ((loc, rotateRight direction)==) $ unfoldr (simStep bounds obstacles') s-}
        -- wouldNextCauseLoop s@(loc,direction) = {-helper [] s-}
        --     {-isJust $ find ((loc, rotateRight direction)==) -}
        --     hasDuplicateBefore $ unfoldr (simStep2 bounds obstacles') s
        --     where
        --         obstacles' = if oob (nextCoord s) then obstaclesArray else obstaclesArray // [(nextCoord s, True)]
        --             -- if oob (nextCoord s) then obstacles else nextCoord s:obstacles -- make the next point an obstacle
        --
        --
        --             -- prob need to check if its oob tho
        --         helper states' state = case simStep2 bounds obstacles' state of
        --             Just (prev, state') -> if prev `elem` states' 
        --                 then True
        --                 else helper (state':states') state'
        --             Nothing -> False
        --
        -- hasDuplicateBefore :: (Eq a) => [a] -> Bool
        -- hasDuplicateBefore = helper []
        --     where
        --         helper _ [] = False
        --         helper before (x:xs) = x `elem` before || helper (x:before) xs
        


hasDuplicateBefore2 :: (Int,Int) -> [(Coordinate, Direction)] ->  Bool
hasDuplicateBefore2 (boundX, boundY) ys = runST $ do
    a <- newArray ((-1,-1, 0), (boundX, boundY, 3)) False 
    helper a ys
    where
        helper :: STA.STUArray s (Int, Int, Int) Bool -> [(Coordinate, Direction)] -> ST s Bool
        helper _ [] = pure False
        helper a (((u,v), d):xs) = do
            e <- readArray a (u,v,directionToInt d)
            if e
                then pure True
                else writeArray a (u,v,directionToInt d) True >> helper a xs

-- lower than 1852
-- not 1798


simStep2 :: (Int,Int) -> Array (Int, Int) Bool -> (Coordinate, Direction) -> Maybe ( (Coordinate, Direction), (Coordinate, Direction))
simStep2 bounds@(boundX, boundY) obstacles s@(loc, direction)
    | oob loc = Nothing
    | obstacles ! next = simStep2 bounds obstacles (loc, rotateRight direction)
    | otherwise = Just ((loc, direction), (next, direction))
    where 
        next = nextCoord s
        oob (x,y) = x >= boundX || y >= boundY || x < 0 || y < 0



simStep :: (Int,Int) -> [Coordinate] -> (Coordinate, Direction) -> Maybe ( (Coordinate, Direction), (Coordinate, Direction))
simStep bounds@(boundX, boundY) obstacles s@(loc, direction)
    | oob loc = Nothing
    | next `elem` obstacles = simStep bounds obstacles (loc, rotateRight direction)
    | otherwise = Just ((loc, direction), (next, direction))
    where 
        next = nextCoord s
        oob (x,y) = x >= boundX || y >= boundY || x < 0 || y < 0

nextCoord (loc, direction) = addCoord loc (directionToCoord direction)


-- part1 :: (((Int,Int), [Coordinate]), (Coordinate, Direction)) -> Int
-- part1 ((bounds@(boundX,boundY), obstacles), state) = length . nub . unfoldr simStep $ state
--
--     where 
--         simStep :: (Coordinate, Direction) -> Maybe (Coordinate, (Coordinate, Direction))
--         simStep (loc, direction)
--             | oob loc = Nothing
--             | next `elem` obstacles = simStep (loc, rotateRight direction)
--             | otherwise = Just (loc, (next, direction))
--             where 
--                 next = addCoord loc $ directionToCoord direction
--                 oob (x,y) = x >= boundX || y >= boundY || x < 0 || y < 0

parse :: [String] -> (((Int,Int), [Coordinate]), (Coordinate, Direction))
parse ls = (((length ls, length (head ls)), obstacles), (guardPos, U))
    where
        obstacles = filterOnChar '#' coords
        guardPos = head $ filterOnChar '^' coords
        coords = indexMatrix ls

        filterOnChar c = fmap snd . filter ( (==c) . fst )
        indexMatrix :: [[a]] -> [(a,(Int, Int))]
        indexMatrix xs = [ (y, (ix, iy)) | (ys, ix) <- zip xs [0..], (y, iy) <- zip ys [0..] ]

        -- coords = concat . indexMatrix2 $ ls
        -- indexMatrix2 :: [[a]] -> [[(a,(Int, Int))]]
        -- indexMatrix2 xs = fmap (\(ys, idx) -> zip ys (fmap (idx,) [1..] )) (zip xs [1..])
        




day6 :: Solver
day6 = mkSolver 6 "Guard Gallivant" $ solution . parse . lines

