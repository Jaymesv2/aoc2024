module Days.Day06 (day6) where

import Solver

import Data.List (unfoldr, sort)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Array.IArray as IA
import Data.Array.ST qualified as STA
import Data.Array.Unboxed as UA


import Control.Monad.ST
import Data.Array.MArray
import Util.List


type Coordinate = (Int,Int)

data Direction = U | R | D | L deriving stock (Show, Eq, Ord, Enum)

rotateRight :: Direction -> Direction
rotateRight = toEnum . (`mod` 4) . (+1) . fromEnum

directionToCoord :: Direction -> (Int,Int)
directionToCoord U = (-1,0)
directionToCoord D = (1,0)
directionToCoord R = (0,1)
directionToCoord L = (0,-1)



nub :: Ord a => [a] -> [a]
nub = fmap NonEmpty.head . NonEmpty.group . sort

addCoord :: Coordinate -> Coordinate -> Coordinate
addCoord (x1, y1) (x2, y2) = (x1+x2, y1+y2)

solution :: (UArray (Int,Int) Bool  , (Coordinate, Direction)) -> (Int, Int)
solution (obstaclesArray, initState) = (part1, part2)
    where 
        states = unfoldr (simStep obstaclesArray Nothing) initState

        part1 = length . nub . fmap fst $ states
        part2 = length . nub . filter (/=fst initState) $ fst <$> filter wouldObstacleCauseLoop states

        wouldObstacleCauseLoop :: (Coordinate, Direction) -> Bool
        wouldObstacleCauseLoop s = 
            obstaclesArray !? fst s == Just False -- next is not an obstacle and is in bounds
            && hasDuplicateBounded 
                (snd $ IA.bounds obstaclesArray) 
                -- (unfoldr (simStep $ obstaclesArray // [(fst s, True)]) initState) -- returns to a previous point and direction while walking
                (unfoldr (simStep obstaclesArray (Just $ fst s) ) initState) -- returns to a previous point and direction while walking


hasDuplicateBounded :: (Int,Int) -> [(Coordinate, Direction)] ->  Bool
hasDuplicateBounded (boundX, boundY) ys = runST $ do
    a <- newArray ((0,0,0), (boundX, boundY, 3)) False 
    helper a ys
    where
        helper :: STA.STUArray s (Int, Int, Int) Bool -> [(Coordinate, Direction)] -> ST s Bool
        helper _ [] = pure False -- no duplicate in empty array
        helper a (((u,v), d):xs) = do
            e <- readArray a (u,v,fromEnum d) -- read the current location
            if e
                then pure True -- if the current location has been visited
                else writeArray a (u,v,fromEnum d) True >> helper a xs -- if the current location hasn't been visited


simStep ::  UArray (Int, Int) Bool -> Maybe Coordinate -> (Coordinate, Direction) -> Maybe ( (Coordinate, Direction), (Coordinate, Direction))
simStep  obstacles extraob s@(loc, direction)
    | oob loc = Nothing
    | Just next == extraob || obstacles !? next == Just True = Just ((loc, direction), (loc, rotateRight direction))
    | otherwise = Just ((loc, direction), (next, direction))
    where 
        (_,(boundX,boundY)) = IA.bounds obstacles
        next = nextCoord s
        oob (x,y) = x > boundX || y > boundY || x < 0 || y < 0

nextCoord :: (Coordinate, Direction) -> Coordinate
nextCoord (loc, direction) = addCoord loc (directionToCoord direction)

parse :: [String] -> (UArray (Int,Int) Bool, (Coordinate, Direction))
parse ls = (obstacles, (head $ filterOnChar '^' coords , U))
    where
        obstacles = array ((0,0), (length ls-1, length (head ls)-1)) $ fmap (=='#') <$> coords
        filterOnChar c = fmap fst . filter ( (==c) . snd )
        coords :: [((Int,Int), Char)]
        coords = indexMatrix ls



day6 :: Solver
day6 = mkSolver 6 "Guard Gallivant" $ solution . parse . lines

