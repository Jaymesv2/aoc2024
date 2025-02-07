{-# LANGUAGE TransformListComp, BangPatterns #-}
module Days.Day12 (day12) where

import Solver

import Control.Arrow
-- import Data.Function.Memoize
import Data.Array.IArray as IA
import Util.List (indexMatrix, groupOn)
import Data.List (groupBy, nub, sortOn)
import Data.Array.ST qualified as STA
import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
-- import Data.Tuple
import Data.Maybe
import Data.Char
import Data.Tuple
import Debug.Trace
import Data.Function

-- import Data.Array.Unboxed as UA


data Direction = U | R | D | L deriving stock (Show, Eq, Ord, Enum)

rotateRight :: Direction -> Direction
rotateRight = toEnum . (`mod` 4) . (+1) . fromEnum

directionToCoord :: Direction -> (Int,Int)
directionToCoord U = (-1,0)
directionToCoord D = (1,0)
directionToCoord R = (0,1)
directionToCoord L = (0,-1)

addCoord :: Coordinate -> Coordinate -> Coordinate
addCoord (x1, y1) (x2, y2) = (x1+x2, y1+y2)

nextCoord :: (Coordinate, Direction) -> Coordinate
nextCoord (loc, direction) = addCoord loc (directionToCoord direction)


dsRead :: (MArray a (i,Int) m, Ix i) => a i (i,Int) -> i  -> m (i,Int)
dsRead a i = do
    iv@(iParent, _) <- readArray a i
    if iParent == i
        then pure iv
        else do
            parentPtr <- dsRead a iParent
            writeArray a i parentPtr 
            pure parentPtr

dsUnion :: (MArray a (i, Int) m, Ix i) => a i (i,Int) -> i -> i -> m ()
dsUnion a i j = do
    (iParent, iSize) <- dsRead a i
    (jParent, jSize) <- dsRead a j
    if iParent == jParent 
        then pure ()
        else if iSize < jSize 
            then do 
                writeArray a iParent (jParent, iSize)
                writeArray a jParent (jParent, iSize + jSize)
            else do 
                writeArray a jParent (iParent, jSize)
                writeArray a iParent (iParent, iSize + jSize)

dsDoUnions :: Array Coordinate Char -> [(Coordinate, [Coordinate])]
dsDoUnions a = runST $ do
    s <- (dsNew (bounds a) :: ST s (STA.STArray s Coordinate (Coordinate,Int)))
    forM_ (assocs a) $ \(coord, currentVal) -> do
        forM_ (neighbors a coord) $ \(neighborCoord, neighborVal) -> do
            when (neighborVal == currentVal) $
                dsUnion s coord neighborCoord

    -- sequence_ 
    --         [ dsUnion s coord neighborCoord
    --         | coord <- indices a,
    --         let currentVal = a ! coord
    --         , (neighborCoord, neighborVal) <- neighbors a coord 
    --         , neighborVal == currentVal
    --         ]
    dsGroups s

--(MArray a (i, Int) m, Ix i) => a i (i,Int) -> i -> i -> m ()

        


dsNew :: (Ix i, MArray a (i,Int) m) =>  (i,i) -> m (a i (i,Int))
dsNew bnds = newGenArray bnds (pure . (,1) )

-- dsGroupIds :: (Ix i, MArray a (i,Int) m) => a i (i, Int) -> m [i]
-- dsGroupIds = fmap ( fmap fst . filter (uncurry (==)) . fmap (fmap fst) ) . getAssocs


-- O(n log n)
dsGroups :: (Ix i, MArray a (i,Int) m) => a i (i, Int) -> m [(i,[i])]
dsGroups a = do 
    ixs <- fmap fst <$> getAssocs a
    groupOn <$> mapM (\i -> (, i) . fst <$> dsRead a i) ixs



showGroups :: (Int,Int) -> [(Coordinate, [Coordinate])] -> String
showGroups (boundX,boundY) grps = [if j == boundY+1 then '\n' else intToDigit ( fromJust $ lookup (i,j) assocs' ) | i <- [0..boundX], j <- [0..boundY+1]]
    where
        assocs' = [(j,n) | (n,(_,xs)) <- zip [0..] grps, j <- xs]


type Coordinate = (Int,Int)


solution :: Array Coordinate Char -> (Int, String )
solution coords = (part1, part2)
    where
        unions :: [(Coordinate, [Coordinate])]
        unions = dsDoUnions coords

        groups :: [[Coordinate]]
        groups = snd <$> unions
        
        part1 = sum $ fmap (\grp -> perimeterOfGroup grp * areaOfGroup grp) groups

        perimeter :: Array Coordinate Int
        perimeter = array (bounds coords) 
            [(coord, 
                4 - validNeighbors coords (coord, val)) -- makes less garbage
                | (coord, val) <- assocs coords ]

        perimeterOfGroup :: [Coordinate] -> Int
        perimeterOfGroup = sum . fmap (perimeter ! ) 

        areaOfGroup :: [Coordinate] -> Int
        areaOfGroup = length

        part2 = 
            show $ (\g -> let 
            c = coords ! head g
            in (c, wallsInGroup g)) <$> groups

        
        -- -- is a wall with a on the left.
        -- isWall :: Char -> Coordinate -> Coordinate -> Bool
        -- isWall ch a b = coords !? a == Just ch && coords !? b /= Just ch

        (_,(boundX,boundY)) = bounds coords


        {- rightWalls,leftWalls,upWalls,downWalls :: [[(Coordinate, Maybe Char)]]
        rightWalls = 
            [
                [ ((x,y), coords !? (x,y+1))
                | x <- [0..boundX]
                ]
            | y <- [0..boundY]
            ]

        leftWalls =
            [   [ ((x,y), coords !? (x,y-1))
                | x <- [0..boundX]
                ]
            | y <- [0..boundY]
            ]

        upWalls = 
            [ [ ((x,y), coords !? (x-1,y))
                | y <- [0..boundY]
                ]
            | x <- [0..boundX]
            ]

        downWalls = 
            [
                [ ((x,y), coords !? (x+1,y))
                | y <- [0..boundY]
                ]
            | x <- [0..boundX]
            ]
        allWalls = leftWalls ++ rightWalls ++ upWalls ++ downWalls


        isWallBetween :: Char -> (Coordinate, Maybe Char) -> Bool
        isWallBetween c (c1, x) = c == ( coords ! c1 ) && x /= Just c -}

        -- countWallsOf :: Char -> [(Coordinate, Maybe Char)]-> Int
        -- countWallsOf c = 
        --     length .
        --     filter id .
        --     nub .
        --     fmap (isWallBetween c)


        -- wallsInGroup :: [Coordinate] -> Int
        wallsInGroup group = lWallsCnt + rWallsCnt + uWallsCnt + dWallsCnt
            where
                -- newWalls = filter (\(c,_) -> coords ! c /= ch || c `elem` group) <$> allWalls
                ch = coords ! head group

                rightWalls = wallsAt (\(x,y) -> (x,y+1))
                leftWalls  = wallsAt (\(x,y) -> (x,y-1))
                upWalls    = wallsAt (\(x,y) -> (x-1,y))
                downWalls  = wallsAt (\(x,y) -> (x+1,y))

                -- tracer x = trace (ch:": " ++ show x) x

                lWalls = {- tracer $ -} mergeNeighbors ((==) `on` snd) $ sortOn id leftWalls
                rWalls = {- tracer $ -} mergeNeighbors ((==) `on` snd) $ sortOn id rightWalls
                uWalls = {- tracer $ -} mergeNeighbors ((==) `on` fst) $ sortOn swap upWalls
                dWalls = {- tracer $ -} mergeNeighbors ((==) `on` fst) $ sortOn swap downWalls

                lWallsCnt = length lWalls
                rWallsCnt = length rWalls
                uWallsCnt = length uWalls
                dWallsCnt = length dWalls

                wallsAt :: (Coordinate -> Coordinate) -> [Coordinate]
                wallsAt f = 
                    [coord
                    | coord <- group
                    , let chAtNeighbor = coords !? f coord
                    , chAtNeighbor /= Just ch
                    ]
                
                mergeNeighbors :: (Coordinate -> Coordinate -> Bool) -> [Coordinate] -> [Coordinate]
                mergeNeighbors f (c1:cy@(c2:_)) | f c1 c2 = mergeNeighbors f cy
                mergeNeighbors f (cx:cy@((_,_):_)) =  cx:mergeNeighbors f cy
                mergeNeighbors _ x = x
                


        -- isWallBetween c (c1, x) = c == ( coords ! c1 ) && x /= Just c

                -- right = ((x,y), coords !? (x,y+1))
                -- left = ((x,y), coords !? (x,y-1))
                -- upWalls = ((x,y), coords !? (x-1,y))
                -- downWalls = ((x,y), coords !? (x+1,y))


        -- countEdges :: [Coordinate, Direction] -> Int
        -- countEdges g = error ""
        --     where
        --         byDirection = groupBy \(_,a (_,b) -> a==b) g
        --
        --
        -- edgesOfGroup :: [Coordinate] -> [(Coordinate, Direction)]
        -- edgesOfGroup = 
        --     concatMap (\coord@(i,j) -> [neighbor
        --         | let currentVal = coords ! coord
        --         , neighbor@(newCoord, _) <- [((i+1,j), U), ((i-1,j), D), ( (i,j+1), R ), ( (i,j-1), L )]
        --         , case coords !? newCoord of
        --             Just n' | n' == currentVal -> False
        --             _ -> True
        --         ])

        {-

        c|n
          -
        y u

        c|n
        
        c|u

        -}

        {- walkEdges :: (Coordinate, Direction) -> (Coordinate, Direction) -> [(Coordinate, Direction)]
        walkEdges startPoint currentEdge@(coord, dir) 
            -- if the next step would return to the start
            -- | next == startPoint = []
            

            -- if the neighborVal is not equal to the current it is a wall
            | neighborVal /= Just currentVal = currentEdge:walkEdges startPoint nextEdge

            | otherwise = error ""
            
            where
                rightDir = rotateRight dir
                currentVal = coords ! coord
                cNeighbor = nextCoord currentEdge
                neighborVal = coords !? cNeighbor

                cNext = nextCoord (coord, rightDir)
                nextVal = coords !? cNext 
                nextEdge = if nextVal == Just currentVal 
                    then  
                        error ""
                    else  
                        error "" -}




        

{- foldValidNeighbors :: Eq a => Array Coordinate a -> Coordinate -> (b -> a -> b) -> b -> b
foldValidNeighbors coords (i,j) f = foldIf (i+1,j) . foldIf (i-1,j) . foldIf (i,j+1) . foldIf (i,j-1)
    -- if coords !? (i+1,j) == Maybe 
    where
        elemHere = coords ! (i,j)
        -- foldIf :: Coordinate -> b -> b
        foldIf coord x = case coords !? coord of
            Nothing -> x
            Just s -> if elemHere == s then f x s else x

foldValidNeighborsIdx :: Eq a => Array Coordinate a -> Coordinate -> (b -> (Coordinate, a) -> b) -> b -> b
foldValidNeighborsIdx coords (i,j) f = foldIf (i+1,j) . foldIf (i-1,j) . foldIf (i,j+1) . foldIf (i,j-1)
    -- if coords !? (i+1,j) == Maybe 
    where
        elemHere = coords ! (i,j)
        -- foldIf :: Coordinate -> b -> b
        foldIf coord x = case coords !? coord of
            Nothing -> x
            Just s -> if elemHere == s then f x (coord, s) else x -}
        

validNeighbors :: Eq a =>  Array Coordinate a -> (Coordinate, a) -> Int
validNeighbors coords ((i,j), a) 
    = (fromEnum . isJust $ (coords !? (i+1,j) >>= (\s -> guard (s == a) )))
    + (fromEnum . isJust $ (coords !? (i-1,j) >>= (\s -> guard (s == a) )))
    + (fromEnum . isJust $ (coords !? (i,j+1) >>= (\s -> guard (s == a) )))
    + (fromEnum . isJust $ (coords !? (i,j-1) >>= (\s -> guard (s == a) )))


-- this functions make SO MUCH garbage :(
neighbors :: Array Coordinate a -> Coordinate -> [(Coordinate,a)]
neighbors coords (i,j) = 
    [(newCoord, n') | newCoord <-  [(i+1,j), (i-1,j), (i,j+1), (i,j-1)] , Just n' <- [coords !? newCoord] ]


day12 :: Solver
day12 = mkSolver 12 "Garden Groups" $ solution . parse . lines

parse :: [String] -> Array (Int,Int) Char
parse ls = array ((0,0), (length ls-1, length (head ls)-1)) $ {- fmap digitToInt <$> -} indexMatrix ls
