module Days.Day09 (day9) where

import Solver

import Control.Arrow

import Data.Functor
import Data.List as L
import Data.Char (digitToInt, isDigit)
-- import Data.List (unsnoc)
import Data.Sequence as S
import Control.Monad
import Data.Maybe

day9 :: Solver
day9 = mkSolver 9 "Disk Fragmenter" $ (part1 &&& part2) . buildBlocks . L.filter isDigit

part1 :: [(Int, Int, Maybe Int)] -> Int
part1 s = sum . L.zipWith (*) [0..] $ compact  $ (\(_,a,b)-> (a,b)) <$> S.fromList s

part2 :: [(Int, Int, Maybe Int)] -> Int
part2 l = checksum2 $ compact2 l

        
compact :: Seq (Int, Maybe Int) -> [Int]
compact ((0,_) :<| xs) = compact xs
compact ((i, Nothing) :<| (xs :|> (0,_))) = compact ((i,Nothing) :<| xs)
compact ((i, Just x) :<| xs) = x:compact ((i-1, Just x) :<| xs)
compact ((i, Nothing) :<| (xs :|> (j,Nothing))) = compact ((i,Nothing) :<| (xs :|> (j-1, Nothing)))
compact ((i, Nothing) :<| (xs :|> (j, Just x))) = x:compact ((i-1,Nothing) :<| (xs :|> (j-1, Just x)))
compact ((_,Nothing) :<| Empty) = []
compact Empty = []

checksum2 :: [(Int,Int,Int)] -> Int
checksum2 ((pos,len,bid):xs) = sum (fmap(*bid) [pos..pos+len-1]) + checksum2 xs
checksum2 [] = 0

compact2 :: [(Int, Int, Maybe Int)] -> [(Int, Int, Int)]
compact2 s = L.sortOn (\(x,_,_) -> x) res
    where
        blocks :: [(Int, Int, Int)] -- pos, length, id
        baseFreeLst :: [(Int, Int)] -- pos, length
        (blocks, baseFreeLst) = (fmap (\(x,y,z) -> (x,y, fromJust z)) *** fmap (\(x,y,_) -> (x,y))) $ L.partition (isJust . (\(_,_,x)->x)) s

        moveBlock :: [(Int,Int)] -> (Int,Int,Int) -> ( [(Int,Int)], (Int,Int,Int) )
        moveBlock fl@(flb@(fbPos, fbLen) : fl') b@(pos,len,blockId) 
            | pos < fbPos = (fl, b) -- return the free list
            | len < fbLen = ((fbPos+len, fbLen-len):fl', (fbPos, len, blockId))
            | len == fbLen = (fl', (fbPos, len, blockId))
            | otherwise =  first (flb:) $ moveBlock fl' b
        moveBlock [] b = ([],b)

        res = snd $ mapAccumR moveBlock baseFreeLst blocks

buildBlocks :: String -> [(Int, Int, Maybe Int)]
buildBlocks = helper 0 0 True
    where 
        helper :: Int -> Int -> Bool -> String -> [(Int, Int, Maybe Int)]
        helper _ _ _ [] = [] -- base case
        helper idx pos b (x:xs) = (pos, digitToInt x, guard b $> idx):helper (idx+fromEnum b) (pos + digitToInt x) (not b) xs


-- printRanges :: [(Int,Int, Int)] -> String
-- printRanges = helper 0 
--     where
--         helper :: Int -> [(Int,Int,Int)] -> String
--         helper _ [] = []
--         helper idx d@((pos, _,_):xs) | idx < pos  = L.replicate (pos-idx) '.' ++ helper pos d
--         -- helper idx ((pos, 0, bid):xs) = helper idx xs
--         helper _ ((pos, len, bid):xs) = L.replicate len (intToDigit bid) ++ helper (pos+len) xs
