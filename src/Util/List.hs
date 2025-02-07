module Util.List
    ( splitOn
    , groupsOf
    , windows
    , union
    , tupleWindows2
    , makeTableWithHeaders
    , fmtMatrix
    , fmtTable
    , groupOn
    , indexMatrix
    , combinations
    , combinationsN
    ) where

import Data.List (intercalate, transpose, groupBy, sortOn, tails)

combinations :: [a] -> [(a,a)]
combinations [] = []  
combinations (x:xs) = fmap (x,) xs ++ combinations xs

combinationsN :: Int -> [a] -> [[a]]
combinationsN 0 _ = [[]]
combinationsN n lst = do
    (x:xs) <- tails lst
    rest   <- combinationsN (n-1) xs
    return $ x : rest

indexMatrix :: [[a]] -> [((Int, Int),a)]
indexMatrix xs = [ ((ix, iy), y) | (ys, ix) <- zip xs [0..], (y, iy) <- zip ys [0..]]

splitOn :: (a->Bool) -> [a] -> [[a]]
splitOn on s = uncurry (:) $ f <$> break on s 
    where f x = if null x then [] else splitOn on $ tail x

groupsOf :: Int -> [a] -> [[a]]
groupsOf size lst = case splitAt size lst of
    (x, []) -> [x]
    (x, y) -> x:groupsOf size y

groupOn :: (Ord a) => [(a, b)] -> [(a, [b])]
groupOn = fmap (\xs -> (fst (head xs), fmap snd xs)) . groupBy (\(a,_) (b,_) -> a == b) . sortOn fst

tupleWindows2 :: [a] -> [(a,a)]
tupleWindows2 xs = zip xs $ drop 1 xs

windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows n xs = let window = take n xs
    in if length window < n then [] else window:windows n (tail xs)

union :: Eq a => [a] -> [a] -> [a]
union xs ys = [x | x <- xs, y <- ys, x == y]

makeTableWithHeaders :: [String] -> [[String]] -> String
makeTableWithHeaders headers columns = fmtTable $ zipWith (:) headers columns

fmtMatrix :: Show a => [[a]] -> String
fmtMatrix = fmtTable' (("", "", ""),("", "", ""),("", "", ""),("", " ","")) ' ' . fmap (fmap (show))
fmtTable :: [[String]] -> String
fmtTable  = fmtTable' (("┌", "┬", "┐"), ("├", "┼", "┤"), ("└", "┴", "┘"), ("│","│","│")) '─'

type SepGroup = (String, String, String)

fmtTable' :: (SepGroup, SepGroup, SepGroup, SepGroup) -> Char -> [[String]] -> String
fmtTable' (top, mid, bottom, col) c columns = genSep top ++ intercalate (genSep mid) rows ++ genSep bottom
    where columnWidths = fmap (maximum . fmap length) columns
          rows = genSep' (uncurry padTo) col columnWidths <$> transpose columns

          padTo :: Int -> String -> String
          padTo len s = uncurry (++) $ (s++) <$> splitAt (n `quot` 2) (replicate n ' ') where n = len - length s

          genSep' :: ((Int, String) -> String) -> (String, String, String) -> [Int] -> [String] -> String
          genSep' f (beg, sep, end) widths row = beg ++ (intercalate sep $ fmap f $ (zip widths row)) ++ end ++ "\n"

          genSep :: (String, String, String) -> String
          genSep s = genSep' (flip replicate c . fst) s columnWidths (repeat [])
