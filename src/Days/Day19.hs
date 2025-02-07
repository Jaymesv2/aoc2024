{-# LANGUAGE ScopedTypeVariables #-}
module Days.Day19 (day19) where

import Solver
import Text.Parsec
import Data.Text (Text)
import Data.Function.Memoize


day19 :: Solver
day19 = mkParsecSolver 19 "Linen Layout" () parseInput solution 

solution :: ([String], [String]) -> (Int,Int)
solution (patterns, towels) = (p1, p2)
    where
        p1 = length . filter (/=0) $ b
        p2 = sum b
        b = fmap (solutionCount patterns) towels

solutionCount :: [String] -> String -> Int
solutionCount patterns = memoFix (\canBuild' s -> 
    sum [if null r then 1 else canBuild' r 
        | p <- patterns
        , Just r <- [prefixStrip p s] ])

prefixStrip :: Eq a => [a] -> [a] -> Maybe [a]
prefixStrip (p:ps) (x:xs) | p == x = prefixStrip ps xs
prefixStrip [] xs = Just xs
prefixStrip _ _ = Nothing


parseInput :: Parsec Text () ([String], [String])
parseInput = do
    ps <- pat `sepBy1` string ", "
    _ <- string "\n\n"
    pats <- pat `sepEndBy1` char '\n'
    pure (ps, pats)
    where
        pat = many1 (satisfy (\c -> c == 'w' || c == 'u' || c == 'b' || c == 'r' || c == 'g'))
