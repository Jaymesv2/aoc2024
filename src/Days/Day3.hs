{-# LANGUAGE TupleSections #-}
module Days.Day3 (day3) where

import Solver

import Control.Arrow
import Data.Text (Text)
import Data.Either
import Data.Functor
import Data.Maybe
import Data.List (foldl')

import Text.Parsec


parseInput :: Parsec Text () [ Either Bool (Int, Int) ]
parseInput = catMaybes <$> many (choice [
                try (string "do()") $> Just (Left True),
                try (string "don't()") $> Just (Left False),
                -- Just . Right <$> try ((,) <$> (string "mul(" *> (read <$> many1 digit)) <*> (char ',' *> (read <$> many1 digit) <* char ')')),
                Just . Right <$> try parseMul,
                anyToken $> Nothing
                ])
    where 
        parseMul :: Parsec Text () (Int, Int)
        parseMul = do
            _ <- string "mul("
            a <- read <$> many1 digit
            _ <- char ','
            b <- read <$> many1 digit
            _ <- char ')'
            pure (a,b)


part1 :: [Either Bool (Int, Int) ] -> Int
part1 = sum . fmap (uncurry (*)) . rights

part2 :: [Either Bool (Int, Int) ] -> Int
part2 = snd . foldl' (\(c,s) -> \case 
    Left cn -> (cn,s)
    Right (a,b) -> (c, if c then s + a*b else s)
    ) (True, 0)

day3 :: Solver
day3 = mkParsecSolver 3 "Mull It Over" () parseInput (part1 &&& part2)

