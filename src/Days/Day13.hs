module Days.Day13 (day13) where

import Solver

import Control.Arrow
import Data.Text (Text)

import Text.Parsec
import Data.Maybe

type Int' = Int
-- solve le linear system
solveEqn :: (Int',Int',Int',Int',Int',Int') -> Maybe (Int',Int')
solveEqn (ax, ay, bx, by, px, py) = if ar == 0 && br == 0 then Just (aq,bq) else Nothing
    where
        det = ax*by-ay*bx
        da = by*px - bx*py
        db = -ay*px+ax*py
        (aq, ar) = da `divMod` det
        (bq, br) = db `divMod` det


part1,part2 :: [ (Int',Int',Int',Int',Int',Int') ] -> Int'
part1 = sum . fmap (uncurry (+) . first (*3)) . filter (\(x,y) -> x <= 100 && y <= 100) . mapMaybe solveEqn 

part2 = sum .
     fmap ( uncurry (+) . first (*3) ) 
    . mapMaybe ( solveEqn 
    . (\(a,b,c,d,e,f) -> (a,b,c,d,e+10000000000000, f+10000000000000))) 


day13 :: Solver
day13 = mkParsecSolver 13 "Claw Contraption" () parseInput (part1 &&& part2)


parseInput :: Parsec Text () [ (Int',Int',Int',Int',Int',Int') ]
parseInput = do
    flip sepBy1 (try $ char '\n' >> char '\n') $ do
        _ <- string "Button A: X+" 
        ax <- number
        _ <- string ", Y+"
        ay <- number
        _ <- string "\nButton B: X+"
        bx <- number
        _ <- string ", Y+"
        by <- number
        _ <- string "\nPrize: X="
        px <- number
        _ <- string ", Y="
        py <- number
        pure (ax,ay,bx,by,px,py)
    where 
        number = read <$> many1 digit

