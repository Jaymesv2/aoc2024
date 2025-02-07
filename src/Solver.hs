{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, OverloadedLists, UndecidableInstances #-}
module Solver 
    ( Solution
    , Solver
    , mkSolver
    , runSolver
    , genTable
    , solutionNum
    , mkParsecSolver
    ) where

import Text.Printf
import Util.List
import Data.List (unzip4)
import Data.Text
import Data.Text.IO qualified as TIO
import Text.Parsec (Parsec, runParser)


data AnswerClass = Number | Visual | Dynamic

-- data TestCase = forall a b. TestCase String a b

data Unimplemented = Unimplemented

-- class Solution   where
--     answClass :: AnswerClass


-- instance Solution Unimplemented 
    

data TestCase = TestCase String 




-- data Solver = forall a b. (Solution a, Solution b) => Solver Int String [TestCase] (Text -> a, Text -> b)
data Solver = forall a b. Solver Int String [TestCase] (Text -> Solution)

data Solution = forall a b. (Show a, Show b) => Solution Int String a b


-- mkParsecSolver3 :: (Show a, Show b) => Int -> String -> [TestCase] -> Parsec Text u c -> u -> (c -> a) -> (c -> b) -> Solver
-- mkParsecSolver3 day name parser state fa fb = Solver day name [] (\txt ->  Solution day name (fa (parse txt)) (fb (parse txt)) )
--     where
--         parse = runPar parser name state



mkParsecSolver :: (Show a, Show b) => Int -> String -> u -> Parsec Text u c -> (c -> (a,b)) -> Solver
mkParsecSolver day name state parser f = Solver day name [] (uncurry (Solution day name) . f . runPar parser name state)

mkParsecSolver2 :: (Show a, Show b) => Int -> String -> u -> Parsec Text u c -> (c -> a) -> (c -> b) -> Solver
mkParsecSolver2 day name state parser fa fb = Solver day name [] (\txt ->  Solution day name (fa (parse txt)) (fb (parse txt)) )
    where
        parse = runPar parser name state
        
    


mkSolver :: (Show a, Show b) => Int -> String -> (String -> (a,b)) -> Solver
mkSolver d n s = Solver d n [] (uncurry (Solution  d n ) . s . unpack)

mkSolver2 :: (Show a, Show b) => Int -> String -> (String -> a) -> (String -> b) -> Solver
mkSolver2 d n fa fb = Solver d n [] (\inp -> Solution  d n (fa . unpack $ inp) (fb . unpack $ inp) )
    


runPar :: Parsec Text u a -> String -> u -> Text -> a
runPar parser name state input = case runParser parser state name input of 
    Right a -> a
    Left err -> error (show err)


instance Show Solution where
    show (Solution day name p1 p2) = "Day " ++ show day ++ " \"" ++ name ++ "\", part1: " ++ show p1 ++ ", part2: " ++ show p2

solutionNum :: Solution -> Int
solutionNum (Solution n _ _ _) = n

solutionToTuple :: Solution -> (Int, String, String, String)
solutionToTuple (Solution d name x y) = (d, name, show x, show y)






runSolver' :: (Int -> FilePath) -> Solver -> IO Solution
runSolver' inputFinder (Solver n _ _ solver) = solver <$> TIO.readFile (inputFinder n)


runSolver :: Solver -> IO Solution
runSolver = runSolver' (printf "inputs/%02d.txt")

genTable :: [Solution] -> String
genTable sols = makeTableWithHeaders ["Day", "Name", "Part 1", "Part 2"] [show <$> a,b,c,d]
    where (a,b,c,d) = unzip4 $ fmap solutionToTuple sols








{-

import System.Environment
import System.CPUTime (getCPUTime)
import Control.Monad
import Control.Exception

genTableWithTimings :: [Solver] -> String
genTableWithTimings solvers = makeTableWithHeaders ["Day", "Name", "Part 1", "Part 2", "Time"] [show <$> a,b,c,d]
    where 
        (a,b,c,d) = unzip4 $ fmap solutionToTuple sols
        sols = error ""

-- a cpu time in ps
showCpuTime :: Integer -> String
showCpuTime time 
    | time > 1000000000 = printf "%d s" (td / 1000000000)
    | time > 1000000 = printf "%d m" (td / 1000000)
    | time > 1000 = printf "%d ns" (td / 1000)
    | otherwise = printf "%d ps" time
    where 
        td :: Double
        td = fromIntegral time
        

mkTimed :: (a -> b) -> a -> IO (b, Integer)
mkTimed f inp = do
    start <- getCPUTime
    res <- evaluate $ f inp
    -- rnf x `seq` return ()
    end <- getCPUTime
    let diff = fromIntegral (end - start)
    -- printf "Computation time: %0.9f sec\n" (diff :: Double)
    -- printf "Individual time: %0.9f sec\n" (diff / fromIntegral lim :: Double)
    pure (res, diff)



runTimedSolver' :: (Int -> FilePath) -> Solver -> IO (Solution, Integer)
runTimedSolver' inputFinder (Solver n _ solver) = TIO.readFile (inputFinder n) >>= mkTimed solver

 -}
