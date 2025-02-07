module Util.Graph () where
-- import Algebra.Graph
import Algebra.Graph.Labelled.AdjacencyMap





-- dijkstraOf :: (Ord e, Ord a) => AdjacencyMap e a -> Map



{- {-# LANGUAGE ScopedTypeVariables #-}
-- import Algebra.Graph

import Data.Maybe
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as M
import Data.OrdPSQ as OPSQ

dijkstra :: (Ord i, Eq i, Show i) => (i -> [(i, Int)]) -> i  -> Map i (i, Int)
-- dijkstra neighborsOf start = helper M.empty $ OPSQ.singleton start (0 :: Int) start
dijkstra neighborsOf start = foldMins helper M.empty $ OPSQ.singleton start (0 :: Int) start
    where
        -- fold minimums
        foldMins :: (Ord k, Ord p) => (b -> (k,p,v, OrdPSQ k p v) -> (b, OrdPSQ k p v) ) -> b -> OrdPSQ k p v -> b
        foldMins f b q = maybe b (uncurry (foldMins f) . f b) $ minView q

        -- foldMins f b q = case minView q of
        --     -- Just r -> let (b', q'') = f b r in foldMins f b' q''
        --     Just r -> uncurry (foldMins f) $ f b r
        --     Nothing -> b
        
        -- helper :: (Ord i, Eq i, Show i) => Map i (i, Int) -> OrdPSQ i Int i -> Map i (i, Int)

        helper paths (u, udist :: Int, prev, q') = let 
                paths'' = M.insert u (prev, udist) paths
                q''' = foldl' (\q'' (v, cost) -> 
                    let 
                        altCost = udist + cost
                        vPath = M.lookup v paths
                        (_, alteredQueue) =  
                            OPSQ.alter (\case
                                Nothing -> ((), Just (altCost, u))
                                Just p'@(curCost, _) -> ((), if altCost < curCost then Just (altCost, u) else Just p')
                                ) v q''
                    in if isJust vPath then q'' else alteredQueue
                    ) q' (neighborsOf u)
                in (paths'', q''')

                -- helper paths'' q'''



        -- helper paths q = case minView q of
        --     Just (u :: i, udist :: Int, prev :: i, q') -> let 
        --         paths'' = M.insert u (prev, udist) paths
        --         q''' = foldl' (\q'' (v, cost) -> 
        --             let 
        --                 altCost = udist + cost
        --                 vPath = M.lookup v paths
        --                 (_, alteredQueue) =  
        --                     OPSQ.alter (\case
        --                         Nothing -> ((), Just (altCost, u))
        --                         Just p'@(curCost, _) -> ((), if altCost < curCost then Just (altCost, u) else Just p')
        --                         ) v q''
        --             in if isJust vPath then q'' else alteredQueue
        --             ) q' (neighborsOf u)
        --         in helper paths'' q'''
        --     Nothing -> paths
        -- -}
