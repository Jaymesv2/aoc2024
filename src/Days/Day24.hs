module Days.Day24 (day24) where

import Solver
import Control.Arrow
-- import Text.Parsec
import Data.Text (Text)
import Debug.Trace
import Data.Function.Memoize
import Util.Parsers
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Array.IArray
-- import Data.Function
import Data.Map (Map)
import Data.Map qualified as M
import Util.List 
import Data.List
import Text.Parsec
import Data.Function
import Data.Functor
import Data.Graph
import Data.Bits (xor, (.|.), shiftL, setBit)
import Text.Printf

-- import Algebra.Graph as G
-- import Algebra.Graph (Graph)

-- import Algebra.Graph.AdjacencyMap as AM
-- import Algebra.Graph.AdjacencyMap (AdjacencyMap)
-- import Algebra.Graph.AdjacencyMap.Algorithm (topSort)
-- import Algebra.Graph.Labelled


day24 :: Solver
day24 = mkParsecSolver 24 "" () parser (part1 &&& part2)

part1,part2 :: Parsed -> String
part1 j@(wires, connections) = show us
    where
        gateOf v' = let (u,_,_) = f v' in u
        keyOf v' = let (_,u,_) = f v' in u

        (g,f,v) = mkRevConnGraph j
        ts = reverseTopSort g
        baseM, m :: Map String Bool
        baseM = M.fromList wires
        m = foldl' (\acc vert -> let  
            (gate, k, conns) = f vert
            get :: String -> Bool
            get c = acc M.! c
            v' = case gate of
                Gate Xor -> foldl1 xor $ get <$> conns
                Gate And -> all get conns
                Gate Or -> any get conns
                Source -> acc M.! k
            in M.insert k v' acc) baseM ts
        
        us =
            sum $ fmap (\(i,d) -> if d then 2^i else 0 ) $
            zip [0..] $ fmap snd $ filter ( (=='z') . head . fst) $ M.toList m
    
        -- xs = foldl' (\acc (i,x) -> applyWhen x (setBit i) acc) 0 $ zip [0..] us
        -- xs = foldl' (.|.) 0 $ (flip setBit <$> [0..]) <*> fmap fromEnum us



part2 j@(wires, connections) = "unimplemented"
    -- where
    --     (g,f,v) = mkRevConnGraph j



fullAdderGraph :: Int -> [(Gate, String,[String])]
fullAdderGraph i = concat 
        [halfAdder "x00" "y00" "c00" "z00"
        , concatMap (\n -> fullAdderN (printf "c%d" n) n)  [1..44]
        , fullAdderN (printf "y45") 45
        ]


fullAdderN :: String -> Int -> [(Gate,String,[String])]
fullAdderN nextCout n = fullAdder (printf "x%d" n) (printf "y%d" n) (printf "c%d" $ n-1) nextCout (printf "z%d" n) 

halfAdder :: String -> String -> String -> String -> [(Gate, String, [String])]
halfAdder a b cout out = [ (Gate Xor, out, [a,b]), (Gate And, cout, [a,b])]

fullAdder :: String -> String -> String -> String -> String -> [(Gate,String,[String])]
fullAdder a b cin cout out = 
        [ (Gate Xor, g1, [a,b])
        , (Gate And, g2, [a,b])
        , (Gate Xor, cout, [cin, g1])
        , (Gate And, g3, [cin, g1])
        , (Gate Or, out, [g2, g3])
    ]
        where
            g1 = "fst"
            g2 = "snd"
            g3 = "thrd"


mkRevConnGraph :: ([(String, Bool)], [(String, Op, String, String)]) -> (Graph, Vertex -> (Gate, String, [String]), String -> Maybe Vertex) 
mkRevConnGraph (is, conns) = graphFromEdges $ [(Source, name, []) | (name,_) <- is] ++ [(Gate op, out, [a,b]) | (a,op,b,out) <- conns]




data Op = And | Or | Xor deriving stock (Eq, Show)
type Parsed = ([(String, Bool)], [(String, Op, String, String)])

data Gate = Source | Gate Op

parser :: Parsec Text () Parsed
parser = do
    wires <- wire `sepEndBy1` char '\n'
    _ <- char '\n'
    connections <- connection `sepEndBy1` char '\n'
    pure (wires,connections)
    where
        wire = (,) <$> (ident <* string ": ") <*> wireBool
        wireBool = (False <$ char '0') <|> (char '1' $> True)
        ident = count 3 (satisfy (/='\n') )
        op = choice [ string "AND" $> And, string "XOR" $> Xor, string "OR" $> Or ]
        connection = do
            a <- ident 
            o <- char ' ' *> op <* char ' '
            b <- ident
            _ <- string " -> "
            c <- ident 
            pure (a,o,b,c)

