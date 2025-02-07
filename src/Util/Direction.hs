module Util.Direction (Direction (N,E,S,W), rotateRight, rotateLeft, directionToCoord, isVertical, isHorizontal, orthogonal, directions ) where

import Data.Ix

data Direction = N | E | S | W deriving stock (Show, Eq, Ord, Enum)

rotateRight :: Direction -> Direction
rotateRight = toEnum . (`mod` 4) . (+1) . fromEnum

rotateLeft :: Direction -> Direction
rotateLeft = toEnum . (`mod` 4) . (+3) . fromEnum


directionToCoord :: Direction -> (Int,Int)
directionToCoord N = (-1,0)
directionToCoord S = (1,0)
directionToCoord E = (0,1)
directionToCoord W = (0,-1)

isVertical,isHorizontal :: Direction -> Bool
isVertical N = True
isVertical S = True
isVertical W = False
isVertical E = False
isHorizontal = not . isVertical

orthogonal :: Direction -> Direction -> Bool
orthogonal N N = False
orthogonal N S = False
orthogonal N E = True
orthogonal N W = True
orthogonal S N = False
orthogonal S S = False
orthogonal S E = True
orthogonal S W = True
orthogonal E N = True
orthogonal E S = True
orthogonal E E = False
orthogonal E W = False
orthogonal W N = True
orthogonal W S = True
orthogonal W E = False
orthogonal W W = False

directions :: [Direction]
directions = [N,E,S,W]


instance Ix Direction where
    range (l,u) = toEnum <$> [fromEnum l..fromEnum u]
    -- range (N,N) = [N]
    -- range (N,E) = [N,E]
    -- range (N,S) = [N,E,S]
    -- range (N,W) = [N,E,S,W]
    -- range (E,E) = [E]
    -- range (E,S) = [E,S]
    -- range (E,W) = [E,S,W]
    -- range (S,S) = [S]
    -- range (S,W) = [S, W]
    -- range (W,W) = [W]
    -- range _ = []

    index (l,u) i = fi - fl
    -- if fl <= fi && fu <= fi
        where 
            fl = fromEnum l
            -- fu = fromEnum u
            fi = fromEnum i
                
    inRange (l,u) i = fromEnum l <= fromEnum i && fromEnum u <= fromEnum i
    

