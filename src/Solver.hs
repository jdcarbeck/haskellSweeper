module Solver (aiMove,getAdj,genPossibleMoves,getPossibleMoves,findNeighbors,makeMoves) where

import Game
import Data.Matrix
import Data.Char
import Data.List

-- A set of safe moves
type Move = (Point,Bool)

type PossibleMoves = [Move]

-- GetFlaged labels = F
-- GetUnknown labels = *
-- Set of possible moves will be based on all points that have a Int then connected points
-- getAllAdj :: Board -> [Point]

-- Takes a board and creates a list of all points of adjacent type
getAdj :: Board -> [Point]
getAdj (state, sol) = findAdj points state
                    where points = genPoints (nrows state) (ncols state)

findAdj :: [Point] -> Field -> [Point]
findAdj (x:[]) state | isAdj (state!x) = [x]
                     | otherwise = [] 
findAdj (x:xs) state | isAdj (state!x) = x:(findAdj xs state)
                     | otherwise = findAdj xs state

isAdj :: Char -> Bool
isAdj c = (c /= ' ') && (c /= 'F') && (c /= '*')

-- TODO ISSUE IS TO DO WITH THE SOLUTION CONTAINING ALREAD MADE MOVES
aiMove :: Board -> PossibleMoves
aiMove (state, sol) = genPossibleMoves points state
                    where
                        points = getAdj (state, sol)

genPossibleMoves :: [Point] -> Field -> PossibleMoves
genPossibleMoves [] _ = []
genPossibleMoves (x:[]) f  = getPossibleMoves x f
genPossibleMoves (x:xs) f  = joinedMoves
                            where
                                movesForPoint = getPossibleMoves x f
                                otherMoves = genPossibleMoves xs f
                                moveType = getMoveType movesForPoint
                                joinedMoves | moveType = otherMoves `union` movesForPoint
                                            | otherwise = movesForPoint `union` otherMoves


getMoveType :: PossibleMoves -> Bool
getMoveType [] = False
getMoveType ((_,b):_) = b

getPossibleMoves :: Point -> Field -> PossibleMoves
getPossibleMoves p field | (label == (length u)) = makeMoves f True
                         | (label == ((length u) + (length f'))) = makeMoves f False
                         | otherwise = []
                    where
                        f = findNeighbors p field notMine
                        u = findNeighbors p field mine
                        f' = findNeighbors p field unKnownCell
                        label = digitToInt (field!p)

notMine :: Char -> Bool
notMine c = not (mine c)

unKnownCell :: Char -> Bool
unKnownCell c = c == '*'

mine :: Char -> Bool
mine c = c == 'F'


makeMoves :: [Point] -> Bool -> PossibleMoves
makeMoves [] b = []
makeMoves (x:xs) b = (x,b):(makeMoves xs b)


-- function used to determine neighbouring points that contain a pass value 
findNeighbors :: Point -> Field -> (Char -> Bool) -> [Point]
findNeighbors (i,j) f c = tl ++ t ++ tr ++ l ++ r ++ bl ++ b ++ br
                    where
                        tl = getCellCon (i-1,j-1) f c 
                        t = getCellCon (i-1,j) f c 
                        tr = getCellCon (i-1,j+1) f c 
                        l = getCellCon (i,j-1) f c 
                        r = getCellCon (i,j+1) f c 
                        bl = getCellCon (i+1,j-1) f c 
                        b = getCellCon (i+1,j) f c 
                        br = getCellCon (i+1,j+1) f c 

-- returns a cell of a field based on condition it is in bounds and equal to passed char
getCellCon :: Point -> Field -> (Char -> Bool) -> [Point]
getCellCon (i,j) f c | (isValidGet i j f) && (c (f!(i,j))) = [(i,j)]
                     | otherwise = []

-- boolean function to wrap maybe case of safeGet
isValidGet :: Int -> Int -> Matrix Char -> Bool
isValidGet i j m = case (safeGet i j m) of
                    Just x -> True
                    Nothing -> False