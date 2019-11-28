module Solver (aiMove) where

import Game
import Data.Matrix
import Data.Char
import Data.List

-- A set of safe moves
type Move = (Point,Bool)

type PossibleMoves = [Move]

-- takes a board and generates a list of moves
aiMove :: Board -> PossibleMoves
aiMove (state, sol) = genPossibleMoves adjpoints revealed state
                    where
                        adjpoints = getCells (state, sol) isAdj
                        revealed = getCells (state, sol) isRevealed

-- Takes a board and creates a list of all points of adjacent type
getCells :: Board -> (Char -> Bool) -> [Point]
getCells (state, sol) func = findCells points func state
                    where points = genPoints (nrows state) (ncols state)

-- from a list of points uses a condition to return a list of points that satisfiy that condition                    
findCells :: [Point] -> (Char -> Bool) -> Field -> [Point]
findCells (x:[]) func state | func (state!x) = [x]
                            | otherwise = [] 
findCells (x:xs) func state | func (state!x) = x:(findCells xs func state)
                            | otherwise = findCells xs func state

-- condition for revealed                            
isRevealed :: Char -> Bool
isRevealed c = (c /= '*')

-- condition for a adj cell
isAdj :: Char -> Bool
isAdj c = (c /= ' ') && (c /= 'F') && (c /= '*')

-- returns a ordered list based of the movetype based off the possible moves from the revealed moves in the Field
genPossibleMoves :: [Point] -> [Point] -> Field -> PossibleMoves
genPossibleMoves [] p _ = []
genPossibleMoves (x:[]) p f  = getPossibleMoves x p f
genPossibleMoves (x:xs) p f  = joinedMoves
                            where
                                movesForPoint = getPossibleMoves x p f
                                otherMoves = genPossibleMoves xs p f
                                moveType = getMoveType movesForPoint
                                joinedMoves | moveType = otherMoves `union` movesForPoint
                                            | otherwise = movesForPoint `union` otherMoves

-- returns if the move is a reveal or a flagg
getMoveType :: PossibleMoves -> Bool
getMoveType [] = False
getMoveType ((_,b):_) = b

-- takes a point and a list of points already searched for and determines set of PossibleMoves
getPossibleMoves :: Point -> [Point] -> Field -> PossibleMoves
getPossibleMoves p ps field | (label == (length u)) = makeMoves f True
                            | (label == ((length u) + (length f'))) = makeMoves f False
                            | otherwise = []
                    where
                        f = (findNeighbors p field notMine) \\ ps
                        u = (findNeighbors p field mine) \\ ps
                        f' = (findNeighbors p field unKnownCell) \\ ps
                        label = digitToInt (field!p)
                        
notMine :: Char -> Bool
notMine c = not (mine c)

unKnownCell :: Char -> Bool
unKnownCell c = c == '*'

mine :: Char -> Bool
mine c = c == 'F'

-- from moves and move type make Possible Moves
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