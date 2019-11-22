{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Random
import Data.Char
import Data.Matrix

import Game
import Solver

type Move = ((Int,Int),Bool)

main :: IO ()
main = do
    seed <- newStdGen
    let board = newGame 5 5 2 seed
    --     (state,sol) = makeMove (1,1) True board
    -- putStrLn $ printBoard (state,sol)
    -- let adjPoints = getAdj (state,sol)
    -- putStrLn $ show $ adjPoints
    -- let possibleMovesU = findNeighbors (head adjPoints) state '*'
    -- let possibleMovesF = findNeighbors (head adjPoints) state 'F'
    -- putStrLn $ "# of Possible moves Unknown " ++ (show $ possibleMovesU)
    -- putStrLn $ "# of Possible moves Flagged " ++ (show $ possibleMovesF)
    -- putStrLn $ show $ digitToInt (state!(head adjPoints))
    -- let possibleMoves = getPossibleMoves (head adjPoints) state
    -- let madeMoves = makeMoves possibleMovesU True
    -- putStrLn $ show $ possibleMoves
    -- putStrLn $ show $ madeMoves
    -- let (cord, mvType) = aiMove (state,sol)
    -- let board'' = makeMove cord mvType (state,sol)
    -- putStrLn $ show $ printBoard board''
    minesweeper board

minesweeper :: Board -> IO ()
minesweeper (state, sol) = do
    putStrLn $ show state
    putStrLn $ show $ aiMove (state,sol)
    if win (state, sol)
    then putStrLn "You Win, Congrats!"
    else
        if lost (state,sol)
        then putStrLn "Boom! You lost :("
        else do
            if (aiMove (state,sol)) == []
            then do
                (point, move) <- getMove
                minesweeper (makeMove point move (state,sol))
            else do
               let possMoves = aiMove (state,sol)
               let (point, move) = head possMoves
               minesweeper (makeMove point move (state,sol))

getMove :: IO Move
getMove = do
    putStrLn "Enter a action: (f|r)"
    action <- getLine
    let move = head action
    putStrLn "Enter a row: (1..N)"
    row <- getInt
    putStrLn "Enter a col: (1..N)"
    col <- getInt
    let moveType | (move == 'f') = False
                 | otherwise = True
    return ((row,col),moveType)
    
getInt :: IO Int
getInt = fmap read getLine
