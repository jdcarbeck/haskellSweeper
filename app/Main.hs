{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Random

import Game

type Move = ((Int,Int),Bool)

main :: IO ()
main = do
    seed <- newStdGen
    let board = newGame 10 10 1 seed
    minesweeper board

minesweeper :: Board -> IO ()
minesweeper (state, sol) = do
    putStrLn $ show state
    if win (state, sol)
    then putStrLn "You Win, Congrats!"
    else
        if lost (state,sol)
        then putStrLn "Boom! You lost :("
        else do
            (point, move) <- getMove
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
