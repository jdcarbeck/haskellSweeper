module Main where

import System.Random
import Game

main :: IO ()
main = do
    r <- newStdGen
    let (state,sol) = newGame 10 8 5 r
    let (state',_) = makeMove (1,1) False (state,sol)
    putStrLn $ show state
    putStrLn $ show state'