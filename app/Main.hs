{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import System.Random
import Data.Matrix
import qualified Data.Vector as V

import Control.Applicative
import Control.Monad
import Data.IORef

import Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Game
import Solver

type Move = ((Int,Int),Bool)

main :: IO ()
main =  do
    seed <- newStdGen
    let board = newGame 10 10 1 seed
    let board' = makeMove (1,1) True board
    startGUI defaultConfig (setup board')


setup :: Board -> Window -> UI()
setup b window = void $ do
    return window # set title "Sweeper"
    aiMove <- UI.button #+ [ string "AI Move" ]
    moveType <- UI.input
                    # set UI.type_ "checkbox" 
                    # set (attr "id") "flag"
    moveLabel <- UI.label #+ [ string "Flag" ]
                    # set (attr "for") "flag"
    let board = mkBoard b
    
    getBody window #+ [ grid board, element aiMove, element moveType, element moveLabel ]


mkBoard :: Board -> [[UI Element]]
mkBoard (state, _) = elBoard state (nrows state) (ncols state)

elBoard :: Field -> Int -> Int -> [[UI Element]]
elBoard f 1 y = [(elRow f 1 y)]
elBoard f x y = (elBoard f (x-1) y) ++ [(elRow f x y)]

elRow :: Field -> Int -> Int -> [UI Element]
elRow f x 1 = (elCell (x,1) $ f!(x,1)):[]
elRow f x y = (elRow f x (y-1)) ++ [(elCell (x,y) $ f!(x,y))]

elCell :: (Int,Int) -> Char -> UI Element
elCell p c | c == '*' = UI.button #+ [ string (c:[]) ] 
                            # set style [("border-radius","3px"), ("margin","2px"), ("border-style","solid"), ("height","30px"), ("width","30px"), ("outline", "0px")]
                            #. (show p)
            | c == 'F' = UI.button #+ [ string (c:[]) ] 
                            # set style [("border-radius","3px"), ("margin","2px"), ("border-style","solid"), ("height","30px"), ("width","30px"), ("outline", "0px")]
                            #. (show p)
            | c == 'B' = UI.button #+ [ string (c:[]) ] 
                            # set style [("border-radius","3px"), ("margin","2px"), ("border-style","solid"), ("height","30px"), ("width","30px"), ("outline", "0px")]
                            #. (show p)  
            | c == ' ' = UI.button #+ [ string ('0':[]) ] 
                            # set style [("color", "#FFFFFF"),("border-radius","3px"), ("margin","2px"), ("border-style","solid"), ("height","30px"), ("width","30px"), ("outline", "0px")]
                            #. (show p)
            | otherwise = UI.button #+ [ string (c:[]) ] 
                            # set style [("border-radius","3px"), ("margin","2px"), ("border-style","solid"), ("height","30px"), ("width","30px"), ("outline", "0px")]
                            #. (show p)


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
