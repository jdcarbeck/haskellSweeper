{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import System.Random
import Data.Char
import Data.Matrix
import qualified Data.Vector as V
import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions, encode, decode)
import Control.Monad.IO.Class


import Web.Scotty
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Internal as C
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R

import Game
import Solver

type Move = ((Int,Int),Bool)

data GameState = GameState {
    state :: [[Char]],
    sol :: [[Int]],
    isWon :: Bool,
    isLost :: Bool
} deriving (Generic, Show)

instance ToJSON GameState where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON GameState

data MoveFromState = MoveFromState {
    mvState :: [[Char]],
    mvSol :: [[Int]],
    x :: Int,
    y :: Int,
    mvType :: Bool
} deriving (Generic, Show)
instance ToJSON MoveFromState where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON MoveFromState


main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        html $ page
    get "/new" $ do
        seed <- liftIO newStdGen
        let board = newGame 10 10 10 seed
        newGameBoard board
    post "/" $ do
        x <- jsonData
        move x
    post "/ai" $ do
        x <- jsonData
        ai x


page :: T.Text
page = do
    R.renderHtml $ do
        H.head $ do
            H.title "Minesweeper"
            H.script H.! A.src "https://unpkg.com/axios/dist/axios.min.js" $ " "
            H.script H.! A.src "https://unpkg.com/vue" $ " "
        H.body $ do
            H.div H.! A.id "app" $ do
                H.h1 "Minesweeper"
                H.p "Click cells to reveal them, use the flag option to flag cells when clicking."
                H.button H.! C.customAttribute "v-on:click" "newGame" $ "New Game"
                H.button H.! C.customAttribute "v-on:click" "aiMove" $ "AI Move"
                H.label H.! A.for "flagged" $ " "
                H.input H.! A.type_ "checkbox" H.! A.id "flagged" H.! C.customAttribute "v-model" "flag"
                H.table $ do
                    H.tr H.! C.customAttribute "v-for" "(row,x) in boardState" $ do
                        H.th H.! C.customAttribute "v-for" "(item,y) in row" H.! C.customAttribute "v-on:click" "makeMove(x,y)" $ "{{ item }}"

            H.script H.! A.type_ "application/javascript" $ "new Vue({ el: \"#app\", data: { boardState: [[]], boardSol: [[]], win: false, lost: false, flag: true }, methods: { newGame: function(){ console.log(\"Making a new game\"); axios.get(\"http://localhost:3000/new\") .then(resp =>{ console.log(resp.data); this.boardState = resp.data.state; this.boardSol = resp.data.sol; this.win = resp.data.isWon; this.lost = resp.data.isLost; }) .catch(error => {}) }, aiMove: function(){ console.log(\"Making a AI move\"); axios.post(\"http://localhost:3000/ai\", { \"mvState\": this.boardState, \"mvSol\": this.boardSol, \"x\": 0, \"y\": 0, \"mvType\": false, }) .then(resp =>{ console.log(resp.data); this.boardState = resp.data.state; this.boardSol = resp.data.sol; this.win = resp.data.isWon; this.lost = resp.data.isLost; }) .catch(error ={}) }, makeMove: function(x,y){ x = x+1; y = y+1; flag = this.flag; console.log(\"Making a move: \" + x + \", \" + y + \" Reveal: \" + flag); axios.post(\"http://localhost:3000/\",{ \"mvState\": this.boardState, \"mvSol\": this.boardSol, \"x\": x, \"y\": y, \"mvType\": this.flag, }) .then(resp =>{ console.log(resp.data.state); this.boardState = resp.data.state; this.boardSol = resp.data.sol; this.win = resp.data.isWon; this.lost = resp.data.isLost; }) .catch(error ={}) } }, mounted: function(){} });"
                
newGameBoard :: Board -> ActionM()
newGameBoard (state, sol) = do
        let n = nrows state
            stateAsArray = matrixToList state n
            solAsArray = matrixToList sol n
            game = GameState stateAsArray solAsArray False False
        json game
            
matrixToList :: Matrix a -> Int -> [[a]]
matrixToList m 1 =  [(V.toList $ getRow 1 m)]
matrixToList m x = (matrixToList m (x-1)) ++ [(V.toList $ getRow x m)]

move :: MoveFromState -> ActionM()
move s = do
    let boardState = fromLists $ mvState s
        boardSol = fromLists $ mvSol s
        (state',sol') = makeMove ((x s),(y s)) (mvType s) (boardState,boardSol)
        stateArr = matrixToList state' (nrows state')
        solArr = matrixToList sol' (nrows sol')
        game = GameState stateArr solArr (win (state',sol')) (lost (state',sol'))
    json game
                    
ai :: MoveFromState -> ActionM()  
ai s = do 
    let boardState = fromLists $ mvState s
        boardSol = fromLists $ mvSol s
        ((x,y),mv) = head $ aiMove (boardState,boardSol)
        (state',sol') = makeMove (x,y) mv (boardState,boardSol)
        stateArr = matrixToList state' (nrows state')
        solArr = matrixToList sol' (nrows sol')
        game = GameState stateArr solArr (win (state',sol')) (lost (state',sol'))
    json game

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
    -- minesweeper board

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
