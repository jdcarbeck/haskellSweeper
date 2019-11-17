module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import System.Random
import Data.Maybe

import Game

type Size = Float
type Origin = (Float,Float)
type Region = (Cords,(Float,Float),(Float,Float))
type Cords = (Int,Int)

drawMines :: BoardState -> Size -> Origin -> Picture
drawMines (((i,j),c):[]) scale (x,y) | (c == '*') = pic
                                     | otherwise = color yellow pic
            where 
                i' = fromIntegral i
                j' = fromIntegral j
                x' = (i'-1.0)*scale
                y' = (-(j'-1.0)*scale)
                s = scale*0.95
                pic = translate x' y' $ translate x y $ rectangleSolid s s
drawMines (((i,j),c):ps) scale (x,y) | (c == '*') = pictures [pic,otherSquares]
                                     | otherwise = pictures [(color yellow pic),otherSquares]
            where 
                i' = fromIntegral i
                j' = fromIntegral j
                x' = (i'-1.0)*scale
                y' = (-(j'-1.0)*scale)
                s = scale*0.95
                pic = (translate x' y' $ translate x y $ rectangleSolid s s)
                otherSquares = drawMines ps scale (x,y)


drawBoard :: Size -> Float -> Board -> Picture
drawBoard size n b = drawMines (boardState b) size' (x,y)
                where 
                    size' = (size/n)
                    x = (-size/2) + (size'/2)
                    y = (size/2) - (size'/2)

-- CheckCordinates which takes mous x and mouse y and determines if it is a point in the board
-- checkCoordinate :: Size -> Float -> Float -> Maybe Int
-- checkCoordinate size n f' =
--     let f

handleKeys :: [Region] -> Event -> Board -> Board
handelKeys regions (EventKey (MouseButton LeftButton) Down _ p) gameBoard =
    fromMaybe gameBoard $ do
        p' <- checkCoordinate regions p
        return $ makeMove p' True gameBoard
handleKeys _ _ gameBoard = gameBoard -- handels a event thats not specified

checkCoordinate :: [Region] -> (Float,Float) -> Maybe (Int,Int)
checkCoordinate (x:[]) f | f `inRegion` x = Just $ getRegionCord x
                         | otherwise = Nothing
checkCoordinate (x:xs) f | f `inRegion` x = Just $ getRegionCord x
                         | otherwise = checkCoordinate xs f

getRegionCord :: Region -> (Int,Int)
getRegionCord ((i,j),_,_) = (i,j)

inRegion :: (Float,Float) -> Region -> Bool
inRegion (x,y) (_,(minX,maxX),(minY,maxY)) | xRange && yRange = True
                                           | otherwise = False
                                        where
                                            xRange = ((x >= minX) && (x < maxX))
                                            yRange = ((y >= minY) && (y < maxY))

tileRegions :: Size -> Float -> (Float,Float) -> Board -> [Region]
tileRegions size n ori b = genRegions (boardState b) size' ori
        where 
            size' = (size/n)

genRegions :: BoardState -> Size -> Origin -> [Region]
genRegions (((i,j),_):[]) scale (x,y) = [((i,j),(minX,maxX),(minY,maxY))]
                                where
                                    i' = fromIntegral i
                                    j' = fromIntegral j
                                    minX = x + (i'-1.0)*scale
                                    maxX = x + (i'-1.0)*scale + scale
                                    minY = y + (j'-1.0)*scale 
                                    maxY = y + (j'-1.0)*scale + scale
genRegions (((i,j),_):ps) scale (x,y) = ((i,j),(minX,maxX),(minY,maxY)) : genRegions ps scale (x,y)
                                where
                                    i' = fromIntegral i
                                    j' = fromIntegral j
                                    minX = x + (i'-1.0)*scale
                                    maxX = x + (i'-1.0)*scale + scale
                                    minY = y + (j'-1.0)*scale 
                                    maxY = y + (j'-1.0)*scale + scale

main :: IO()
main = do
    r <- newStdGen
    -- let size = 400.0
    -- let n = 2
    -- let newBoard = newGame 2 2 1 r
    -- let regions = tileRegions size n (100,100) newBoard
    -- putStrLn $ show $ checkCoordinate regions (310,200) 
    let window = InWindow "Sweeper" (500,500) (100, 100)
        size = 400.0
        n = 5
        newBoard = newGame 5 5 1 r
        regions = tileRegions size n (100,100) newBoard  
        in play window white 1 newBoard (drawBoard size n) (handleKeys regions) (flip const)