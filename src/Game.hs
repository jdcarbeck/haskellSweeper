module Game (genBombs, genSolution, genBoard) where

import System.Random
import Data.Ix


type Point = (Int,Int)
data Plays = Flag | Covered | Shown

type Bombs = Matrix Bool
type Solution = Matrix Int
type PlayedMoves = [[Plays]]

type Board = [[Point]]

data Minesweeper = Minesweeper { 
        width :: Int,
        height :: Int,
        mines :: Int, 
        gameState :: PlayedMoves,
        gameSolution :: Solution,
        gameRegions :: Board 
    }

genBombs :: Int -> Int -> Int -> StdGen -> Bombs
genBombs w h n r = matrix w h (\(i,j) -> elem (i,j) p)
                    where
                        allp = genPoints w h
                        p = bombPoints allp n r

genSolution :: Matrix Bool -> Solution
genSolution bombs = matrix w h (\(i,j) -> cellType (i,j) bombs)
                        where 
                            w = nrows bombs
                            h = ncols bombs

cellType :: Point -> Matrix Bool -> Int
cellType (i,j) bombs | (getBomb i j bombs) = -1
                     | otherwise = sum $ map fromEnum [tl,t,tr,l,r,bl,b,br]
                        where 
                            tl = getBomb (i-1) (j-1) bombs
                            t = getBomb (i-1) j bombs
                            tr = getBomb (i-1) (j+1) bombs
                            l = getBomb i (j-1) bombs
                            r = getBomb i (j+1) bombs
                            bl = getBomb (i+1) (j-1) bombs
                            b = getBomb (i+1) j bombs
                            br = getBomb (i+1) (j+1) bombs
        
getBomb :: Int -> Int -> Matrix Bool -> Bool
getBomb i j m = case (safeGet i j m) of
                    Just b -> b
                    Nothing -> False

getCel :: Int -> Int -> Matrix Int -> Int
getCel i j m = case (safeGet i j m) of
                    Just val -> val
                    Nothing -> -1

bombPoints :: [Point] -> Int -> StdGen -> [Point]
bombPoints bombs n rand = take n (shuffle' bombs (length bombs) rand)

genPoints :: Int -> Int -> [Point]
genPoints w h  = [ (x,y) | x <- [1 .. w], y <- [1 .. h] ]


genBoard :: Solution -> Board
genBoard sol = getRegions p sol 
            where 
                r = nrows sol
                c = ncols sol
                p = genPoints r c 

-- Region starting with each point which idetifies the points around it that are not bombs
getRegions :: [Point] -> Solution -> [[Point]]
getRegions ((i,j):[]) sol = (getRegion (i,j) sol):[]
getRegions (x:xs) sol | ((sol!x) == 0) = (getRegion x sol):regions
                      | otherwise = [x]:regions
                            where regions = getRegions xs sol

getRegion :: Point -> Solution -> [Point]
getRegion (i,j) sol | ((getCel i j sol) == 0) = [(i,j)] ++ findConnected (i,j) sol
                    | otherwise = [] 

findConnected :: Point -> Solution -> [Point]
findConnected (i,j) sol = region
                    where
                        tl = connected ((i-1),(j-1)) sol
                        t = connected ((i-1),j) sol
                        tr = connected ((i-1),(j+1)) sol
                        l = connected (i,(j-1)) sol
                        r = connected (i,(j+1)) sol
                        bl = connected ((i+1),(j-1)) sol
                        b = connected ((i+1),j) sol
                        br = connected ((i+1),(j+1)) sol
                        region = tl ++ t ++ tr ++ l ++ r ++ bl ++ b ++ br

connected :: Point -> Solution -> [Point]
connected (i,j) sol | (val == -1) = []
                    | otherwise = [(i,j)]
                    where
                        val = getCel i j sol
-- -------------------------------------------------------------------------

