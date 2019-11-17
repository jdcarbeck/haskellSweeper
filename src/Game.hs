module Game (newGame, makeMove, win, lost, boardState, BoardState, Board) where

import System.Random.Shuffle
import System.Random
import Data.Matrix
import Data.List

type Point = (Int,Int)

type Bombs = Matrix Bool
type Solution = Matrix Int
type Field = Matrix Char
type Board = (Field, Solution)
type BoardState = [(Point,Char)]

data Move a = Reveal | Flag

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

-- Region starting with each point which idetifies the points around it that are not bombs
-- getRegions :: [Point] -> Solution -> [[Point]]
-- getRegions ((i,j):[]) sol = (getRegion (i,j) sol):[]
-- getRegions (x:xs) sol | ((sol!x) == 0) = (getRegion x sol):regions
-- /                      | otherwise = [x]:regions
--                             where regions = getRegions xs sol

getRegion :: Point -> Solution -> [Point]
getRegion (i,j) sol | ((getCel i j sol) == 0) = findConnected (i,j) sol
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
                        region =  tl ++ t ++ tr ++ l ++ r ++ bl ++ b ++ br

connected :: Point -> Solution -> [Point]
connected (i,j) sol | (val == -1) = []
                    | otherwise = [(i,j)]
                    where
                        val = getCel i j sol


adj :: Point -> Solution -> [Point]
adj p sol = findAdj (getRegion p sol) [p] sol

-- points to examine, found points
findAdj :: [Point] -> [Point] -> Solution -> [Point]
findAdj [] found sol = found
findAdj (x:xs) found sol = findAdj xs' found' sol
                        where 
                            found' = (x:found)
                            xss = (getRegion x sol) `union` xs
                            xs' = (xss \\ found')
 -- -------------------------------------------------------------------------
-- Game Function here

newGame :: Int -> Int -> Int -> StdGen -> Board
newGame w h nBombs rand = (matrix w h $ \(i,j) -> '*', sol)
                        where
                            bombs = genBombs w h nBombs rand
                            sol = genSolution bombs

makeMove :: Point -> Bool -> Board -> Board
makeMove p b (state, sol) | b = ((reveal showPoints state sol), sol)
                          | isFlaged = ((setElem '*' p state),sol)
                          | otherwise = ((setElem 'F' p state),sol)
                          where
                            showPoints = adj p sol
                            isFlaged = (state!p == 'F')


reveal :: [Point] -> Field -> Solution -> Field
reveal [] f sol = f
reveal (x:xs) f sol = reveal xs f' sol
                where 
                    val | c == -1 = 'B'
                        | c == 0 = ' '
                        | otherwise = head $ show c
                        where c = sol ! x
                    f' = setElem val x f

-- win condition all bombs are marked
win :: Board -> Bool
win (state, sol) = check points (state, sol) marked (&&)
                where points = genPoints (nrows state) (ncols state)

marked :: Point -> Board -> Bool
marked p (state, sol) | isBomb && isMarked = True
                      | (not isBomb) = True
                      | otherwise = False
                    where
                        isBomb = (sol!p == -1)
                        isMarked = (state!p == 'F')

check :: [Point] -> Board -> (Point -> Board -> Bool) -> (Bool -> Bool -> Bool) -> Bool
check (x:[]) g f _ = f x g
check (x:xs) g f op = (f x g) `op` (check xs g f op)

unCovered :: Point -> Board -> Bool
unCovered p (state, sol) | isBomb && isReveal = True
                         | otherwise = False
                        where
                            isBomb = (sol!p) == -1
                            isReveal = (state!p == 'B')
lost :: Board -> Bool
lost (state, sol) = check points (state, sol) unCovered (||)
                where points = genPoints (nrows state) (ncols state)

boardState :: Board -> BoardState
boardState (state, _) = genState state points
                    where
                        points = genPoints (nrows state) (ncols state)

genState :: Field -> [Point] -> BoardState
genState f (x:[]) = [(x,(f!x))]
genState f (x:xs) = (x,(f!x)):genState f xs