module Example where

import Data.Array.IO
import Control.Monad

import Data.List
import System.Random

import Item as It
import Direction
import Room
import Player
import GameState as Gs
import qualified Item as It
import Data.IntMap (fromList)
import qualified Data.Map as M
import Data.Maybe

class Example a where
    example :: IO a

newtype Set = Set [Point] deriving (Eq,Show)

instance Example Item where
    example = do
            iname <- choose itemNames
            randWeight <- randomRIO (1,120)
            return (Item iname randWeight)

instance Example Direction where
    example = do choose [N,S,E,W]

exitExample ::IO Exit
exitExample = do
                dir <- example ::IO Direction
                rname <- choose roomNames
                return (dir,rname)

instance Example Room where
    example = do
            rname <- choose roomNames
            exits <- exampleList exitExample (randomRIO (2,4))
            objs <- exampleList (choose itemNames) (randomRIO (2,5))
            return (Room rname
                ("You are in a randomly gen room which is  " ++ show rname)
                exits
               (nub objs))


instance Example Player where
    example = do
        items <- exampleList (choose itemNames) (randomRIO (0,10))
        room <- choose roomNames
        weight <- randomRIO (30,50)
        return(Player (nub items) weight room)

instance Example GameState  where
    example = do
        randGrid <- randomMap
        return (GameState
                  Nothing
                  (gridToMap randGrid)
                  univ
                  you
                  randGrid)

-- choose a random item from a list
choose :: [a] -> IO a
choose lst = do
  index <- randomRIO(0,length lst-1)
  return (getNthElement index lst)

-- get the nth element of a list
getNthElement:: Int -> [a] -> a
getNthElement n lst = let (a,b) = splitAt n lst
                      in head b

exampleList :: IO a -> IO Int -> IO [a]
exampleList  rand1 rand2 =
    do
        len <- rand2
        sequence (replicate len rand1)

exampleGrid :: [Room] -> IO GameGrid
exampleGrid rooms = do
                    shuffledRooms <- shuffle rooms
                    return (M.fromList $ zip coords shuffledRooms)

-- shuffle a list and returns a side effectful list
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

-- makes points into a set
mkSet :: (Int, Int) -> Set
mkSet (x,y) = Set [(x,y)]

-- removes all the exists from a room, its a very useful helper function

zeroExits :: Room -> Room
zeroExits room = room {exits = []}

-- all possible coords in 3X3 grid
coords :: [Point]
coords =[(,)] <*> [0,1,2] <*> [0,1,2]

-- return true if the given two points are neighbours
neighbors :: Point -> Point -> Bool
neighbors (x1,y1) (x2,y2) = (x1 == x2 && y1 == y2+1)
                            ||  (x1 == x2 && y1 == y2-1)
                            ||  (x1 == x2+1 && y1 == y2)
                            ||  (x1 == x2-1 && y1 == y2)

-- returns a list of all the possible links between two sets of points
possibleLinks :: Set -> Set -> [(Point, Point)]
possibleLinks (Set []) (Set ((_, _):_:_)) = []
possibleLinks (Set ((_, _):_:_)) (Set []) = []
possibleLinks (Set []) (Set []) = []
possibleLinks (Set [x]) (Set []) = []
possibleLinks (Set []) (Set [y]) = []
possibleLinks (Set (x:xs)) (Set (y:ys)) = if neighbors x y
                                            then (x,y):possibleLinks (Set xs) (Set ys)
                                            else possibleLinks (Set xs) (Set ys)

-- returns the orientation of the link between source and destination
orientation :: Point -> Point -> Maybe Direction
orientation (x1,y1) (x2,y2)
  | x1 == x2 = if y2 > y1 then Just E else if y2 < y1 then Just W else Nothing
  | y1 == y2 = if x2 > x1 then Just S else if x2 < x1 then Just N else Nothing
  | otherwise = Nothing

-- get only the exit directions from a room
getExitDirections :: [Exit] -> [Direction]
getExitDirections [] = []
getExitDirections ((dir,_):xs) = dir:getExitDirections xs

-- add a exit to a room, with direction and destination
addExit :: Room -> Direction -> Room -> Room
addExit destination dir source = if dir 
                                    `elem`
                                    getExitDirections (exits source)
                                  then source
                                  else source {exits
                                          = (dir,rname destination)
                                            :exits source}

-- Link points together, if they are neighbours and orientation is valid
nodeLink :: Point -> Point -> GameGrid -> Maybe GameGrid
nodeLink p1 p2 grid
  | isNothing (orientation p1 p2) = Nothing
  | orientation p1 p2 == Just N 
    && neighbors p1 p2 = Just (nodeLinkHelper p1 p2 grid N)
  | orientation p1 p2 == Just S
    && neighbors p1 p2 = Just (nodeLinkHelper p1 p2 grid S)
  | orientation p1 p2 == Just E 
    && neighbors p1 p2 = Just (nodeLinkHelper p1 p2 grid E)
  | orientation p1 p2 == Just W 
    && neighbors p1 p2 = Just (nodeLinkHelper p1 p2 grid W)
  | otherwise = Nothing

-- helper function for nodeLink which basically adds the exit to room in 
  -- the given direction to the given destination room
nodeLinkHelper:: Point -> Point -> GameGrid -> Direction ->  GameGrid
nodeLinkHelper p1 p2 gg N = let
                              temp =
                                M.insert
                                  p1 (addExit (gg M.! p2) N (gg M.! p1)) gg
                            in
                              M.insert
                               p2 (addExit (temp M.! p1) S (temp M.! p2)) temp
nodeLinkHelper p1 p2 gg E = let
                              temp =
                                M.insert
                                  p1 (addExit (gg M.! p2) E (gg M.! p1)) gg
                            in
                              M.insert
                                p2 (addExit (temp M.! p1) W (temp M.! p2)) temp
nodeLinkHelper p1 p2 gg S = let temp =
                                  M.insert
                                    p1 (addExit (gg M.! p2) S (gg M.! p1)) gg
                            in
                              M.insert
                                p2 (addExit (temp M.! p1) N (temp M.! p2)) temp
nodeLinkHelper p1 p2 gg W = let temp =
                                  M.insert
                                    p1 (addExit (gg M.! p2) W (gg M.! p1)) gg
                            in
                              M.insert
                                p2 (addExit (temp M.! p1) E (temp M.! p2)) temp

-- returns true if the a point from the first set is a negbhbour of a point
  -- from the second set
setNeighbors :: Set -> Set -> Bool
setNeighbors (Set []) (Set []) = True
setNeighbors (Set [x]) (Set []) = False
setNeighbors (Set []) (Set [y]) = False
setNeighbors (Set set1) (Set set2) = let
                                      allcombinations
                                        = [(,)] <*> set1 <*> set2
                                    in any (\(x,y) -> neighbors x y) allcombinations

-- gives union of two sets
setUnion :: Set -> Set -> Set
setUnion (Set []) (Set []) = Set []
setUnion (Set [x]) (Set []) = Set [x]
setUnion (Set []) (Set [y]) = Set [y]
setUnion (Set set1) (Set set2) = Set (set1 `union` set2)

-- setLink takes two sets of points and a game grid as inputs, and returns a 
-- game grid with two exits linking rooms located at randomly-selected pair of 
-- neighboring points, one from the first set and one from the second set.

setLink :: Set -> Set -> GameGrid -> IO (Maybe GameGrid)
setLink (Set s1) (Set s2) gg = do
                                p1 <- choose s1
                                p2 <- choose s2
                                return (nodeLink p1 p2 gg)

-- updateSetList takes two sets and a list of sets as an input. You can assume 
-- that the input list of sets will contain each of the two input sets, 
-- possibly among others. It deletes the two input sets from the list of sets, 
-- and adds the union of those two input sets to the list. 
updateSetList :: Set -> Set -> [Set] -> [Set]
updateSetList s1 s2 [] = []
updateSetList s1 s2 setLst = setUnion s1 s2:updateSetListHelper s1 s2 setLst

-- a helper function for updateSetList which returs the list of sets with the
-- two input sets removed
updateSetListHelper :: Set -> Set -> [Set] -> [Set]
updateSetListHelper s1 s2 [] = []
updateSetListHelper s1 s2 (x:xs) = if x == s1 || x == s2
                                    then updateSetListHelper s1 s2 xs
                                    else x:updateSetListHelper s1 s2 xs

-- one round of kruskal's algorithm
oneRound :: [Set] -> GameGrid -> IO (Maybe GameGrid, Set, Set)
oneRound [] gg = return (Just gg, Set [], Set [])
oneRound setLst gg = do
                       set1 <- choose setLst
                       set2 <- choose setLst
                       if setNeighbors set1 set2
                       then do
                              newGrid <- setLink set1 set2 gg
                              return (newGrid, set1, set2)
                       else oneRound setLst gg

-- generating a random game grid from one round method
generateMap :: [Set] -> GameGrid -> IO GameGrid
generateMap [] gg = return gg
generateMap [x] gg = return gg
generateMap setLst gg = do
                          (newGrid, set1, set2) <- oneRound setLst gg
                          case newGrid of
                            Nothing -> generateMap setLst gg
                            Just correctGrid -> generateMap (updateSetList set1 set2 setLst) correctGrid

-- storing the random game grid in a constant
randomMap :: IO GameGrid
randomMap = do
             initialGrid <- exampleGrid (map zeroExits allRooms)
             let initialSets  = map mkSet (M.keys initialGrid)
             generateMap initialSets initialGrid

-- -- converts a game grid to a gameMap
-- moved to game state while adding a feature for better use
-- since its better to reuse than have dupplicate code
-- gridToMap :: GameGrid -> GameMap
-- gridToMap gg = M.fromList (map (\x -> (rname x, x)) (M.elems gg))

