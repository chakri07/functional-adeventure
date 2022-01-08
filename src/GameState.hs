module GameState where

import Data.List
import Data.Char
import Control.Exception
import qualified Data.Map as M

import Item
import Room
import Player
import Direction
import Data.Map (filterWithKey)

type GameMap = M.Map RoomName Room
type Error a = Either String a

type Point = (Int, Int)
type GameGrid = M.Map Point Room

data GameState
    = GameState {
        message :: Maybe String
        , gmap :: GameMap
        , universe :: Universe
        , player ::Player
        , grid :: GameGrid
        } deriving Show

data KeyError = KeyError
  deriving Show

instance Exception KeyError

-- makes game map from list of rooms
mkMap :: [Room] -> GameMap
mkMap = M.fromList.convertRoom

-- making a game map from all rooms list
gameMap :: GameMap
gameMap = mkMap allRooms

-- making room list into list so it can be used in game map
convertRoom :: [Room] -> [(RoomName,Room)]
convertRoom [room] = [(rname room,room)]
convertRoom lst =
    let hd = head lst
        tl = tail lst
    in (rname hd, hd) : convertRoom tl

-- initializing game state
initialState :: GameState
initialState = GameState Nothing gameMap univ you gameGrid

-- helper function to get object
getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

-- get an object from the universe
getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

-- helper function to get room
getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

-- get a room from the game map
getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

-- 3.3.1 Exercise: setRoomMap
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap = M.insert

-- 3.3.2 Exercise: setMessage
setMessage :: String -> GameState -> GameState
setMessage "" gs = gs {message = Nothing}
setMessage newMsg gs = gs {message = Just newMsg}

-- 3.3.3 Exercise: currentInventory
currentInventory :: GameState -> [ItemName]
currentInventory gs = inventory $player gs

-- 3.3.4 Exercise: currentRoom
currentRoom :: GameState -> Room
currentRoom gs = let roomName = location $player gs
                  in getRoom roomName gs

-- 3.3.5 Exercise: nearbyObjects
nearbyObjects :: GameState -> [ItemName]
nearbyObjects gs = objects $currentRoom gs

-- -- 3.2.1 Exercise: takeItem Revisited
takeItem :: ItemName -> GameState -> GameState
takeItem iname gs = case alreadyHaveTakeCheck iname gs of
                        Left err -> gs {message = Just err}
                        Right gs -> case inRoomTakeCheck iname gs of
                            Left err -> gs {message = Just err}
                            Right gs -> case weightCheck iname gs of
                                Left err -> gs {message = Just err}
                                Right gs -> gs {message = Just
                                ("You took the item "++ show iname)
                                    , player = Player.addItem iname (player gs)
                                    , gmap = setRoomMap (rname $currentRoom gs)
                                    (Room.removeItem iname $currentRoom gs)
                                    (gmap gs)
                                                }

-- 3.2.2 Exercise: dropItem Revisited
dropItem :: ItemName -> GameState -> GameState
dropItem iname gs = case anywhereDropCheck iname gs of
                        Left err -> gs {message = Just err}
                        Right gs -> case inRoomDropCheck iname gs of
                            Left err -> gs {message = Just err}
                            Right gs -> gs {message = Just
                            ("You dropped the item "++ show iname)
                            , player = Player.removeItem iname (player gs)
                            , gmap = setRoomMap (rname $currentRoom gs)
                            (Room.addItem iname $currentRoom gs) (gmap gs)
                            }

-- Assignment 4 
-- 3.0.1 Exercise: inventoryWeight
inventoryWeight :: GameState -> Integer
inventoryWeight gs = getTotalItemsWeight (inventory$ player gs) gs

-- it calulates the total weight of the items in the given list
getTotalItemsWeight :: [ItemName] -> GameState -> Integer
getTotalItemsWeight [] gs = 0
getTotalItemsWeight [x] gs = weight (getObject x gs)
getTotalItemsWeight (x:xs) gs = weight (getObject x gs) + getTotalItemsWeight xs gs

-- 3.1.1 Exercise: alreadyHaveTakeCheck
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck iname gs =
        if iname `elem` inventory (player gs)
            then Left ("you are carrying " ++ show iname)
            else Right gs

-- 3.1.2 Exercise: inRoomTakeCheck
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck iname gs =
        if iname `elem` objects (currentRoom gs)
            then Right gs
            else Left ("There is no " ++ show iname ++ " in this room.")

-- 3.1.3 Exercise: weightCheck
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck iname gs =
        let objWeight = weight $ getObject iname gs
        in if objWeight + inventoryWeight gs <= maxWeight (player gs)
            then Right gs
            else Left "Too much weight can't carry Item"

-- 3.1.4 Exercise: anywhereDropCheck
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck iname gs =
    if iname `elem` inventory (player gs)
        || iname `elem` objects (currentRoom gs)
        then Right gs
        else Left ("What do you mean DROOOOPPP  the " ++ show iname)

-- 3.1.5 Exercise: inRoomDropCheck
inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck iname gs =
        if iname `elem` objects (currentRoom gs)
            then Left ("You are not carrying "++ show iname)
            else Right gs


-- 3.3.2 Exercise: roomHasObjects
roomHasObjects :: GameState -> Bool
roomHasObjects gs = hasObjects $ currentRoom gs

-- 3.3.3 Exercise: destinationName
destinationName :: Direction -> Room -> Maybe RoomName
destinationName dir room = lookup dir (exits room)

-- 3.3.4 Exercise: move
move :: Direction -> GameState -> GameState
move dir gs = case destinationName dir (currentRoom gs) of
        Nothing  -> gs {
            message = Just "There is no exit where you are trying to go"
            }
        Just destination -> gs {
            message = Just ("You are going the Direction "++ show dir)
        ,player = Player.newLocation destination (player gs)}

-- list of items you need to take to win
itemsToWin :: [ItemName]
itemsToWin = [WoodWicket,SteelWicket ,PerspexWicket ,GoldBails ,SilverBails]

-- checks if u have won the game
haveWonGame :: GameState -> Bool
haveWonGame gs = rname (currentRoom gs) == ImprobabiltyShip

haveUnlockedSecretRoom :: GameState -> Bool
haveUnlockedSecretRoom gs = all (`elem` inventory( player gs)) itemsToWin

-- exampple game grid  
gameGrid :: GameGrid
gameGrid = M.fromList [((0,2),gryffindorCommons)
                      ,((1,2),hogwartsLibrary)
                      ,((0,3),hagridHut)
                      ,((0,1),chamberOfSecrets)
                      ,((1,0),gringottsBank )
                      ,((0,0),greatHall )
                      ,((1,3),diagonAlley )
                      ,((1,1),azkaban)]

-- this function helps in displaying the exits of a room
displayExit :: Direction -> Room -> String
displayExit dir room = case destinationName dir room of
                            Nothing -> " "
                            Just _ -> case dir of
                                N -> "|"
                                S -> "|"
                                E -> "-"
                                W -> "-"

-- printing the name of the room in map
roomLetter :: Maybe RoomName -> Room -> String
roomLetter Nothing room = showRoomName room
roomLetter (Just givenRoomName) room = if givenRoomName == rname room
                                        then "*"
                                        else showRoomName room
-- helper function to display the name of the room(roomLetter)
showRoomName :: Room -> String
showRoomName room = let roomName = show (rname room)
                    in [head roomName,
                        head (tail roomName),
                        head (tail (tail roomName))]

-- generates a list of list of rooms which we will be using in
-- displaying the map
showGridList :: GameGrid -> [[Room]]
showGridList gg  =  [M.elems $ filterWithKey (\(x,y) room -> x==0) gg]
                    ++ [M.elems $ filterWithKey (\(x,y) room -> x==1) gg]
                    ++ [M.elems $ filterWithKey (\(x,y) room -> x==2) gg]
                    ++ [M.elems $ filterWithKey (\(x,y) room -> x==3) gg]

-- displays a single row of the map
showRow :: Maybe RoomName -> [Room] -> String
showRow rname row = foldl (\acc room ->
                            acc
                            ++ "  "
                            ++ displayExit N room
                            ++"  " )
                            "" row
                    ++ "\n"
                    ++ foldl (\acc room -> acc
                            ++( displayExit W room
                            ++ roomLetter rname room
                            ++ displayExit E room)) "" row
                    ++ "\n"
                    ++ foldl (\acc room ->
                            acc
                            ++ "  "
                            ++ displayExit S room
                            ++ "  ")
                             "" row
                    ++ "\n"

-- shows the map using show row
showGrid :: Maybe RoomName -> GameGrid -> String
showGrid rname gg = foldl (\acc row -> acc
                            ++ showRow rname row
                            ++ "\n")
                            "" (showGridList gg)
                            ++ "\n"

-- type GameGrid = M.Map Point Room
-- find the location of the room in the map
findRoomLocation :: RoomName -> [(Point,Room)] -> Maybe Point
findRoomLocation roomName [] = Nothing
findRoomLocation roomName ((point,room):xs) = if roomName == rname room
                                               then Just point
                                               else
                                                   findRoomLocation
                                                    roomName
                                                    xs


addRoomToGrid :: RoomName -> Room -> GameGrid -> GameGrid
addRoomToGrid roomName room gg = case
                                findRoomLocation HagridHut (M.toList gg) of
                                Just loc -> M.insert
                                    loc room gg
                                Nothing -> gg

-- bottom right of the map
bottomRight :: Point
bottomRight = (2,1)

-- position for secret room
secretPosition :: Point
secretPosition = (2,2)

-- 
addSecretRoomToGameState ::GameState ->  GameState
addSecretRoomToGameState gs = let finalGrid = addSecretRoomToGrid( grid gs)
                        in gs{ grid = finalGrid
                               ,gmap = gridToMap finalGrid}


-- converts a game grid to a gameMap
gridToMap :: GameGrid -> GameMap
gridToMap gg = M.fromList (map (\x -> (rname x, x)) (M.elems gg))


-- type GameGrid = M.Map Point Room
-- adds secret room to the map
addSecretRoomToGrid :: GameGrid -> GameGrid
addSecretRoomToGrid gg  = let bottomRightRoom = M.lookup bottomRight gg
                        in case bottomRightRoom of
                            Just room ->
                                M.insert
                                secretPosition
                                (addExitToSecretRoom room)
                                (replaceRoomWithSecretExit gg)
                            Nothing -> gg

-- adds the exit to the secret room ( helper for addSecretRoomToGrid)
replaceRoomWithSecretExit ::  GameGrid -> GameGrid
replaceRoomWithSecretExit gg = let bottomRightRoom = M.lookup bottomRight gg
                            in case bottomRightRoom of
                               Just room -> M.insert bottomRight
                                            (addSecretExitToRoom room) gg
                               Nothing -> gg

-- helper for addSecretRoomToGrid
addSecretExitToRoom :: Room -> Room
addSecretExitToRoom room = room {exits = exits room ++ [(E,ImprobabiltyShip)]}

-- helper for addSecretRoomToGrid
addExitToSecretRoom :: Room -> Room
addExitToSecretRoom room = let exitLst = exits improbabiltyShip
                            in improbabiltyShip {exits = exitLst
                                                    ++ [(W,rname room)]}

-- god mode function
godMode :: GameState -> GameState
godMode gs = gs {
    player = (player gs) {
        inventory = [WoodWicket,SteelWicket ,PerspexWicket ,GoldBails ,SilverBails]
        }
    }



