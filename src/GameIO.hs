module GameIO where

import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Player
import Room
import Command
import System.Exit (exitSuccess)
import Direction
import Item
import Data.Map as M
import qualified Control.Applicative as Maybe



type GameIO a = StateT GameState IO a

-- modifying game state monad
effectChange :: (GameState -> GameState) -> GameIO ()
effectChange transition = do
                            stateBefore <- get
                            put $ transition stateBefore
-- promt in the repl
prompt :: GameIO ()
prompt = lift $ putStr "-> " >> hFlush stdout

-- printing the message from game state
printMessage :: GameIO ()
printMessage = do
                state <- get
                case message state of
                    Just x -> lift (putStrLn x) >> effectChange (setMessage "")
                    Nothing -> lift $ pure ()

-- print the room description
printDescription :: GameIO ()
printDescription = do
                    presentState <- get
                    lift (putStrLn $ desc $ currentRoom presentState)

-- print the room obejcts
printObjects :: GameIO ()
printObjects = do
            presentState <- get
            case nearbyObjects presentState of
                [] -> pure ()
                lst -> lift
                        $ putStrLn "You see the following objects:"
                    >> do putStrLn
                            $ unlines
                            $ Prelude.map show lst

-- Print the room exits
printExits :: GameIO ()
printExits = do
            presentState <- get
            case exits $ currentRoom presentState of
                [] -> pure ()
                lst -> lift
                        $ putStrLn "There are exits in the following directions:"
                    >> do putStrLn
                            $ unlines
                            $ Prelude.map (show . fst) lst

-- add max weight and remaining weight to the game state
-- 2.0.2 Exercise: printInventory
printInventory :: GameIO ()
printInventory = do
            presentState <- get
            case currentInventory presentState of
                [] -> lift $ putStrLn "You aren't carrying anything"
                lst -> lift
                        $ putStrLn "You have the following items:"
                    >> do putStrLn
                            $ unlines
                            $ Prelude.map show lst

-- 2.0.3 Exercise: actionOverList
actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
actionOverList action [] = pure ()
actionOverList action (x:xs) = do
                                presState <- get
                                put $ action x presState
                                printMessage
                                actionOverList action xs

-- 2.0.4 Exercise: finishGame
finishGame :: GameIO ()
finishGame = lift$  putStrLn "You Have escaped from this world Now on to the\ 
                    \Next one !!!!! Thanks for playing" >> exitSuccess

-- 2.0.5 Exercise: exit
exit :: GameIO ()
exit = lift$  putStrLn "Good bye !!" >> exitSuccess

-- 2.0.6 Exercise: checkGameOver
checkGameOver :: GameIO ()
checkGameOver = do
                presState <- get
                if haveWonGame presState
                    then finishGame
                    else pure ()

checkSecretRoomUnlocked :: GameIO ()
checkSecretRoomUnlocked = do
                        presState <- get
                        if haveUnlockedSecretRoom presState
                            then addSecretRoom
                            else pure()

-- 2.0.7 Exercise: syntaxError
syntaxError :: GameIO ()
syntaxError = lift $ putStrLn "I don't Understand that"

-- 2.0.8 Exercise: opening
opening :: GameIO ()
opening = lift $ putStrLn 
                "Hi! I am marvin the robot. Please help you to escape from this\
                \ world. I need to collect all the 5 items to escape from this\
                \ universe. Plese Help me\n"
                >> putStrLn "You need to collect the following items:"
                >> putStrLn "1. steel Wicket"
                >> putStrLn "2. silver Wicket"
                >> putStrLn "3. gold Wicket"
                >> putStrLn "4. gold Bails"
                >> putStrLn "5. silver Bails"
                
-- 2.0.9 Exercise: performCommand
performCommand :: Command -> GameIO ()
performCommand command = case command of
                            Look  -> performLookCommand
                            Take lst -> actionOverList takeItem lst
                            Drop lst -> actionOverList dropItem lst
                            Move dir  -> performMoveCommand dir
                            Inventory -> performInventoryCommand
                            Map -> displayMap
                            Exit -> exit
                            GodMode -> godModeActivate
-- helper function for performCommand to perform look command
performLookCommand :: GameIO()
performLookCommand = do
                        printDescription
                        printObjects
                        printExits

-- helper function for performCommand to perform move command
performMoveCommand :: Direction -> GameIO()
performMoveCommand dir = do
                        presState <- get
                        put $ move dir presState
                        printMessage

-- helper function for performCommand to perform Inventory command
performInventoryCommand :: GameIO()
performInventoryCommand = printInventory

-- helper function for performCommand to perform Map command
displayMap :: GameIO ()
displayMap = do
            presentState <- get
            lift $ putStrLn $ "You are in "
                ++show (rname (currentRoom presentState))
            lift $ putStr (showGrid (Maybe.pure (rname $ currentRoom presentState)) (grid presentState))

-- 2.0.10 Exercise: performConjunction
performConjunction :: Conjunction -> GameIO ()
performConjunction [] = pure ()
performConjunction (x:xs) = performCommand x >> performConjunction xs

-- 2.0.11 Exercise: parseConjunction
parseConjunction :: String -> GameIO ()
parseConjunction str = case parse conjunctionP str of
                        Left err -> syntaxError
                        Right conj -> performConjunction conj

-- 2.0.12 Exercise: repl
repl :: GameIO ()
repl = prompt
        >> do
            input <- lift getLine
            maybe syntaxError performConjunction (parseInput input)
            checkSecretRoomUnlocked
            checkGameOver
-- this add secret room to the map and updates all the required state
addSecretRoom :: GameIO()
addSecretRoom = do
                presentState <- get
                put $ addSecretRoomToGameState presentState
                if ImprobabiltyShip  `elem ` M.keys (gmap presentState)
                    then pure()
                    else lift $ putStrLn "You have found your improbability ship!! Whoo hoo!! Now go to it!! and Escape this universe!! See Map to know where to Find it"

-- function to activate god mode
godModeActivate :: GameIO ()
godModeActivate = do
                    presentState <- get
                    put $ godMode presentState
                    lift $ putStrLn "God Mode Activated"