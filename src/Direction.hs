{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Direction where

data Direction = N | S | E | W
                deriving Eq

instance Show Direction where
    show dir 
        = case dir of 
             N  -> "north"
             S  -> "south"
             E  -> "east"
             W  -> "west"

-- changes a string to a direction
stringToDirection :: String -> Direction
stringToDirection "north" = N
stringToDirection "south" = S
stringToDirection "east"  = E
stringToDirection "west"  = W
