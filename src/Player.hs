{-# OPTIONS_GHC -Wno-missing-fields #-}
module Player where

import Item
import Room

data Player
    = Player {
        inventory :: [ItemName ]
        , maxWeight :: Integer
        , location :: RoomName
        } deriving(Show,Eq)
-- example player
you ::Player
you = Player [] 300 GryffindorCommons 

-- add item to inventory
addItem :: ItemName -> Player -> Player
addItem iname playa = Player {inventory = iname : inventory playa
                                , maxWeight = maxWeight playa
                                , location = location playa
                            }

-- remove item from player
removeItem :: ItemName -> Player -> Player
removeItem iname playa = Player {inventory = removeItemFromInv 
                                                iname$inventory playa
                                , maxWeight = maxWeight playa
                                , location = location playa
                            }

-- remove item from inventory(helper for removeItem)
removeItemFromInv :: ItemName -> [ItemName] -> [ItemName]
removeItemFromInv iname = foldr (\x acc -> 
                                    if x == iname 
                                        then acc 
                                        else x : acc) []

-- override and place player in new room
newLocation :: RoomName -> Player -> Player
newLocation rname playa = Player {inventory = inventory playa
                                , maxWeight = maxWeight playa
                                , location = rname
                            }

