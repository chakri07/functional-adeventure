module Room where
import Item
import Direction
import Data.List as Dl
import Item (ItemName(SuperComputer))

data RoomName
  = GryffindorCommons
  | HogwartsLibrary
  | HagridHut
  | ChamberOfSecrets
  | GreatHall
  | Azkaban
  | GringottsBank
  | DiagonAlley
  | ImprobabiltyShip
  deriving ( Eq, Ord)

instance Show RoomName where
  show GryffindorCommons = "Gryffindor Commons"
  show HogwartsLibrary = "Hogwarts Library"
  show HagridHut = "Hagrid's Hut"
  show ChamberOfSecrets = "Chamber Of Secrets"
  show GreatHall = "Great Hall"
  show Azkaban = "Azkaban"
  show GringottsBank = "Gringotts Bank"
  show DiagonAlley = "Diagon Alley"
  show ImprobabiltyShip = "Improbabilty Ship"

type Exit = (Direction, RoomName)

data Room
    = Room {
        rname:: RoomName
        ,desc :: String
        ,exits ::[Exit]
        , objects :: [ItemName]
    } deriving(Show,Eq)

-- 9 rooms defined starting with Gryffindor Commons
gryffindorCommons :: Room
gryffindorCommons = Room GryffindorCommons 
                    "You are in GryffindorCommons, The place where it all\ 
                    \ began" 
                    [ (S ,HagridHut) 
                    , (N,ChamberOfSecrets)
                    , (E,HogwartsLibrary)] 
                    [SteelWicket,GobletOfFire]

hogwartsLibrary :: Room
hogwartsLibrary = Room HogwartsLibrary 
                  "You are in Hogwarts library, The home for the infinite\
                  \ amount of wizard knoledge" 
                  [(W,GryffindorCommons)] 
                  [GauntRing,ElderWand]

hagridHut :: Room
hagridHut = Room HagridHut 
            "You are in Hagrid's Hut, The place to all the crazy stuff\
            \that happens in the forest" 
            [(N,GryffindorCommons)
            ,(E,DiagonAlley)] 
            [SilverBails,FirePotion]

chamberOfSecrets :: Room
chamberOfSecrets = Room ChamberOfSecrets 
                   "You are in the Chamber of Secrets, This is tom riddle's \
                   \ secret place" 
                   [(S,GryffindorCommons)
                   ,(N ,GreatHall)
                   ,(E,Azkaban)] 
                   [GoldBails,TomRiddleDairy]

greatHall :: Room
greatHall = Room GreatHall 
            "You are in great hall, FOOOOOOODDDDDD" 
            [(S,ChamberOfSecrets)
            ,(E,GringottsBank)] 
            [PerspexWicket,SortingHat]

azkaban :: Room
azkaban = Room Azkaban 
          "You are in Azkaban \
          \Prison for the most dangerouts criminals" 
          [(W,ChamberOfSecrets)] 
          [WoodWicket,ResurrectionStone]

gringottsBank :: Room
gringottsBank = Room GringottsBank 
                " You are in the Gringotts Bank, \
                \The only wizarding bank in London" 
                [(W,GreatHall)] 
                [CloakOfInvisibility,KnutsHeap]

diagonAlley :: Room
diagonAlley = Room DiagonAlley 
              "You are in Diagon Alley, \
              \ Wizard Shopping Street, You can find anything the heart \ 
              \desires" 
              [(W,HagridHut)] 
              [QuidditchBroom,ButterBeer]

improbabiltyShip :: Room
improbabiltyShip = Room ImprobabiltyShip 
                   "You are in Improbability ship\
                   \ The ship that helps you to get out of the place" 
                   [] 
                   [SuperComputer]

-- list of all room names
roomNames :: [RoomName]
roomNames = map rname allRooms

-- add items to room
addItem :: ItemName -> Room -> Room
addItem iname room = let items = objects room
                    in room {objects = iname:items}

-- remove items from room
removeItem :: ItemName -> Room -> Room
removeItem iname room = room {objects =
                                Dl.delete iname (objects room)}

-- list of all rooms
allRooms :: [Room]
allRooms = [gryffindorCommons
            ,hogwartsLibrary
            ,hagridHut
            ,chamberOfSecrets
            ,greatHall
            ,azkaban
            ,gringottsBank
            ,diagonAlley]

-- 3.3.1 Exercise: hasObjects
hasObjects :: Room -> Bool
hasObjects rname = objects rname /= []
