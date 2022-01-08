module Item where

import qualified Data.Map as M
import Data.Map (keys)
import Data.Char (toLower)

data ItemName
  = SteelWicket
  | WoodWicket
  | PerspexWicket
  | GoldBails
  | SilverBails
  | TomRiddleDairy
  | GauntRing  
  | GobletOfFire
  | ElderWand
  | CloakOfInvisibility
  | ResurrectionStone
  | QuidditchBroom
  | ButterBeer
  | FirePotion
  | SortingHat
  | KnutsHeap
  | SuperComputer
  deriving (Eq, Ord)

type Universe = M.Map ItemName Item

data Item
    = Item {name:: ItemName
    , weight :: Integer}
    deriving(Show,Eq)

-- Item names 
instance Show ItemName where
    show SteelWicket = "steel Wicket"
    show WoodWicket = "wood Wicket"
    show PerspexWicket = "perspex Wicket"
    show GoldBails = "gold Bails"
    show SilverBails = "silver Bails"
    show TomRiddleDairy = "tom Riddle Dairy"
    show GauntRing   = "gaunt Ring"
    show GobletOfFire = "goblet Of Fire"
    show ElderWand = "elder Wand"
    show CloakOfInvisibility = "cloak Of Invisibility"
    show ResurrectionStone = "resurrection Stone"
    show QuidditchBroom = "quidditch Broom"
    show ButterBeer = "butter Beer"
    show FirePotion = "fire Potion"
    show SortingHat = "sorting Hat"
    show KnutsHeap = "knuts"
    show SuperComputer = "super computer"

-- converts string to ItemName
stringToItemName :: String -> ItemName
stringToItemName "steel Wicket" = SteelWicket
stringToItemName "wood Wicket" = WoodWicket
stringToItemName "perspex Wicket" = PerspexWicket
stringToItemName "gold Bails" = GoldBails
stringToItemName "silver Bails" = SilverBails
stringToItemName "tom Riddle Dairy" = TomRiddleDairy
stringToItemName "gaunt Ring  " = GauntRing  
stringToItemName "goblet Of Fire" = GobletOfFire
stringToItemName "elder Wand" = ElderWand
stringToItemName "cloak Of Invisibility" = CloakOfInvisibility
stringToItemName "resurrection Stone" = ResurrectionStone
stringToItemName "quidditch Broom" = QuidditchBroom
stringToItemName "butter Beer" = ButterBeer
stringToItemName "fire Potion" = FirePotion
stringToItemName "sorting Hat" = SortingHat
stringToItemName "knuts" = KnutsHeap
stringToItemName "super computer" = SuperComputer
stringToItemName _ = error "Please enter a valid item name"

-- Items from here on
steelWicket :: Item
steelWicket = Item SteelWicket 35

woodWicket :: Item
woodWicket = Item WoodWicket 30

perspexWicket :: Item
perspexWicket = Item PerspexWicket 45

goldBails :: Item
goldBails = Item GoldBails 15

silverBails :: Item
silverBails = Item SilverBails 10

tomRiddleDairy :: Item
tomRiddleDairy = Item TomRiddleDairy 9

gauntRing   :: Item
gauntRing   = Item GauntRing 2

gobletOfFire :: Item
gobletOfFire = Item GobletOfFire 50

elderWand :: Item
elderWand = Item ElderWand 5

cloakOfInvisibility :: Item
cloakOfInvisibility = Item CloakOfInvisibility 35

resurrectionStone :: Item
resurrectionStone = Item ResurrectionStone 50

quidditchBroom :: Item
quidditchBroom = Item QuidditchBroom 35

butterBeer :: Item
butterBeer = Item ButterBeer 10

firePotion :: Item
firePotion = Item FirePotion 20

sortingHat :: Item
sortingHat = Item SortingHat 10

knuts :: Item
knuts = Item KnutsHeap 18

superComputer :: Item
superComputer = Item SuperComputer 1000


-- make universr from item list
mkUniverse :: [Item] -> Universe
mkUniverse = M.fromList.convert

-- make a small universe with all items
univ :: Universe
univ = mkUniverse[goldBails,steelWicket,silverBails,perspexWicket,woodWicket,gobletOfFire,elderWand,tomRiddleDairy,gauntRing,cloakOfInvisibility,resurrectionStone]

-- list of the item names
itemNames :: [ItemName]
itemNames = keys univ

-- convert a list of items to a list of (itemName,item) pairs to be used in 
-- map that can be used when needed.
convert :: [Item] -> [(ItemName,Item)]
convert [Item x y] =
    let item = Item x y
    in [(name item,item)]
convert lst =
    let hd = head lst
        tl = tail lst
    in (name hd, hd) : convert tl