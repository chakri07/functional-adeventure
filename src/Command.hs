module Command where

import Text.Parsec hiding (parse, runParser, (<|>))
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Data.Char
import Data.List

import Item
import Direction

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Map
  | GodMode
  | Exit
  deriving (Eq, Show)

type Conjunction = [Command]



parse :: Parser a -> String -> Either ParseError a
parse prsr = P.parse prsr ""

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

-- used as a parser to grabspaces
grabSpaces = many1 $ satisfy (`elem` " \r\t\n")

-- -- item name parser 
itemNameP :: Parser ItemName
itemNameP = stringToItemName <$>
        ((P.string "steel Wicket" <* (P.eof <|> P.optional grabSpaces)) 
        <|> (P.string "wood Wicket" <* (P.eof <|> P.optional grabSpaces))
        <|> (P.string "perspex Wicket" <* (P.eof <|> P.optional grabSpaces))
        <|> (P.string "gold Bails" <* (P.eof <|> P.optional grabSpaces))
        <|> (P.string "silver Bails" <* (P.eof <|> P.optional grabSpaces))
        <|> (P.string "tom Riddle Dairy" <* (P.eof <|> P.optional grabSpaces))
        <|> (P.string "gaunt ring" <* (P.eof <|> P.optional grabSpaces))
        <|> (P.string "goblet Of Fire" <* (P.eof <|> P.optional grabSpaces))
        <|> (P.string "elder Wand" <* (P.eof <|> P.optional grabSpaces))
        <|> (P.string "cloak Of Invisibility" <* (P.eof <|> P.optional grabSpaces))
        <|> (P.string "resurrection Stone" <* (P.eof <|> P.optional     grabSpaces))
        )


nounPhrase_stub :: Parser [ItemName]
nounPhrase_stub = many1 itemNameP

-- 2.0.8 Exercise: nounPhrase revisited
nounPhrase :: Parser [ItemName]
nounPhrase = P.sepBy1 itemNameP (P.optional grabSpaces 
                                  *> P.string "," 
                                  *> P.optional grabSpaces)

inventoryP :: Parser Command
inventoryP =  Inventory <$ (P.optional grabSpaces 
                            <* P.string "inventory" 
                            *> P.optional  grabSpaces)

-- 1.6.3 Exercise: takeP
takeP :: Parser Command
takeP = Take <$> (P.optional grabSpaces *>
                    P.string "take"
                    *> P.string " "
                    *> P.optional grabSpaces
                    *> nounPhrase
                    <* P.optional grabSpaces)

-- 1.6.4 Exercise: exitP
exitP :: Parser Command
exitP = Exit <$  ((P.optional grabSpaces 
                    *> P.string "exit" 
                    *> P.optional grabSpaces)
                <|> (P.optional grabSpaces 
                      *> P.string "quit" 
                      *> P.optional grabSpaces))


-- 2.0.1 Exercise: dropP
dropP :: Parser Command
dropP = Drop <$> (P.optional grabSpaces *>
                    P.string "drop"
                    *> P.string " "
                    *> P.optional grabSpaces
                    *> nounPhrase
                    <* P.optional grabSpaces)

-- 2.0.2 Exercise: lookP
lookP :: Parser Command
lookP = Look <$ (P.optional grabSpaces
                *> P.string "look"
                *> P.optional grabSpaces)

-- 2.0.3 Exercise: direction
directionP :: Parser Direction
directionP =Direction.stringToDirection <$>
            (P.optional grabSpaces *>
            (P.string "north"
            <|> P.string "south"
            <|> P.string "east"
            <|> P.string "west")
            <* P.optional grabSpaces)

-- 2.0.4 Exercise: moveP
moveP :: Parser Command
moveP = Move <$> directionP

mapP :: Parser Command
mapP = Map <$ (P.optional grabSpaces
                *> P.string "map" 
                *> P.optional grabSpaces *> P.eof)

godModeP :: Parser Command
godModeP = GodMode <$ (P.optional grabSpaces
                *> P.string "god"  
                *> P.optional grabSpaces *> P.eof)

-- 2.0.5 Exercise: commandP
commandP :: Parser Command
commandP = inventoryP
            <|> lookP
            <|> takeP
            <|> dropP
            <|> moveP
            <|> mapP
            <|> exitP
            <|> godModeP

-- 2.0.6 Exercise: conjunctionP
conjunctionP :: Parser Conjunction
conjunctionP = P.sepBy1 (P.optional grabSpaces *> commandP) 
                        (P.optional grabSpaces *> 
                        P.string "and" *> 
                        P.optional grabSpaces) 
                        <* P.eof

-- this is the main parser that will parse the whole command
parseInput :: String -> Maybe Conjunction
parseInput str = case parse conjunctionP str of
                    Left _ -> Nothing
                    Right parsed -> Just parsed

