module Main where


import Control.Monad.State

import GameIO
import GameState
import Example as E

-- driver function of the game
main :: IO ()
main = do
  state <- example :: IO GameState
  evalStateT opening state 
  evalStateT (forever  repl) state