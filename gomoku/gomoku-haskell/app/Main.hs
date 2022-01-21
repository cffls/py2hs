module Main where

import Gomoku

main :: IO ()
main = do
  playGame $ createGame 15