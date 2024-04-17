module Main where

import Game.Deck

main :: IO ()
main = do
    print $ shuffleDeck createDeck 6
