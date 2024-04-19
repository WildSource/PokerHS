module Main where

import Game.Deck

main :: IO ()
main = do
    let deck = createDeck 
    print $ shuffleNTime (riffleShuffle) deck 1
