module Game.Deck (
    createDeck,
    shuffleDeck,
    cardCount,
    Deck,
    Card
) where

import System.Random.Shuffle

data Card = Spade Int | Club Int | Heart Int | Diamond Int 
    deriving Show

data Deck a = Deck [Card]
    deriving Show

createDeck :: Deck a
createDeck = 
    let range = [1..13]
    in Deck $ [Spade number| number <- range] ++
        [Club number| number <- range]++
        [Heart number| number <- range]++
        [Diamond number| number <- range]

shuffleDeck :: Deck a -> Deck a
shuffleDeck (Deck cards) =
    Deck $ shuffle cards [1..52]

cardCount :: Deck a -> Int
cardCount (Deck cards) = 
    length cards
