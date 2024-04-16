module Game.Deck (createDeck, Deck, Card) where

data Card = Spade Int | Club Int | Heart Int | Diamond Int 
    deriving Show

data Deck = Deck [Card]
    deriving Show

createDeck :: Deck
createDeck = 
    let range = [1..13]
    in Deck $ [Spade number| number <- range] ++
        [Club number| number <- range]++
        [Heart number| number <- range]++
        [Diamond number| number <- range]
    