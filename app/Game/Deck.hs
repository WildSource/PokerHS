module Game.Deck (
    createDeck,
    shuffleDeck,
) where


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

shuffleDeck :: Deck [Card] -> Int -> Deck [Card]
shuffleDeck deck 0 = deck
shuffleDeck deck shuffleCount =
    shuffleDeck (riffleShuffle deck) $ shuffleCount - 1    

riffleShuffle :: Deck [Card] -> Deck [Card]
riffleShuffle deck =
   shuffle $ splitDeck deck 
    
splitDeck :: Deck a -> ([Card],[Card])
splitDeck (Deck cards) = 
    splitAt (middleIndex cards) cards
    where
        middleIndex :: [Card] -> Int 
        middleIndex cardList = 
            div (cardCount cardList) 2

shuffle :: ([Card],[Card]) -> Deck [Card]
shuffle (firstHalf,secondHalf) =
  Deck $ concat [[card1, card2] | card1 <- firstHalf, card2 <- secondHalf]     

cardCount :: [Card] -> Int
cardCount cards = 
    length cards
