module Game.Deck (
    createDeck,
    riffleShuffle,
    shuffleNTime
) where

data Card = Spade Int | Club Int | Heart Int | Diamond Int 
    deriving Show

data Deck a = Deck [Card]
    deriving Show

createDeck :: Deck [Card]
createDeck = 
    let range = [1..13]
    in Deck $ [Spade number| number <- range] ++
        [Club number| number <- range]++
        [Heart number| number <- range]++
        [Diamond number| number <- range]

shuffleNTime :: (Deck [Card] -> Deck [Card]) -> Deck [Card] -> Int -> Deck [Card]
shuffleNTime _ deck 0 = deck 
shuffleNTime f deck n =
    f $ shuffleNTime f deck $ n - 1 

riffleShuffle :: Deck [Card] -> Deck [Card]
riffleShuffle deck =
   shuffle $ splitDeck deck 

shuffle :: ([Card],[Card]) -> Deck [Card]
shuffle (firstHalf, secondHalf) =
  Deck $ [z | (x, y) <- zip firstHalf secondHalf, z <- [x, y]]    

splitDeck :: Deck a -> ([Card],[Card])
splitDeck (Deck cards) = 
    splitAt (middleIndex cards) cards
    where
        middleIndex :: [Card] -> Int 
        middleIndex cardList = 
            div (cardCount cardList) 2

cardCount :: [Card] -> Int
cardCount cards = 
    length cards

cardCount' :: Deck [Card] -> Int
cardCount' (Deck cards) = 
    length cards
