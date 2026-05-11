-- Black Jack Game



-- Game Entity Declaration

-- The suit of a card
data Suit = Hearts | Spades | Diamonds | Clubs
  deriving (Eq, Show)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Eq, Show)

-- A card has a rank and a suit
data Card = Card Rank Suit
  deriving (Eq, Show)

-- A hand is a list of cards
type Hand = [Card]

-- The possible players
data Player = Bank | Guest
  deriving (Eq, Show)




-- Game Functionality

-- 1. faceCards
faceCards :: Hand -> Integer
faceCards [] = 0
faceCards (Card rank _ : xs)
    | rank == Jack || rank == Queen || rank == King = 1 + faceCards xs
    | otherwise = faceCards xs


-- We gotta make tis better on gawd
-- If anyone wanna work on tis function go for it, it works but too long, tested til nr 4 and they work, havent tested the rest.
-- 2. value
value :: Hand -> Integer
value [] = 0
value (Card Ace _ : xs)
    | 11 + value xs > 21 = 1 + value xs
    | otherwise = 11 + value xs
value (Card Two _ : xs) = 2 + value xs
value (Card Three _ : xs) = 3 + value xs
value (Card Four _ : xs) = 4 + value xs
value (Card Five _ : xs) = 5 + value xs
value (Card Six _ : xs) = 6 + value xs
value (Card Seven _ : xs) = 7 + value xs
value (Card Eight _ : xs) = 8 + value xs
value (Card Nine _ : xs) = 9 + value xs
value (Card Ten _ : xs) = 10 + value xs
value (Card rank _ : xs)
    | rank == Jack || rank == Queen || rank == King = 10 + value xs



-- 3. isBlackjack
isBlackjack :: Hand -> Bool
isBlackjack [a,b] = value [a,b] == 21
isBlackjack _ = False



-- 4. gameOver
gameOver :: Hand -> Bool
gameOver hand = value hand > 21



-- 5. winner
winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest = Bank
  | gameOver bank = Guest
  | value guest > value bank = Guest
  | otherwise = Bank



-- 6. <+
(<+) :: Hand -> Hand -> Hand
[] <+ ys = ys
(x:xs) <+ ys = x : (xs <+ ys)



-- 7. handSuit
handSuit :: Suit -> Hand
handSuit suit = [ Card Ace suit , Card Two suit, Card Three suit, Card Four suit, Card Five suit, Card Six suit, Card Seven suit, Card Eight suit, Card Nine suit, Card Ten suit, Card Jack suit, Card Queen suit, Card King suit]



-- 8. belongsTo
belongsTo :: Card -> Hand -> Bool
belongsTo _ [] = False
belongsTo card (x:xs)
  | card == x = True
  | otherwise = belongsTo card xs



-- 9. fullDeck
fullDeck :: Hand
fullDeck =
  handSuit Hearts
  <+ handSuit Spades
  <+ handSuit Diamonds
  <+ handSuit Clubs



-- 10. draw
draw :: Hand -> Hand -> (Hand, Hand)
draw [] hand = error "Deck is empty"
draw (x:xs) hand = (xs, x : hand)



-- 11. playBank
playBank :: Hand -> Hand -> Hand
playBank deck hand
  | value hand < 16 =
      playBank newDeck newHand
  | otherwise = hand
  where
    (newDeck, newHand) = draw deck hand