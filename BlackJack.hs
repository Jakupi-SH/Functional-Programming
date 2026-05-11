-- Black Jack Game


-- Game Entity Declaration
-- The suit of a card
data Suit = Hearts | Spades | Diamonds | Clubs
  deriving (Eq, Show)

-- The rank of a card
data Rank = Ace | Num Int | Jack | Queen | King
  deriving (Eq, Show)

-- A card has a rank and a suit
data Card = Card Rank Suit
  deriving (Eq, Show)

-- A hand is a list of cards.
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



-- 2. value
value :: Hand -> Integer
value [] = 0
value (Card Ace _ : xs)
    | 11 + value xs > 21 = 1 + value xs
    | otherwise = 11 + value xs
value (Card (Num n) _ : xs) = fromIntegral n + value xs
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
handSuit suit =
  [ Card Ace suit, Card (Num 2) suit, Card (Num 3) suit, Card (Num 4) suit, Card (Num 5) suit, Card (Num 6) suit, Card (Num 7) suit, Card (Num 8) suit, Card (Num 9) suit, Card (Num 10) suit, Card Jack suit, Card Queen suit, Card King suit]



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