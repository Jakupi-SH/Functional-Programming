-- Black Jack Game


-- Game Entity Declaration

data Suit = Hearts | Spades | Diamonds | Clubs
  deriving (Eq, Show)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King -- these are translated into Integers later
  deriving (Eq, Show)

data Card = Card Rank Suit
  deriving (Eq, Show)

type Hand = [Card]

data Player = Bank | Guest
  deriving (Eq, Show)




-- Helper Functions

rankValue :: Rank -> Integer -- Translates the Rank of a card into its Integer value
rankValue Ace   = 1
rankValue Two   = 2
rankValue Three = 3
rankValue Four  = 4
rankValue Five  = 5
rankValue Six   = 6
rankValue Seven = 7
rankValue Eight = 8
rankValue Nine  = 9
rankValue Ten   = 10
rankValue Jack  = 10
rankValue Queen = 10
rankValue King  = 10


countAces :: Hand -> Integer -- Counts the number of Aces in a Hand, which is important for calculating the value of a hand since Aces can be worth either 1 or 11
countAces [] = 0
countAces (Card Ace _ : xs) = 1 + countAces xs
countAces (_ : xs) = countAces xs


baseValue :: Hand -> Integer -- Calculates the base value of a Hand by summing the rank values of all cards, treating Aces as 1
baseValue [] = 0
baseValue (Card rank _ : xs) =
  rankValue rank + baseValue xs


upgradeAces :: Integer -> Integer -> Integer -- Upgrades the value of Aces from 1 to 11 as long as it does not cause the hand value to exceed 21
upgradeAces total 0 = total
upgradeAces total aces
  | total + 10 <= 21 = upgradeAces (total + 10) (aces - 1)
  | otherwise        = total




-- Game Functionality

-- 1. faceCards
faceCards :: Hand -> Integer
faceCards [] = 0
faceCards (Card rank _ : xs)
  | rank == Jack || rank == Queen || rank == King = 1 + faceCards xs -- Checks if a card is a face card then returns the number of times they apfupear 
  | otherwise = faceCards xs


-- 2. value
value :: Hand -> Integer
value hand =
  upgradeAces (baseValue hand) (countAces hand)


-- 3. isBlackjack
isBlackjack :: Hand -> Bool
isBlackjack hand = length hand == 2 && value hand == 21 -- Checks if a hand of two cards is equal to 21 (an ace and a rank value of 10)


-- 4. gameOver
gameOver :: Hand -> Bool
gameOver = (>21) . value -- Checks if the Hand value surpasses 21


-- 5. winner
winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest = Bank -- Checks whether one, or both guest and bank have surpassed 21, if not the greater value wins
  | gameOver bank = Guest
  | value guest > value bank = Guest
  | otherwise = Bank


-- 6. <+
(<+) :: Hand -> Hand -> Hand
[] <+ ys = ys
(x:xs) <+ ys = x : (xs <+ ys) -- returns an append (the union of both hands) of the the two Hands given


-- 7. handSuit
handSuit :: Suit -> Hand
handSuit suit =
  [ Card Ace suit, Card Two suit, Card Three suit, Card Four suit, Card Five suit, Card Six suit, Card Seven suit, Card Eight suit, Card Nine suit, Card Ten suit, Card Jack suit, Card Queen suit, Card King suit ] -- Returns all Card Ranks of a specific Suit


-- 8. belongsTo
belongsTo :: Card -> Hand -> Bool
belongsTo _ [] = False
belongsTo card (x:xs)
  | card == x = True -- Checks whether a card is the same as a card in a given Hand
  | otherwise = belongsTo card xs


-- 9. fullDeck
fullDeck :: Hand
fullDeck =
  handSuit Hearts
  <+ handSuit Spades
  <+ handSuit Diamonds
  <+ handSuit Clubs -- Returns all cards in a standard deck


-- 10. draw
draw :: Hand -> Hand -> (Hand, Hand)
draw [] hand = error "Deck is empty"
draw (x:xs) hand = (xs, x : hand) -- Adds one Card from a Hand to another simulating drawing a card from a deck


-- 11. playBank
playBank :: Hand -> Hand -> Hand
playBank deck hand
  | value hand < 16 = playBank newDeck newHand -- Check if the current hand value is less than 16
  | otherwise       = hand
  where
    (newDeck, newHand) = draw deck hand -- Draws a card from the deck hand if value is less than 16