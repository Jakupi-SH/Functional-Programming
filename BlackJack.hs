-- Black Jack Game

-- Problem List
-- Currently the Ace transormation into a 1 if the total value is more than 21 only works if the Ace is in the first position, or if there are more than 2 cards and all are aces. Not sure if we can use helpers yet (would probably be able to fix with them), and we cannot use imports or lists. This causes problems on all functions that use values of a Hand.



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





-- Game Functionality

-- 1. faceCards
faceCards :: Hand -> Integer
faceCards [] = 0
faceCards (Card rank _ : xs)
    | rank == Jack || rank == Queen || rank == King = 1 + faceCards xs -- Checks if a card is a face card then returns the number of times they appear 
    | otherwise = faceCards xs


-- 2. value
value :: Hand -> Integer
value [] = 0
value (Card Ace _ : xs)
    | 11 + rest > 21 = 1  + rest
    | otherwise      = 11 + rest -- Checks whether an Ace raises the Hand value over 21, if yes, Ace = 1
    where rest = value xs
value (Card Two _ : xs)   = 2  + value xs -- Each Rank Type is given a value they should add to the current value of the Hand
value (Card Three _ : xs) = 3  + value xs
value (Card Four _ : xs)  = 4  + value xs
value (Card Five _ : xs)  = 5  + value xs
value (Card Six _ : xs)   = 6  + value xs
value (Card Seven _ : xs) = 7  + value xs
value (Card Eight _ : xs) = 8  + value xs
value (Card Nine _ : xs)  = 9  + value xs
value (Card Ten _ : xs)   = 10 + value xs
value (Card Jack _ : xs)  = 10 + value xs
value (Card Queen _ : xs) = 10 + value xs
value (Card King _ : xs)  = 10 + value xs


-- 3. isBlackjack
isBlackjack :: Hand -> Bool
isBlackjack [a,b] = value [a,b] == 21 -- Checks if a hand of two cards is equal to 21 (an ace and a rank value of 10)
isBlackjack _ = False


-- 4. gameOver
gameOver :: Hand -> Bool
gameOver hand = value hand > 21 -- Checks if the Hand value surpasses 21


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
  <+ handSuit Clubs -- Returns all cards in a standard deck of BlackJack


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