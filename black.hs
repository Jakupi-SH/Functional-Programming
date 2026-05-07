module Main where

-- =========================
-- Types
-- =========================

-- The suit of a card.
data Suit = Hearts | Spades | Diamonds | Clubs
  deriving (Eq, Show)

-- The rank of a card.
data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Eq, Show)

-- A card has a rank and a suit.
data Card = Card Rank Suit
  deriving (Eq, Show)

-- A hand is a list of cards.
type Hand = [Card]

-- The possible players.
data Player = Bank | Guest
  deriving (Eq, Show)

-- =========================
-- Core assignment functions
-- =========================

-- Counts the number of face cards in a hand.
faceCards :: Hand -> Integer
faceCards [] = 0
faceCards (Card rank _ : xs) = faceCount rank + faceCards xs
  where
    faceCount Jack  = 1
    faceCount Queen = 1
    faceCount King  = 1
    faceCount _     = 0

-- Calculates the value of a hand.
-- Aces first count as 11, then are reduced to 1 if needed.
value :: Hand -> Integer
value hand = adjustForAces (rawValue hand) (countAces hand)

-- Raw value where every ace is worth 11.
rawValue :: Hand -> Integer
rawValue [] = 0
rawValue (Card rank _ : xs) = rankValue rank + rawValue xs
  where
    rankValue (Numeric n) = n
    rankValue Jack        = 10
    rankValue Queen       = 10
    rankValue King        = 10
    rankValue Ace         = 11

-- Counts how many aces are in the hand.
countAces :: Hand -> Integer
countAces [] = 0
countAces (Card rank _ : xs) = aceValue rank + countAces xs
  where
    aceValue Ace = 1
    aceValue _   = 0

-- Adjusts the total value if it is above 21 by turning aces from 11 into 1.
adjustForAces :: Integer -> Integer -> Integer
adjustForAces total aces
  | total <= 21 = total
  | aces == 0   = total
  | otherwise   = adjustForAces (total - 10) (aces - 1)

-- True if the hand has exactly 2 cards and value 21.
isBlackjack :: Hand -> Bool
isBlackjack [c1, c2] = value [c1, c2] == 21
isBlackjack _        = False

-- True if the hand value is above 21.
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Determines the winner. Tie goes to the bank.
winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest           = Bank
  | gameOver bank            = Guest
  | value guest > value bank = Guest
  | otherwise                = Bank

-- Puts the first hand on top of the second.
(<+) :: Hand -> Hand -> Hand
[] <+ ys     = ys
(x:xs) <+ ys = x : (xs <+ ys)

infixr 5 <+

-- Returns all 13 cards of a given suit.
handSuit :: Suit -> Hand
handSuit s =
  [ Card Ace s
  , Card King s
  , Card Queen s
  , Card Jack s
  , Card (Numeric 10) s
  , Card (Numeric 9) s
  , Card (Numeric 8) s
  , Card (Numeric 7) s
  , Card (Numeric 6) s
  , Card (Numeric 5) s
  , Card (Numeric 4) s
  , Card (Numeric 3) s
  , Card (Numeric 2) s
  ]

-- Checks whether a card belongs to a hand.
belongsTo :: Card -> Hand -> Bool
belongsTo _ [] = False
belongsTo c (x:xs)
  | c == x    = True
  | otherwise = belongsTo c xs

-- The full 52-card deck.
fullDeck :: Hand
fullDeck =
  handSuit Hearts <+ handSuit Spades <+ handSuit Diamonds <+ handSuit Clubs

-- Draws one card from the top of the deck into a hand.
draw :: Hand -> Hand -> (Hand, Hand)
draw [] _ = error "draw: empty deck"
draw (c:cs) hand = (cs, c : hand)

-- Bank keeps drawing while value is less than 16.
playBank :: Hand -> Hand -> Hand
playBank deck hand
  | value hand < 16 =
      let (deck', hand') = draw deck hand
      in playBank deck' hand'
  | otherwise = hand

-- =========================
-- Pretty printing helpers
-- =========================

showSuit :: Suit -> String
showSuit Hearts   = "Hearts"
showSuit Spades   = "Spades"
showSuit Diamonds = "Diamonds"
showSuit Clubs    = "Clubs"

showRank :: Rank -> String
showRank (Numeric n) = show n
showRank Jack        = "Jack"
showRank Queen       = "Queen"
showRank King        = "King"
showRank Ace         = "Ace"

showCard :: Card -> String
showCard (Card rank suit) = showRank rank ++ " of " ++ showSuit suit

showHand :: Hand -> String
showHand [] = "[]"
showHand [c] = showCard c
showHand (c:cs) = showCard c ++ ", " ++ showHand cs

printHandInfo :: String -> Hand -> IO ()
printHandInfo name hand = do
  putStrLn (name ++ ": " ++ showHand hand)
  putStrLn (name ++ " value: " ++ show (value hand))

printTitle :: String -> IO ()
printTitle t = do
  putStrLn ""
  putStrLn ("=== " ++ t ++ " ===")

-- =========================
-- Demo helpers
-- =========================

-- Player draws exactly one card to imitate a "hit".
playerHitOnce :: Hand -> Hand -> (Hand, Hand)
playerHitOnce deck playerHand = draw deck playerHand

-- A traced version of the bank turn, so we can see each draw.
playBankWithTrace :: Hand -> Hand -> IO Hand
playBankWithTrace deck hand = do
  putStrLn ("Dealer hand: " ++ showHand hand ++ " (value " ++ show (value hand) ++ ")")
  if value hand < 16
    then do
      putStrLn "Dealer draws."
      let (deck', hand') = draw deck hand
      playBankWithTrace deck' hand'
    else do
      putStrLn "Dealer stands."
      return hand

-- =========================
-- One-round imitation
-- =========================

simulateRound :: IO ()
simulateRound = do
  printTitle "Blackjack round simulation"

  -- Fixed deck so the round is always the same.
  -- Order matters: draw takes the first card from the list.
  let deck0 =
        [ Card (Numeric 10) Hearts   -- player first card
        , Card (Numeric 6) Clubs     -- dealer first card
        , Card (Numeric 5) Spades    -- player second card
        , Card (Numeric 9) Diamonds  -- dealer second card
        , Card (Numeric 4) Hearts    -- player hits
        , Card (Numeric 3) Clubs     -- dealer draws
        , Card King Spades           -- next card if needed
        ]

  putStrLn "Initial dealing..."

  -- Initial deal: player, dealer, player, dealer
  let (deck1, player1) = draw deck0 []
  let (deck2, dealer1) = draw deck1 []
  let (deck3, player2) = draw deck2 player1
  let (deck4, dealer2) = draw deck3 dealer1

  printHandInfo "Player" player2
  printHandInfo "Dealer" dealer2

  putStrLn ""
  putStrLn "Player chooses to hit."
  let (deck5, player3) = playerHitOnce deck4 player2
  printHandInfo "Player" player3

  if gameOver player3
    then do
      putStrLn "Player busts. Dealer wins immediately."
    else do
      putStrLn ""
      putStrLn "Player stands."
      putStrLn "Dealer plays..."
      dealerFinal <- playBankWithTrace deck5 dealer2

      putStrLn ""
      putStrLn "Final result:"
      printHandInfo "Player" player3
      printHandInfo "Dealer" dealerFinal
      putStrLn ("Winner: " ++ show (winner player3 dealerFinal))

-- =========================
-- Main
-- =========================

main :: IO ()
main = do
  printTitle "Basic tests"

  let blackjackHand = [Card Ace Hearts, Card King Spades]
  let bustedHand = [Card (Numeric 10) Clubs, Card (Numeric 7) Diamonds, Card (Numeric 5) Hearts]

  printHandInfo "Blackjack hand" blackjackHand
  putStrLn ("Is blackjack? " ++ show (isBlackjack blackjackHand))

  putStrLn ""

  printHandInfo "Busted hand" bustedHand
  putStrLn ("Game over? " ++ show (gameOver bustedHand))

  putStrLn ""
  putStrLn ("Face cards in blackjack hand: " ++ show (faceCards blackjackHand))
  putStrLn ("Cards in full deck: " ++ show (countCards fullDeck))

  simulateRound

-- Extra helper for demo output.
countCards :: Hand -> Integer
countCards [] = 0
countCards (_:xs) = 1 + countCards xs