module BlackJack where

import Cards
import Wrapper
import Test.QuickCheck
import System.Random


-- Task A --

-- size hand2
--   = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
--   = 1 + size (Add (Card Jack Spades) Empty))
--   = 2 + size Empty
--   = 2



-- Test cards and a test hand --
aCard1 :: Card
aCard1 = Card {rank = Numeric 6, suit = Spades}

aCard2 :: Card
aCard2 = Card {rank = Numeric 9, suit = Spades}

aHand :: Hand
aHand = Add aCard1 (Add aCard2 Empty)

empty :: Hand   -- empty is an empty hand
empty = Empty
--------------------------------



-- The value functions --

-- Calculates the value of a rank
valueRank :: Rank -> Integer
valueRank (Numeric m) = m
valueRank Ace         = 11
valueRank _           = 10


-- Calculates the value of a given card
valueCard :: Card -> Integer
valueCard (Card rank _) = valueRank rank


-- Calculates the amount of aces in a given hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand)            = numberOfAces hand

handAces = Add (Card Ace Hearts) (Add (Card Ace Spades) (Add (Card Ace Clubs) Empty))

-- Calculates the exact value of a given hand
valueHand :: Hand -> Integer
valueHand Empty         = 0
valueHand (Add card hand) = (valueCard card) + (valueHand hand)


-- Corrects the value of a given hand 
-- by checking what value the ace(s) should have
value :: Hand -> Integer
value hand
	| (valueHand hand) > 21 = (valueHand hand) - (10 * (numberOfAces hand))
	| otherwise = valueHand hand
-----------------------



-- The winner functions --

-- Checks if the player is bust
gameOver :: Hand -> Bool
gameOver hand = value hand > 21


-- Checks who the winner is
winner :: Hand -> Hand -> Player
winner gHand bHand
	| gameOver gHand                = Bank
	| gameOver bHand                = Guest
	| (value gHand) > (value bHand) = Guest
	| otherwise                     = Bank
----------------------



-- Combine hand function --

-- Combines two hands
(<+) :: Hand -> Hand -> Hand
Empty <+ hand = hand
(Add card hand1) <+ hand2 = Add card (hand1 <+ hand2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1) + (size h2) == size (h1 <+ h2)
------------------



-- Deck functions --

-- A hand with all cards of a variable suit
suitHand :: Suit -> Hand
suitHand s = Add (Card Ace s) (Add (Card King s) (Add (Card Queen s) (Add (Card Jack s) (Add (Card (Numeric 10) s) (Add (Card (Numeric 9) s) (Add (Card (Numeric 8) s) 
				(Add (Card (Numeric 7) s) (Add (Card (Numeric 6) s) (Add (Card (Numeric 5) s) (Add (Card (Numeric 4) s) (Add (Card (Numeric 3) s) (Add (Card (Numeric 2) s) Empty))))))))))))

-- Combines a full hand of each suit into a full deck				
fullDeck :: Hand
fullDeck = suitHand Spades <+ suitHand Clubs <+ suitHand Hearts <+ suitHand Diamonds


-- Draws a card from a hand (deck) and puts it on another hand
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: the deck is empty."
draw (Add card deck) hand = (deck, (Add card hand))
--------------------



-- Bank functions --

-- Let the bank play using the deck
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- The rules by which the bank plays
playBank' :: Hand -> Hand -> Hand
playBank' deck hand
	| value hand < 16    = playBank' deck' hand'
	| otherwise          = hand
	where (deck', hand') = draw deck hand
----------------------



-- Deck shuffling functions --

-- Shuffles the deck
shuffle :: StdGen -> Hand -> Hand
shuffle _ Empty = Empty
shuffle g (Add card deck)       = Add chosenCard (shuffle g' newHand)
	where (chosenCard, newHand) = removeCard n (Add card deck) Empty
	      (n, g')               = randInt g 1 (size (Add card deck))
	
-- Remove the nth card from the given deck
removeCard :: Integer -> Hand -> Hand -> (Card, Hand)
removeCard n (Add card hand) tempHand
	| (n > (size (Add card hand))) || (n < 0) = error "n out of bounds of hand"
	| n == 1                                 = (card, hand <+ tempHand)
	| otherwise                              = removeCard (n-1) hand (Add card tempHand)
	

-- Randomize an Integer between to given values
randInt :: StdGen -> Integer -> Integer -> (Integer, StdGen)
randInt g min max = (n, g')
	where (n, g') = randomR (min, max) g
	
	
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h


belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty    = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h


prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffle g hand)
-----------------------



-- Interface --
implementation = Interface
  {  iEmpty     = empty
  ,  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }


main :: IO ()
main = runGame implementation
----------------


