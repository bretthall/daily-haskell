{--

Solution to @1HaskellADay http://lpaste.net/5394990454380953600

A little bit of strategy posted from levinotik.com

Solving the White House brainteaser

Ed Felten, the Deputy Chief Technology Officer at the White House Office of 
Science and Technology Policy, posted this fun, little coordination problem. 

The gist is that Alice and Bob are a team. Each in separate rooms, they flip a 
coin. They note what they flipped (Heads or Tails) and write down a guess as 
to the other’s flip. They win as a team as long as either of them are correct. 
It’s helpful to enumerate all the possible flip outcomes across both Alice and 
Bob to try to work out a winning strategy. Here’s the four possibilities:

(Bob flips Heads, Alice flips Tails)
(Bob flips Tails, Alice flips Heads)
(Bob flips Tails, Alice flips Tails)
(Bob flips Heads, Alice flips Heads)

Those are the only four possibilities. Having enumerated all of the cases, we 
can start to make some guesses and check if it results in a win for any of the 
possibilities. As noted in the original blog post, it’s easy to see that Alice 
and Bob simply agreeing to always pick Heads or Tails isn’t going to work. If 
they both always pick Heads, then neither will guess correctly when they both 
flip a Tails.

[etc. You get the idea, I hope]

So, write a haskell solution to this problem: 

What does Alice guess, what does Bob guess, so that they have a winning
strategy.

Show the results of their guesses against the coin flips. Did you come up
with a winning strategy? --}

data Toss = Heads | Tails deriving (Show, Eq)
type Guess = Toss
data Agent = Alice | Bob

data Test = Winning | FAIL deriving (Show, Eq)
type Approach = Toss -> Guess
data Strategy = Strat { alice :: Approach, bob :: Approach }
data Game = On Toss Toss deriving (Show, Eq) -- both alice and bob toss a coin

-- so all games are the distribution of heads and tails over alice and bob

--there are four possible approaches: always guess heads, always guess tails, guess the same thing you flipped, or guess the opposite of your flip 
approachAlwaysHeads = const Heads
approachAlwaysTails = const Tails
approachSame = id
approachOpposite Heads = Tails
approachOpposite Tails = Heads

attack :: Strategy -> [Game] -> [Test]
attack aliceandbobsApproach allgames = map (checkGame aliceandbobsApproach) allgames

checkGame (Strat as bs) (On a b) | (as a == b) || (bs b == a) = Winning
                                 | otherwise = FAIL

possibleGames = [On a b | a <- [Heads, Tails], b <- [Heads, Tails]]
approaches = [("Always Heads", approachAlwaysHeads), 
              ("Always Tails", approachAlwaysTails), 
              ("Same", approachSame), 
              ("Opposite", approachOpposite)]
strats = [(na, nb, Strat a b) | (na, a) <- approaches, (nb, b) <- approaches]

-- can you get devise a strategy so that all tests are winning?

-- *Main> filter (\(_, _, ts) -> all (== Winning) ts) $ map (\(na, nb, s) -> (na, nb, attack s possibleGames)) strats
-- [("Same","Opposite",[Winning,Winning,Winning,Winning]),("Opposite","Same",[Winning,Winning,Winning,Winning])]
