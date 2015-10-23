-- Solution to @1HaskellADay http://lpaste.net/4042786156616613888

{--

Hey, all, whoa! It's Thursday, and this is the first 1HAD problem this week?

What happened?

Well, firstly, sorry, and secondly, I was at the GraphConnect 2015 in San Fran,
learning some spiffy-neato things!

One of these neat things I learnt was the concept of 'triadic closure':

https://en.wikipedia.org/wiki/Triadic_closure

So, let's explore this. Triadic closure says that networks settle to stable
triangles, so first we have to know what triangles in graphs are...

... okay, got it? Still with me?

... And next we have to know what stable triangles are.

Let's do that by example.

A stable triangle is where everybody's friendies ... you know: on FB.

Oh, will you 'friend' me? Oh, yes, I would LUVR to 'friend' you.

That's what friends means these days.


      (a)<-[friends_with]->(b)
      (b)<-[friends_with]->(c)
      (c)<-[friends_with]->(a)

Now you see that the relationships here are bi-directional, but in Graphs, 
bidirectionality is not necessary, nor is it even assumed.

Food for thought.

But that's one form of stability.

The other form of stability is where two friends hate a third:

     (a)<-[friends_with]->(b)
     (a)-[hates]->(c)
     (b)-[hates]->(c)

Awwww, poor c, picked on at school and everything.

Ugh. Moving on (as c was moiself)

An unstable triangle, then, is where there is a member who is loved by one
friend, but hated by the other:

    (a)<-[friends_with]->(b)
    (b)<-[friends_with]->(c)
    (a)-[hates]->(c)

Uh, oh! Instability! And triadic closure predicts (and predicably so) that that
triangle will dissolve or will settle to one of the stable states.

Guess how well it works?

Well, it showed from a set of unstable alliances pre-WWI ('World War I')
('The Great War') ('The War to End All Wars') ('but didn't') how these
(unstable) allies would realign until all triangles of alliances and enemies
reached a stable state.

Let's explore.

First, declare-your-own or use a typed-relation type that declares a kind of
relation between two values.

--}

import Data.List (find)
import Control.Monad (guard)

data Relation a rel b = R a rel b deriving (Eq, Show)

-- Now, let's work with ALLIES and ENEMIES as the relation types:

data Alliance = ALLIES | ENEMIES deriving (Eq, Show)

-- Now, let's have the pre-WWI countries:

data Country = Austria | Germany | Belgium | Britain | France | Italy | Russia
   deriving (Eq, Show, Enum)

countries = enumFrom Austria

-- Now, let's set the historical context. Because of the Napolionic Wars,
-- Britain and France where enemies

treatyLondon, tripleAlliance, francoRussianAlliance, ententeCordiale
         :: [Relation Country Alliance Country]

-- And Britian allied with Belgium with the Treaty of London (1839)

treatyLondon = [R Britain ALLIES Belgium]

-- But then, during the same time, Italy, Austria, and Germany entered into
-- the Triple Alliance

tripleAlliance = [R a ALLIES b | a <- tcs, b <- tcs, a /= b]
  where
    tcs = [Italy, Austria, Germany]

-- And the Franco-Russian alliance came along, ... 

francoRussianAlliance = [R France ALLIES Russia]

-- ... but then, lo-and-behold, Britain and France reversed their emity and 
-- became allies with the Entente Cordiale!

ententeCordiale = [R Britain ALLIES France]

-- create these alliances

alliances :: [Relation Country Alliance Country]
alliances = concat [treatyLondon, tripleAlliance, francoRussianAlliance, ententeCordiale]

-- THE NEXT STEP:

-- assume that all countries that are not allies are enemies, fill out these
-- missing relations ... how will you do that?

friendOrFoe :: [Relation Country Alliance Country] -> Country -> Country -> Alliance
friendOrFoe rels a b = case find (\(R a' _ b') -> ((a, b) == (a', b') || (b, a) == (a', b'))) rels of
                   Just (R _ r _) -> r
                   Nothing -> ENEMIES

friendsAndFrenemies :: [Relation Country Alliance Country]
                    -> [Relation Country Alliance Country]
friendsAndFrenemies rels = [R a (friendOrFoe rels a b) b | a <- countries, b <- countries, a /= b]


-- What you should have is this web, and oh, what a tangled web we weave!

-- Now, in this web identify all triangles, and, further, determine whether
-- they are stable triangles or unstable triangles:

data Stability = STABLE | UNSTABLE deriving (Eq, Show)
data Triangle a b c s = Tri a b c s
      deriving (Eq, Show)

triangles :: [Relation Country Alliance Country] ->
             [Triangle Country Country Country Stability]
triangles rels = do
  a <- countries
  b <- countries
  guard $ a /= b
  c <- countries
  guard $ a /= c && b /= c
  return $ Tri a b c (stability a b c)
  where
    stability a b c | all (== ALLIES) fOrFs = STABLE
                    | numEnemies == 2 && numFriends == 1 = STABLE
                    | otherwise = UNSTABLE
      where
        fOrFs = map (uncurry (friendOrFoe rels)) [(a, b), (a, c), (b, c)]
        numEnemies = length $ filter (== ENEMIES) fOrFs
        numFriends = length $ filter (== ALLIES) fOrFs
  

-- How many triangles do we have?
-- *Main> length $ triangles $ friendsAndFrenemies alliances
-- 210

-- How many unstable triangles do we have?
-- *Main> length $ filter (\(Tri _ _ _ s) -> s == UNSTABLE) $ triangles $ friendsAndFrenemies alliances
-- 66

-- How many stable triangels do we have?
-- *Main> length $ filter (\(Tri _ _ _ s) -> s == STABLE) $ triangles $ friendsAndFrenemies alliances
-- 144


-- Tomorrow we'll use triadic closure to predict the Anglo-Russian Entente
