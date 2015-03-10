{--

From the previous day's Mensa Genius Quiz-a-Day book:

My husband and I can't seem to get our ...

[eheh]

[okay, back in character:]

My husband and I can't seem to get our watches to work properly. His
consistently runs one minute per hour fast, and mine runs two minutes
per hour slow ...

[*sigh*

I think the puzzle composer meant to write 'runs ... too quickly'
and then 'runs ... too slowly,' but what do I know?]

... This morning we nearly missed a wedding because our watches were
an hour apart, and we looked at the slower one.

[And these people are supposed to be Mensans? And they look at the
slower one not to be late? SERIOUSLY? Okay, I'm okay now.]

How many hours had elapsed since we set both of them properly? --}

type Hours = Int
type Minutes = Int
data Watch = RunsAhead Minutes | RunsBehind Minutes

diff :: Watch -> Watch -> Hours -> Minutes
diff w1 w2 hs = (mins w1 * hs) - (mins w2 * hs)
    where
      mins (RunsAhead m) = m
      mins (RunsBehind m) = -m

--Could just do the math, but lets do an iterative solution for kicks
--The solution is hte number of hours that the given minute difference occurs between, if the pair has the same two members
--then the minuts off occurs at an exact hour
durationDifference :: Watch -> Watch -> Minutes -> (Hours, Hours)
durationDifference husband mine difference = calc 1
    where
      calc hs | diff husband mine hs == difference = (hs, hs)
              | diff husband mine hs > difference = (hs - 1, hs)
              | otherwise = calc (hs + 1)

-- *Main> durationDifference (RunsAhead 1) (RunsBehind 2) 60
-- (20,20)

{-- But the more pressing question is this:

CAN GEOPHF PRESENT A MENSAN PUZZLE UNSNARKILY?

That's the real question here --}

data Puzzle = Mensan
   deriving (Eq, Ord, Show)
data Presenter = Geophf
data Attitude = Snarkily | Unsnarkily
   deriving (Eq, Ord, Show)

presentation :: Puzzle -> Presenter -> Maybe Attitude
presentation puz byWhom = undefined

-- note the objective case in the preposition, also note
-- the adVERBIAL modifier to the verb, not the adJECTIVAL!

-- Geez! Boy! Mensa be all messin' up their modifier-kinds?
-- I mean, like: SERIOUSLY?

-- (I guess we have the answer to the snarkiness question)