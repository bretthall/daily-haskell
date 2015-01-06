--Solution to @1HaskellADay http://lpaste.net/117906

-- Next up in our examination of Lenses is the over function.

over :: ((a,b) -> PairLens a b r) -> (r -> r) -> (a,b) -> (a,b)
over lens fn tup = set lens (fn $ view lens tup) tup

-- where you recall, Lens was (ambiguously) declared as:

--data Lens structure = ViewIntoThatStructure

--From yesterday (http://lpaste.net/117933):
data PairLens a b r = PL {tup::(a,b), getLens:: r, setLens::r -> (a,b)}

_1 :: (a, b) -> PairLens a b a
_1 t = PL t (fst t) (\r -> (r, snd t))
_2 :: (a, b) -> PairLens a b b
_2 t = PL t (snd t) (\r -> (fst t, r))

view :: ((a,b) -> PairLens a b r) -> (a, b) -> r
view lensF struct = getLens (lensF struct)

set :: ((a,b) -> PairLens a b r) -> r -> (a, b) -> (a, b)
set lensF delta struct = setLens (lensF struct) delta

{--

An example of over-usage is from the Little Lens Starter Tutorial at

https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial

which is:

>>> over _1 (++ "!!!") ("goal", "the crowd goes wild")
("goal!!!", "the crowd goes wild")

Go ahead and define the over function --}

-- *Main> over _1 (++ "!!!") ("goal", "the crowd goes wild")
-- ("goal!!!","the crowd goes wild")
