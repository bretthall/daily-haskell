--Solution to @1HaskellADay http://lpaste.net/117906

-- Next up in our examination of Lenses is the over function.

over :: (a -> Lens a r) -> (r -> r) -> a -> a
over lens fn tup = set lens (fn $ view lens tup) tup

-- where you recall, Lens was (ambiguously) declared as:

--data Lens structure = ViewIntoThatStructure

--From yesterday (http://lpaste.net/117933):
data Lens a r = Lens {val::a, getLens:: r, setLens::r -> a}

_1 :: (a, b) -> Lens (a, b) a
_1 t = Lens t (fst t) (\r -> (r, snd t))
_2 :: (a, b) -> Lens (a, b) b
_2 t = Lens t (snd t) (\r -> (fst t, r))

view :: (a -> Lens a r) -> a -> r
view lensF struct = getLens (lensF struct)

set :: (a -> Lens a r) -> r -> a -> a
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
