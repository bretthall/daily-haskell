{--

A couplet of thematically-tied puzzles having something to do with one
of those guys who took a short-cut, and we know how that turns out,
every time, don't you?

Spouse: "Honey, why are you home so late from work?" *glares*
Me: "I took a short-cut on my commute home." *shrugs*
Spouse: "Oh."

THAT'S what happens when you take a shortcut!

Did you notice that my honey called me 'honey'?

She's SO the romantic! ;)

These puzzles are from the the Mensa Genuis Quiz-a-Day Book for
October 12th and October 13th. The October 12th problem is
presented here; the October 13th problem is presented as a 
BONUS problem.

October 12th:

From the sequenced (in-order) string of letters: --}

seed :: String
seed = "CAOXLEUEMIBRUXSE"

{--

If 7 is a prime number, cross out the A's and E's from the string;
if not, cross out the C's and L's. If the square root of 625 is 25,
then cross out the I's and the R's; if not, cross out the C's and 
U's. If 0¬∞C is equivalent to 10¬∞F, then cross out the B's, the M's,
and the S's [ed: Ooh! THREE Letters ... maybe]; if not, cross out
only the X's [ed: Ooh! Or just one letter? The suspense is killing
me!]

What do you have left [behind]?

--}

-- Gets the answer but doesn't exercise any monad stuff
easyAnswer = filter (not.(`elem` "AEIRX")) seed

crossOut :: Monad m => Int -> (Int -> Either String String) -> m String
crossOut num predicate = undefined

isPrime :: Int -> Either String String
isPrime se7en = undefined

isSquareRootOf :: Int -> Int -> Either String String
isSquareRootOf isTheDualOfisSquareOf isntIt = undefined

-- curry makes things yummy

isEquivCF :: Int -> Int -> Either String String
isEquivCF celsius fahrenheit = undefined

{-- (non)Helpful (non)hint: the 'monad' is this ana-cata-something-
morphism (Edward Kmett be all like, 'Okay, WHAT did you just say?'

I said this: http://en.wikipedia.org/wiki/Anamorphism)

So, what kind of monad is helpful for cata-re-ana-morphing a list-like
structure? 

(I TOLD you it was a non-helpful non-hint, so don't blame me if you start
seriously considering using lenses and semigroupoid structures in your
code after reading ekmett's big-in-Japan conference proceedings

http://partake.in/events/1698f7f8-4151-4048-b317-03a8c3f1a7ab)

The bonus problem is located at http://lpaste.net/112515

--}