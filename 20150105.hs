{--

Solution to @1HaskellADay http://lpaste.net/117815

Okay, 2014 came and went, and I must have slept through it because I'm
waking up now in 2015 to Lenses. And I STILL don't even have them, because
my poor, old mac with my poor old repl gives me this:

Prelude> import Control.Lens

<no location info>:
    Could not find module `Control.Lens':
      it is not a module in the current program, or in any known package.

So there it is.

What is a Lens?

A lens is a burrito.

No. Wait.

Instead of me introducing lenses, I'm sure you can 

1. be using Lenses already and so don't need an introduction, or 
2. be able to look up lenses yourself, and read a better introduction 
than what I can put here.

Look! I'll even give you a link! For FREE!

https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial

I'm as excited as others for the applications of lens, particularly for
the general way they provide the ability to view or to change data structures,
regardless of the underlying structure.

I'm MOST interested in Iso's, because anything that gives me nothing is
very exciting to me.

HEY! LOOK! You just got id, in three million lines of code! Aren't you
excited?!?!

Uh. Yeah.

Or, eheh, kidding aside, as Iso's give bidirectionality to identities, that's
what floats my boat (as I've been writing reversible mappings manually for
a good deal of the functional work that I do).

So! But! This is not the Iso-article. This is the Lens-article.

So, YOU, dear reader, have access to the lens-library already built into
your ghc, BUT what has that gotten you so far, in terms of understanding what
these libraries give you? Huh? Huh?

Let's follow this line of inquiry, and seeing the writer of this problem
(who refers to himself, occasionally, in the third person) is geophf, it's
going to be rather loosey-goosey in its approach. For a more formal definition,
see, well, the reference implementation from @kmett (another link, right here,
for ya:)

https://hackage.haskell.org/package/lens

And for our approach today, we are just going to look at get and put
(as it were), and take on modify at a later time.

So, for the tuple-type, we have the lenses _1 and _2 and we can either
view or set them.

>>> view _1 (1,2) ~> 1

>>> set _2 "love" (1,2) ~> (1, "love")

Sing with me!

One love, One heart
Let's get together and feel all right!

Thank you, Bob Marley!

So, our assignment for today is to create these lens-producing functions,
_1 and _2 for the tuple-type (only) (for today) and then to define
the functions over lenses, view and set, such that we get (heh: 'get' ...
geddit?) the above results. --}

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

WHEW!

BUT THEN! We must make sure the above obeys the lens-laws, or else
the lens-police will come and read us our lens-rights, lensifully!

(that's a word now)

The lens-laws are

1. Get-Put: You get back what you put in, or:

view l (set l v s) == v

*Main> view _1 (set _1 3 (1,2))
3
*Main> view _2 (set _2 3 (1,2))
3

2. Put-Get: Putting back what you got doesn't change anything:

set l (view l s) s == s

*Main> set _1 (view _1 (1,2)) (1,2)
(1,2)
*Main> set _2 (view _2 (1,2)) (1,2)
(1,2)

3. Put-Put: Putting twice is the same as putting once:

set l v' (set l v s) = set l v' s

*Main> set _1 4 (set _1 3 (1,2))
(4,2)
*Main> set _2 4 (set _2 3 (1,2))
(1,4)

So, make sure that your view and set obey the above three laws for the
Lens-type you've defined for tuples --}

{--

What?

That's all, just defining a getter and a putter for the (lensed) tuple-type.

That's all. Go to! Go to!

--}