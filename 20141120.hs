{--

solution to @1HaskellADay http://lpaste.net/114620

From Mensa Genius Quiz-a-Day book, for October 20th

People have already started buying Halloween candy, the better to eat before
Hallowe'en [ed: note the apostrophe] Our local candy store was selling
large sacks of candy for 25¢ and small sacks for 10¢. The new cashier wasn't
up to the job, though; she marked down the number of sacks she sold, but she
forgot to record their prices. At the end of the day she found she had sold
385 candy sacks and had $62.65 in her cash register. Before her boss came by,
however, she figured out how many of each size of candy sack she had sold.
Can you?

--}

-- We coudl just do the algebra but I did too much of that back in school. Instead lets brute-force it for kicks.

type Count = Int

type Cents = Int

data Total = Total {large::Count, small::Count, totalCount::Count, totalMoney::Cents} deriving (Show)

priceSmall :: Cents
priceSmall = 10

priceLarge :: Cents
priceLarge = 25

makeTotal :: Count -> Count -> Total
makeTotal l s = Total l s (l + s) (l*priceLarge + s*priceSmall)

possible :: Count -> [Total]
possible count = map (uncurry makeTotal) [(l, count - l) | l <- [0..count]]

type Money = Double

toCents :: Money -> Cents
toCents = floor.(* 100.0)

sacksSold :: Money -> Count -> [Total]
sacksSold moonay totalSacksSold = filter ((== toCents moonay).totalMoney) $ possible totalSacksSold

-- *Main> sacksSold 62.65 385
-- [Total {large = 161, small = 224, totalCount = 385, totalMoney = 6265}]

-- Eh! You get it! Do something 'nice' to find a solution to the above problem
