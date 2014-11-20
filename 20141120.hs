{--

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

-- *Main> sacksSold 62.85 385
-- []

-- Hmmm, I'm missing something here. Alegbra says that we need to sell 
-- (6285.0 - 10.0*385)/(25.0 - 10.0) = 162.33333333333334 large sacks
-- so there doesn't appear to be a solution with integral values

-- Eh! You get it! Do something 'nice' to find a solution to the above problem
