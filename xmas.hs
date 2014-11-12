module Xmas where

import Rules
import Data.List
import System.Random
import Control.Monad (liftM)

data Peep = Brett | Lisa | Julie | Dugan | Mom | Dad deriving (Enum, Show, Eq)

peeps :: [Peep]
peeps = enumFrom Brett

couples :: [(Peep, Peep)]
couples = cs ++ inverted
    where
      cs = [(Brett, Lisa), (Julie, Dugan), (Mom, Dad)]
      inverted = [(y, x) | (x, y) <- cs]

type Rule = [(Peep, Peep)] -> Bool

isCouple :: (Peep, Peep) -> Bool
isCouple c = c `elem` couples

isSame :: (Peep, Peep) -> Bool
isSame (x, y) = x == y

defaultRules :: [Rule]
defaultRules = [all (not.isSame), all (not.isCouple)]

momOrDadGetsDugan :: [(Peep, Peep)] -> Bool
momOrDadGetsDugan = any (\(x, y) -> (y == Dugan) && ((x == Mom) || (x == Dad)))

type RuleName = String

specialRules :: [(Rule, RuleName)]
specialRules = [(momOrDadGetsDugan, "Mom or Dad gets Dugan")]

rules :: [Rule]
rules = defaultRules ++ (map fst specialRules)

possibleMatches :: [[(Peep, Peep)]]
possibleMatches = filter (satisfiesAll rules) $ map (zip peeps) $ permutations peeps

numPossibleMatches :: Int
numPossibleMatches = length possibleMatches

getMatches :: IO [(Peep, Peep)]
getMatches = do
  gen <- getStdGen
  (n, newGen) <- return $ randomR (0, numPossibleMatches - 1) gen
  setStdGen newGen
  return $ possibleMatches !! n

format :: [(Peep, Peep)] -> String
format ps = formatSpecialRules ++ (formatMatches ps)

formatSpecialRules :: String
formatSpecialRules = "Special Rules:\n" ++ (concatMap ((++ "\n").snd) specialRules)

formatMatches :: [(Peep, Peep)] -> String
formatMatches ps = "\n" ++ (concatMap formatMatch ps)

formatMatch :: (Peep, Peep) -> String
formatMatch (g, r) = (show g) ++ " -> " ++ (show r) ++ "\n"

printMatches = getMatches >>= return.format >>= putStrLn