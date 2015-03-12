module Rules where

-- http://lpaste.net/114590

import Control.Applicative
import Data.Foldable
import Prelude hiding (and)

--Applies a set of rules to a given value returning true if all the rules are satisfied
satisfiesAll :: (Foldable a, Applicative a) => a (t -> Bool) -> t -> Bool
satisfiesAll rs = and.(applyRules rs)

--Applies the given rules to the given value returning the results
applyRules :: (Applicative a) => a (t -> Bool) -> t -> a Bool
applyRules rs = (rs <*>).pure