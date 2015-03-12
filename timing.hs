{-# LANGUAGE BangPatterns #-}
module Timing where

import System.Clock
import Control.DeepSeq

type Nanosecs = Integer

timeIt :: NFData a => a -> IO (a, Nanosecs)
timeIt a = do
  start <- getTime Monotonic
  let !ans = force a
  end <- getTime Monotonic
  return (ans, calcElapsed start end)
      where
        calcElapsed a b = secDiff * 10^9 + nsecDiff
            where
              secDiff = fromIntegral (sec b - sec a)::Integer
              nsecDiff = fromIntegral (nsec b - nsec a)::Integer
