{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Irreverent.Persevere.Control.Retry
-- Copyright    : (C) 2017-2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Irreverent.Persevere.Control.Retry where

import Irreverent.Persevere.Control.Retry
import Irreverent.Persevere.Data

import Irreverent.Quantity.Core.Data.Time (DurationQuantity)

import Test.Irreverent.Persevere.Arbitrary

import Ultra.Control.Monad.Trans.Either (EitherT, left, runEitherT)
import Ultra.Data.Foldable (filteredBy)

import Lab.Core.IO
import Lab.Core.Property
import Lab.Core.QuickCheck
import Lab.Core.QuickCheck.TH

import Preamble

-- These tests dont seem to work and i think it has something to do with quickcheck swallowing asynchronous exceptions
prop_retry :: Property
prop_retry = forAll cappedRetryPolicies $ \(RP _ policy) -> ioProperty $
  let
    action :: EitherT () IO ()
    action = left ()

  in do
    ex <- runEitherT $ withRetries policy pure action
    pure $ case ex of
      Right ()                -> failWith "withRetries should return result when the action never succeeds"
      Left (RetryError fs)    -> (filteredBy failureInterval fs) === retryIntervalsPolicy policy

retryIntervalsPolicy :: RetryPolicy -> [DurationQuantity]
retryIntervalsPolicy (RetryPolicy p) =
  let
    retryIntervals' :: Int -> [DurationQuantity]
    retryIntervals' n = maybe [] (: retryIntervals' (n + 1)) $ p n
  in reverse $ retryIntervals' 0

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 })
