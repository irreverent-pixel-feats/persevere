{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Irreverent.Persevere.Data
-- Copyright    : (C) 2017-2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Irreverent.Persevere.Data where

import Irreverent.Persevere.Data

import Irreverent.Quantity.Core.Data.Time

import Data.Monoid ( (<>) )

import Test.Irreverent.Persevere.Arbitrary

import Test.Irreverent.Quantity.Core.Arbitrary

import Lab.Core.Property
import Lab.Core.QuickCheck
import Lab.Core.QuickCheck.TH

import Preamble hiding ( (<>) )
import Prelude ( Integer, (^) )

prop_retryLimit :: NonNegative Int -> NonNegative Int -> Property
prop_retryLimit (NonNegative limit) (NonNegative n) = maybe (n .>=. limit) (\n' -> inMicroseconds n' === 0 .&&. n .<. limit) . ($ n) . getRetryPolicy $ retryLimit limit

prop_constantDelay :: NonNegative Int -> Property
prop_constantDelay (NonNegative n) = forAll durationQuantity $ \delay ->
  maybe (failWith "Constant Delay policy is never supposed to give up") (=== delay) . ($ n) . getRetryPolicy $ constantDelay delay

prop_exponentialBackoff :: NonNegative Int -> Property
prop_exponentialBackoff (NonNegative n) = forAll durationQuantity $ \base ->
  maybe (failWith "Exponential back offs never give up") (=== (fmicroseconds . ((2^n) *) . finMicroseconds $ base)) . ($ n) . getRetryPolicy $ exponentialBackoff base

prop_leftIdentity :: NonNegative Int -> Property
prop_leftIdentity (NonNegative n) = forAll retryPolicies $ \(RP _ policy) ->
  getRetryPolicy policy n === getRetryPolicy (mempty <> policy) n

prop_rightIdentity :: NonNegative Int -> Property
prop_rightIdentity (NonNegative n) = forAll retryPolicies $ \(RP _ policy) -> getRetryPolicy policy n === getRetryPolicy (policy <> mempty) n

prop_associativity :: NonNegative Int -> Property
prop_associativity (NonNegative n) = forAll retryPolicies $ \(RP _ x) ->
  forAll retryPolicies $ \(RP _ y) ->
    forAll retryPolicies $ \(RP _ z) ->
      getRetryPolicy ((x <> y) <> z) n === getRetryPolicy (x <> (y <> z)) n

fmicroseconds :: Integer -> DurationQuantity
fmicroseconds = microseconds

finMicroseconds :: DurationQuantity -> Integer
finMicroseconds = inMicroseconds

return []
tests :: IO Bool
tests = $quickCheckAll
