{-# LANGUAGE OverloadedLists #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Irreverent.Persevere.Arbitrary
-- Copyright    : (C) 2017-2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Test.Irreverent.Persevere.Arbitrary
--
-------------------------------------------------------------------
module Test.Irreverent.Persevere.Arbitrary (
  -- * Types
    ShowRetryPolicy(..)
  -- * Generators
  , retryLimits
  , constantDelays
  , exponentialBackoffs
  , delayPolicies
  , uncappedRetryPolicies
  , cappedRetryPolicies
  , retryPolicies
  ) where

import Irreverent.Persevere.Data

import Irreverent.Quantity.Core.Data.Time

import Data.Monoid ((<>))

import Lab.Core.QuickCheck

import Preamble hiding ((<>))

data ShowRetryPolicy = RP String RetryPolicy

instance Show ShowRetryPolicy where
--show :: a -> String
  show (RP s _) = s

instance Semigroup ShowRetryPolicy where
--(<>) :: a -> a -> a
  (<>) (RP sx x) (RP sy y) = RP (sx <> " <> " <> sy) (x <> y)

instance Monoid ShowRetryPolicy where
--mempty :: a
  mempty = RP "Empty" mempty

--mappend :: a -> a -> a
  mappend (RP sx x) (RP sy y) = RP (sx <> " <> " <> sy) (x <> y)


retryLimits :: Gen ShowRetryPolicy
retryLimits = do
  limit <- elements [1..6]
  pure $ RP ("retryLimit " <> show limit) (retryLimit limit)

constantDelays :: Gen ShowRetryPolicy
constantDelays = do
  delay <- elements . fmap fmicroseconds $ [200000, 500000]
  pure $ RP ("constantDelay " <> show delay) (constantDelay delay)

exponentialBackoffs :: Gen ShowRetryPolicy
exponentialBackoffs = do
  base <- elements . fmap fmicroseconds $ [200000, 500000]
  pure $ RP ("exponentialBackoff " <> show base) (exponentialBackoff base)

delayPolicies :: Gen ShowRetryPolicy
delayPolicies = oneof [constantDelays, exponentialBackoffs]

uncappedRetryPolicies :: Gen ShowRetryPolicy
uncappedRetryPolicies = (<>)
  <$> retryLimits
  <*> delayPolicies

cappedRetryPolicies :: Gen ShowRetryPolicy
cappedRetryPolicies = do
  cap <- elements . fmap fmicroseconds $ [300000, 600000]
  (RP uncappeds uncapped) <- uncappedRetryPolicies
  pure $ RP ("capDelay " <> show cap <> " (" <> uncappeds <> ")") (capDelay cap uncapped)

retryPolicies :: Gen ShowRetryPolicy
retryPolicies = oneof [
    uncappedRetryPolicies
  , cappedRetryPolicies
  ]

fmicroseconds :: Integer -> DurationQuantity
fmicroseconds = microseconds
