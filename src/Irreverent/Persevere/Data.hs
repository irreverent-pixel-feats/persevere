{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Persevere.Data
-- Copyright    : (C) 2017-2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Persevere.Data (
  -- * Types
    RetryPolicy(..)
  , RetryError(..)
  , RetryErrorInstance(..)
  , TransientRetryError(..)
  -- * Functions
  , renderRetryError
  , renderRetryErrorInstance
  , renderTransientRetryError
  , attemptNo
  , failureInterval
  -- * Policies
  , retryLimit
  , capDelay
  , constantDelay
  , exponentialBackoff
  ) where

import Irreverent.Quantity.Core.Data.Time

import Ultra.Control.Applicative
import qualified Ultra.Data.Text as T

import Preamble
import Prelude ((^))

-- |
-- With retries an error becomes a list of the errors encountered
-- the iteration number and the delay for that attempt.
-- note that the delay is not adequate to calculate when the
-- attempt actually occcured as the action itself could have taken an
-- inordinate amount of time and that time isnt logged.
--
data RetryError a = RetryError (NonEmpty (RetryErrorInstance a)) deriving (Show, Eq)

renderRetryError :: (a -> T.Text) -> RetryError a -> T.Text
renderRetryError f (RetryError xs) = T.bracketedList "Repeated errors encountered on retries: ['" "']" "', '" . fmap (renderRetryErrorInstance f) . toList $ xs

data TransientRetryError t a =
    TransientRetryError (RetryError t)
  | NonTransientError a
    deriving (Show, Eq)

renderTransientRetryError :: (t -> T.Text) -> (a -> T.Text) -> TransientRetryError t a -> T.Text
renderTransientRetryError f _ (TransientRetryError re)  = renderRetryError f re
renderTransientRetryError _ f (NonTransientError e)     = f e

data RetryErrorInstance a =
    LastFailure Int a
  | RetryErrorInstance Int DurationQuantity a deriving (Show, Eq)

renderRetryErrorInstance :: (a ->T.Text) -> RetryErrorInstance a -> T.Text
renderRetryErrorInstance f (LastFailure n err)          = T.concat ["Giving up at attempt no. ", (T.pack . show) n, ": '", f err, "'"]
renderRetryErrorInstance f (RetryErrorInstance n q err) =
  let
    g :: Int -> Int
    g = id
  in T.concat ["Error on attempt no. ", (T.pack . show) n, ": '", f err, "', retrying again in: ", (T.pack . show . g . inMicroseconds) q]

attemptNo :: RetryErrorInstance a -> Int
attemptNo (LastFailure x _)           = x
attemptNo (RetryErrorInstance x _ _)  = x

failureInterval :: RetryErrorInstance a -> Maybe DurationQuantity
failureInterval (LastFailure _ _) = Nothing
failureInterval (RetryErrorInstance _ dq _) = pure dq

-- |
-- Retry policy takes an iteration number and returns the delay in seconds.
-- Returns a Deterministic delay.  If it returns Nothing then it gives up,
-- If it returns a number it will delay that number of seconds
--
newtype RetryPolicy = RetryPolicy { getRetryPolicy :: Int -> Maybe DurationQuantity }

instance Monoid RetryPolicy where
--mempty :: a
  mempty = RetryPolicy . const . pure . fmicroseconds $ 0

--mappend :: a -> a -> a
  mappend (RetryPolicy f) (RetryPolicy g) = RetryPolicy $ add <$> f <*> g

--uniformFromRetryPolicy
--    :: (Monad m, MonadIO m)
--    => RetryPolicy
--    -> StochasticRetryPolicy m
--uniformFromRetryPolicy (RetryPolicy f) = StochasticRetryPolicy $ do
--    md <- asks $ f
--    flip (maybe (pure Nothing)) md $ \d -> do
--        d' <- lift $ oneOfThese $ 0 :| [1..(abs d)]
--        return . pure $ d'

-- Policies

-- |
-- caps the number of retries
--
retryLimit :: Int -> RetryPolicy
retryLimit n = RetryPolicy $ \n' -> guarded (n > n') (fmicroseconds 0)

constantDelay :: DurationQuantity -> RetryPolicy
constantDelay = RetryPolicy . const . pure

exponentialBackoff :: DurationQuantity -> RetryPolicy
exponentialBackoff base = RetryPolicy $ \n -> pure . fmicroseconds . ((2^n) *) . finMicroseconds $ base

capDelay :: DurationQuantity -> RetryPolicy -> RetryPolicy
capDelay cap (RetryPolicy f) = let min' u v = fmicroseconds ((min `on` finMicroseconds) u v) in RetryPolicy $ fmap (min' cap) . f

-- helpers

fmicroseconds :: Integer -> DurationQuantity
fmicroseconds = microseconds

finMicroseconds :: DurationQuantity -> Integer
finMicroseconds = inMicroseconds

add :: Maybe DurationQuantity -> Maybe DurationQuantity -> Maybe DurationQuantity
add x y = (\u v -> fmicroseconds $ (max `on` finMicroseconds) u v) <$> x <*> y
