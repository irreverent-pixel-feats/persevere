{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Persevere.Control.Retry
-- Copyright    : (C) 2017-2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Persevere.Control.Retry (
    -- * Functions
    withRetries
  , retryOnTransients
  ) where

import Irreverent.Persevere.Data

import Irreverent.Quantity.Core.Data.Time

import Ultra.Control.Monad.Trans.Either (EitherT, pattern EitherT, hoistEither, runEitherT)

import Control.Concurrent

import Preamble

withRetries
  :: forall m e a b. (MonadIO m)
  => RetryPolicy
  -> (RetryErrorInstance e -> m b)
  -> EitherT e m a
  -> EitherT (RetryError e) m a
withRetries (RetryPolicy policy) f mx =
  let
    withRetries'
      :: Int
      -> [RetryErrorInstance e]
      -> EitherT (RetryError e) m a
    withRetries' n errs = EitherT $ do
      ex <- runEitherT mx
      case ex of
        Right x     -> return $ pure x
        Left err    -> do
          let md = policy n
          let lastFail = let l = LastFailure n err in f l >> (return . Left . RetryError $ l :| errs)
          flip (maybe lastFail) md $ \d -> do
            let i = RetryErrorInstance n d err
            _ <- f i
            liftIO . threadDelay . inMicroseconds $ d
            runEitherT $ withRetries' (n + 1) (i : errs)
  in withRetries' 0 []

retryOnTransients
  :: forall m e t e' a b. (MonadIO m)
  => RetryPolicy
  -> (RetryErrorInstance t -> m b)
  -> (e -> Either t e')
  -> EitherT e m a
  -> EitherT (TransientRetryError t e') m a
retryOnTransients p f trans mx =
  let
    splitError :: Either e c -> Either t (Either e' c)
    splitError = either (either Left (pure . Left) . trans) (pure . pure)

    recombineError :: Either (RetryError t) (Either e' c) -> Either (TransientRetryError t e') c
    recombineError = either (Left . TransientRetryError) (either (Left . NonTransientError) pure)

    mx' :: EitherT t m (Either e' a)
    mx' = lift (runEitherT mx) >>= hoistEither . splitError

  in lift (runEitherT $ withRetries p f mx') >>= hoistEither . recombineError
