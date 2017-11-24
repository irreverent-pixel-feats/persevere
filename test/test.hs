module Main where

import qualified Test.Irreverent.Persevere.Control.Retry
import qualified Test.Irreverent.Persevere.Data

import Lab.Core.Main

import Preamble

main :: IO ()
main = labMain
  [ Test.Irreverent.Persevere.Control.Retry.tests
  , Test.Irreverent.Persevere.Data.tests
  ]
