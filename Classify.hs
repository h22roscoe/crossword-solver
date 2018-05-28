{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Classify where

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.String.Utils

data Indicator = Anagram
               | Odds
               | Evens
               | HiddenWord
               | Subtext
               | Duplicate
               | Homophone
               | ExampleOf
               | Reversal
               | Insertion
               | Subtraction
               | Charade
               | None
               deriving (Eq, Ord, Enum, Show)

baseURL :: String
baseURL = "http://127.0.0.1:5000/classify/"

cutoff :: [Double] -> [Indicator]
cutoff vals
  = map fst $ filter snd $ zip inds $ zipWith (\v c -> v > c) vals cutoffs
  where cutoffs = repeat 10.0
        inds    = enumFrom (toEnum 0) :: [Indicator]

classify :: String -> IO (Maybe [Indicator])
classify ind
  = do
      ret <- fmap decode (simpleHttp $ baseURL ++ ind)
      if isJust ret then
        return $ Just $ cutoff (fromJust ret)
      else
        return Nothing
