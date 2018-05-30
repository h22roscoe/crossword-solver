{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Classify where

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.String.Utils
import System.IO.Unsafe
import Types (ClueText)
import qualified Data.Set as Set
import Utilities

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

type IndicatorTable = [(ClueText, [Indicator])]

baseURL :: String
baseURL = "http://127.0.0.1:5000/classify/"

cutoff :: [Double] -> [Indicator]
cutoff vals
  = map fst $ filter snd $ zip inds $ zipWith (\v c -> v > c) vals cutoffs
  where cutoffs = repeat 10.0
        inds    = enumFrom (toEnum 0) :: [Indicator]

classifyHelp :: String -> IO (Maybe [Indicator])
classifyHelp ind
  = do
      ret <- fmap decode (simpleHttp $ baseURL ++ ind)
      if isJust ret then
        return $ Just $ cutoff (fromJust ret)
      else
        return Nothing

classify :: String -> [Indicator]
classify ind
  = case unsafePerformIO (classifyHelp ind) of
      Just [None] -> []
      Just x -> x
      Nothing -> []

makeIndicatorTable :: ClueText -> IndicatorTable
makeIndicatorTable ct
  = map (\w -> (words w, if length (words w) < 5 then classify w else [])) allWs
  where allWs = Set.elems $ Set.fromList $ concatMap (map unwords) (partitions ct)
