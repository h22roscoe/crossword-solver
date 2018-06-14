{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Classify where

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.String.Utils
import Data.List
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
  = [ind | (ind, g) <- zip [Anagram ..] (map (> 10.0) vals), g]

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
  = map classifySmall subs
  where subs = nub $ (substrings ct) \\ [ct]
        classifySmall ws
          = (ws, if length ws < 4 then classify (unwords ws) else [])
