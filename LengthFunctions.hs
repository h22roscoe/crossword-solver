module LengthFunctions where

import Data.Maybe
import Data.Array.IArray
import Types
import Databases
import Indicators
import Utilities

------------ LENGTH EVALUATION FUNCTIONS -----------------

minLength (Text ws) table
  = length (concat ws)
minLength (Abbreviation ws) table
  = minimum (map length (abbreviations (unwords ws)))
minLength (Synonym ws) table
  = fst (bounds (maybe (error (unwords ws)) id (lookup ws table)))
minLength (Anagram _ _ t) table
  = minLength t table
minLength (Odds _ _ ws) table
  = length (odds (concat ws))
minLength (Evens _ _ ws) table
  = length (evens (concat ws))
minLength (ExampleOf _ _ ws) table
  = minLength (Synonym ws) table
minLength (SubText _ _ t) table
  = 1
-- Recall: we want the singular version of the operand...
minLength (Duplicate _ ws _ _ _) table
  = 2 * minLength (Synonym ws) table
minLength (Homophone _ _ t) table
  = 1
minLength (Reversal _ _ t) table
  = minLength t table
minLength (HiddenWord _ _ ws) table
  = 2
minLength (ReversedHiddenWord _ _ _ _ ws) table
  = 2
minLength (Insertion _ _ t t') table
  = (minLength t table) + (minLength t' table)
minLength (Subtraction _ _ t t') table
  = max (minLength t' table - maxLength t table) 1
minLength (Charade _ _ t t') table
  = (sum . map (flip minLength table)) [t, t']

maxLength (Text ws) table
  = length (concat ws)
maxLength (Abbreviation ws) table
  = maximum (map length (abbreviations (unwords ws)))
maxLength (Synonym ws) table
  = snd (bounds (maybe (error (unwords ws)) id (lookup ws table)))
maxLength (Anagram _ _ t) table
  = maxLength t table
maxLength (Odds _ _ ws) table
  = length (odds (concat ws))
maxLength (Evens _ _ ws) table
  = length (evens (concat ws))
maxLength (HiddenWord _ _ ws) table
  = length (concat ws) - 2
maxLength (ReversedHiddenWord _ _ _ _ ws) table
  = length (concat ws) - 2
maxLength (ExampleOf _ _ ws) table
  = maxLength (Synonym ws) table
maxLength (SubText _ _ t) table
  = maxLength t table - 1
maxLength (Duplicate _ ws _ _ _) table
  = 2 * maxLength (Synonym ws) table
maxLength (Homophone _ _ t) table
  = 11
maxLength (Reversal _ _ t) table
  = maxLength t table
maxLength (Insertion _ _ t t') table
  = (maxLength t table) + (maxLength t' table)
maxLength (Subtraction _ _ t t') table
  = max (maxLength t' table - minLength t table) 1
maxLength (Charade _ _ t t') table
  = (sum . map (flip maxLength table)) [t, t']

minLengthMaybe :: ParseTree -> SynonymTable -> Maybe Int
minLengthMaybe (Synonym ws) table
  = Just 1
minLengthMaybe (Anagram _ _ t) table
  = minLengthMaybe t table
minLengthMaybe (ExampleOf _ _ ws) table
  = Just 1
minLengthMaybe (Duplicate _ _ _ _ _) _
  = Just 2
minLengthMaybe (Reversal _ _ t) table
  = minLengthMaybe t table
minLengthMaybe (Insertion _ _ t t') table
  | isJust x && isJust x' = Just $ (fromJust x) + (fromJust x')
  | otherwise             = Just 2
  where
    x  = minLengthMaybe t table
    x' = minLengthMaybe t' table
minLengthMaybe (Subtraction _ _ t t') table
  | isJust x && isJust x' = Just $ max ((fromJust x') - (fromJust x)) 1
  | otherwise             = Just 1
  where
    x  = maxLengthMaybe t table
    x' = minLengthMaybe t' table
minLengthMaybe (Charade _ _ t t') table
  | isJust x && isJust x' = Just $ (fromJust x) + (fromJust x')
  | otherwise             = Just 2
  where
    x  = minLengthMaybe t table
    x' = minLengthMaybe t' table
minLengthMaybe pt table
  = Just $ minLength pt table

maxLengthMaybe :: ParseTree -> SynonymTable -> Maybe Int
maxLengthMaybe (Synonym ws) table
  = Nothing
maxLengthMaybe (Anagram _ _ t) table
  = maxLengthMaybe t table
maxLengthMaybe (ExampleOf _ _ ws) table
  = Nothing
maxLengthMaybe (SubText _ _ t) table
  | isJust x  = Just $ (fromJust x) - 1
  | otherwise = Nothing
  where
    x = maxLengthMaybe t table
maxLengthMaybe (Duplicate _ _ _ _ _) table
  = Nothing
maxLengthMaybe (Reversal _ _ t) table
  = maxLengthMaybe t table
maxLengthMaybe (Insertion _ _ t t') table
  | isJust x && isJust x' = Just $ (fromJust x) + (fromJust x')
  | otherwise             = Nothing
  where
    x  = maxLengthMaybe t table
    x' = maxLengthMaybe t' table
maxLengthMaybe (Subtraction _ _ t t') table
  | isJust x && isJust x' = Just $ max ((fromJust x') - (fromJust x)) 1
  | otherwise             = Nothing
  where
    x  = minLengthMaybe t table
    x' = maxLengthMaybe t' table
maxLengthMaybe (Charade _ _ t t') table
  | isJust x && isJust x' = Just $ (fromJust x) + (fromJust x')
  | otherwise             = Nothing
  where
    x  = maxLengthMaybe t table
    x' = maxLengthMaybe t' table
maxLengthMaybe pt table
  = Just $ maxLength pt table
