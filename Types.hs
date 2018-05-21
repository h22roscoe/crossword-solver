module Types where

import Data.Array.IArray
import qualified Data.Map as Map

type ClueText = [String]

type Clue = (String, Int)

type Definition = ClueText

type Indicator = ClueText

type Operand = ClueText

data Condition = None | Always | Meaning String
               deriving (Eq, Show)

type Parse = (Clue, Definition, Indicator, ParseTree)

-- Duplicate houses the source text and two copies of the singular
-- version of the operand, e.g. hoop c.f. hoops. These will be
-- replaced by the two synonym instances, e.g. o and ring.
data Tree a = Text a |
              Abbreviation a |
              Synonym a |
              Anagram a Indicator (Tree a) |
              Odds a Indicator Operand |
              Evens a Indicator Operand |
              ExampleOf a Indicator Operand |
              HiddenWord a Indicator Operand |
              ReversedHiddenWord a a Indicator Indicator Operand |
              SubText a Indicator (Tree a) |
              Duplicate a a a Indicator Operand |
              Homophone a Indicator (Tree a) |
              Reversal a Indicator (Tree a) |
              Insertion a Indicator (Tree a) (Tree a) |
              Subtraction a Indicator (Tree a) (Tree a) |
              Charade a Indicator (Tree a) (Tree a)
            deriving (Eq, Ord, Show)

type ParseTree = Tree ClueText

type Solution = String

type ResultTree = Tree (ClueText, Solution)

type Pairs = [(ClueText, ClueText)]

type Triples = [(ClueText, ClueText, ClueText)]

type Answer = (String, Parse, ResultTree)

type SynonymTable = [(ClueText, (Array Int [String]))]

type StemCache = Map.Map [String] [String]
