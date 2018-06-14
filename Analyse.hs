module Analyse where

import Types
import Learn
import Data.Char
import Benchmarks.HalfBenchmark
import Benchmarks.Guardian
import Benchmarks.ClueBank

newResults = [earlyFinishMap (match answer False st) ps' | -- Exploit laziness and dont go beyond first solve
  (clue, upAnswer) <- cluebank,--halfbenchmark,
  let answer = map toLower upAnswer,
  let (st, ps) = getSomeParses clue,
  let ps' = take 1000 ps]

oldResults = [earlyFinishMap (match answer False st) ps' | -- Exploit laziness and dont go beyond first solve
  (clue, upAnswer) <- cluebank,--halfbenchmark,
  let answer = map toLower upAnswer,
  let (st, ps) = getSomeParsesOld clue,
  let ps' = take 500 ps]

solved results = filter (not . null) results

allHelper results f = filter (\l -> any f l) (solved results)

alls results = map (allHelper results) [anagram, odds, evens, exampleOf, hiddenWord, reverseHiddenWord, duplicate, subtext, homophone, reversal, insertion, subtraction, charade]

lens results = zip ["Anagrams", "Odds", "Evens", "Eg of", "Hidden", "Rev hidden", "Duplicate", "Subtext", "Homophone", "Reversal", "Insertion", "Subtraction", "Charade"] (map length (alls results))

newSolved = solved newResults

oldSolved = solved oldResults

improvements = diffs newResults oldResults

diffs rsn rso
  = [(idx, ans) | (idx, ans) <- new, notIn idx old]
  where idxn = zip [0 ..] rsn
        idxo = zip [0 ..] rso
        new  = filter (not . null . snd) idxn
        old  = filter (not . null . snd) idxo
        notIn idx [] = True
        notIn idx ((i, _) : xs) = if idx == i then False else notIn idx xs

flatten :: AnswerTree -> [AnswerTree]
flatten x@(SubText' s s' at) = x : flatten at
flatten x@(Homophone' s at) = x : flatten at
flatten x@(Reversal' s at) = x : flatten at
flatten x@(Insertion' s at1 at2) = x : (flatten at1 ++ flatten at2)
-- flatten x@(Subtraction' s at1 at2) = x : (flatten at1 ++ flatten at2)
flatten x@(Charade' s at1 at2) = x : (flatten at1 ++ flatten at2)
flatten at = [at]

helper :: (AnswerTree -> Bool) -> AnswerTree -> Bool
helper f at = any f $ flatten at

anagram
  = helper anagramHelp
  where
    anagramHelp (Anagram' _ _) = True
    anagramHelp _ = False

odds
  = helper oddsHelp
  where
    oddsHelp (Odds' _) = True
    oddsHelp _ = False

evens
  = helper evensHelp
  where
    evensHelp (Evens' _) = True
    evensHelp _ = False

exampleOf
  = helper exampleOfHelp
  where
    exampleOfHelp (ExampleOf' _) = True
    exampleOfHelp _ = False

hiddenWord
  = helper hiddenWordHelp
  where
    hiddenWordHelp (HiddenWord' _) = True
    hiddenWordHelp _ = False

reverseHiddenWord
  = helper reverseHiddenWordHelp
  where
    reverseHiddenWordHelp (ReversedHiddenWord' _) = True
    reverseHiddenWordHelp _ = False

duplicate
  = helper duplicateHelp
  where
    duplicateHelp (Duplicate' _) = True
    duplicateHelp _ = False

subtext
  = helper subtextHelp
  where
    subtextHelp (SubText' _ _ _) = True
    subtextHelp _ = False

homophone
  = helper homophoneHelp
  where
    homophoneHelp (Homophone' _ _) = True
    homophoneHelp _ = False

reversal
  = helper reversalHelp
  where
    reversalHelp (Reversal' _ _) = True
    reversalHelp _ = False

insertion
  = helper insertionHelp
  where
    insertionHelp (Insertion' _ _ _) = True
    insertionHelp _ = False

subtraction
  = helper subtractionHelp
  where
    subtractionHelp (Subtraction' _) = True
    -- subtractionHelp (Subtraction' _ _ _) = True
    subtractionHelp _ = False

charade
  = helper charadeHelp
  where
    charadeHelp (Charade' _ _ _) = True
    charadeHelp _ = False
