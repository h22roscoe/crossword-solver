module Evaluation where

import Debug.Trace
import Data.List
import Data.Char
import Data.Binary
import Data.List
import Data.Maybe
import Data.Array.IArray
import qualified Data.Map

import IndicatorPredicates
import Utilities
import Types
import Databases
import LengthFunctions
import Constraints
import Stemmer

filterPref :: [(String, String)] -> Constraints -> [(String, String)]
filterPref [] c
  = []
filterPref (p@(s1, s2) : ps) c
  | satisfiesPre c s1 = p : filterPref ps c
  | otherwise = []

-- The raw string should be the first item returned, provided it satisfies the
-- constraints (length only, consistent with those from the table)
-- Note: we only pull out words whose length satisfies constraints
lookupWord :: SynonymTable -> String -> Constraints -> [String]
lookupWord synTable s c
  | satisfiesLen c n = s : tableSyns
  | otherwise        = tableSyns
  where
    n = length s
    tableSyns = index (fromMaybe (error s) (lookup (words s) synTable))
    index :: Array Int [String] -> [String]
    index table
      = concat [table ! n | n <- [max mn i..min mx j]]
      where
        (mn, mx) = bounds table
        i = fromMaybe mn (minConstraint c)
        j = fromMaybe mx (maxConstraint c)

-- WARNING: You have to stem the indicator when there is a stem cache...
-- TO DO: the indicators overlap, so the explanation may be wrong, e.g.
-- The clue is: ("Not fed partly twigged",5)
-- I think the answer is supposed to mean "twigged"
-- I think we have the concatenation of 2 sub-clues
--    1: "not" is verbatim text
--    2: I think "partly" indicates 'take first letter(s)'
--          The text "fed" resolves to "federal"
--       The result is then "ed"
-- Joining the results together we obtain "noted"
-- So the final answer is "NOTED"
subtext ind s
  = concat [stringOp s | (indTest, stringOp) <- ops, indTest (map stem ind)]
  where
    ops = [(firstLettersInd, firstLetters),
           (lastLettersInd, lastLetters),
           (removeMiddleInd, removeMiddle),
           (removeEndsInd, removeEnds)]

evaluate :: SynonymTable -> [Parse] -> [Answer]
evaluate synTable ts
  = concatMap eval (zip [0..] ts)
  where
    nts = length ts
    eval (k, parse@((clue, n), def, ind, tree))
      -- Uncomment to get the parse index...
      = trace (show k ++ "/" ++ show nts) $ [(getResult res, parse, res) | res <- results]
      -- = [(getResult res, parse, res) | res <- results]
      where
        results = evalTree synTable
                           tree
                           (Constraints (Just "") (Just n) (Just n))
                           True

makeAnagrams :: String -> Constraints -> Bool -> [String]
makeAnagrams s c isTopLevel
  | satisfiesLen c n =
      if isTopLevel
      then fromMaybe [] (Data.Map.lookup (sort s) sortedWordlist) \\ [s]
      else if n <= 10
           then anagrams s
           else []
  | otherwise = []
  where
    n = length s

-- Anagrams do a sorted lookup if the result is the solution (the Bool arg)...
--
evalTree :: SynonymTable -> ParseTree -> Constraints -> Bool -> [ResultTree]
evalTree synTable t c isTopLevel
  = filter (satisfies c . getResult) result
  where
    result = evalTree' t
    evalTree' (Text ws)
      = [Text (ws, concat ws)]
    evalTree' (Abbreviation ws)
      = [Abbreviation (ws, s) | s <- abbreviations (unwords ws)]
    evalTree' (Synonym ws)
      = [Synonym (ws, s) | s <- lookupWord synTable (unwords ws) c]
    -- Words of length 11 or more take too long (unless at top level)...
    -- Need to reset prefix as we have yet to rearrange the letters...
    evalTree' (Anagram txt ind t)
      = [Anagram (txt, anag) ind res | res <- evalTree synTable t (resetPrefix c) True,
                                       let s = getResult res,
                                       anag <- makeAnagrams s c isTopLevel]
    evalTree' (Odds txt ind ws)
      = [Odds (txt, odds s) ind ws]
      where
        s = filter (not . isSpace) (concat ws)
    evalTree' (Evens txt ind ws)
      = [Evens (txt, evens s) ind ws]
      where
        s = filter (not . isSpace) (concat ws)
-- The hidden word must span all words in the raw text
-- Looking for a hidden word in the reversal of a set of words ws is the
-- same as looking for a word in map reverse (reverse ws).
    -- Pre: length ws > 1
    evalTree' (HiddenWord txt ind ws)
      = [HiddenWord (txt, s ++ concat m ++ p) ind ws |
           s <- suffixes l,
           p <- prefixes r]
      where
        (l, m, r) = extractMiddle ws
    evalTree' (ReversedHiddenWord txt txt' ind ind' ws)
      = [ReversedHiddenWord (txt, s) (txt', concat ws') ind ind' ws |
           res <- evalTree' (HiddenWord txt ind ws'),
           let s = getResult res]
      where
        ws' = map reverse (reverse ws)
    evalTree' (ExampleOf txt ind ws)
      = [ExampleOf (txt, getResult res) ind ws |
           res <- evalTree' (Synonym ws)]
-- The concatMap is REALLY slow here...
-- Need to add some special cases, e.g. headless, head of...
    evalTree' (SubText txt ind (Text ws))
      = [SubText (txt, s) ind (Text (ws, concat ws)) |
           s <- leftToRight synTable ws c (subtext ind)]
    evalTree' (SubText txt ind t)
      = concatMap buildRes (evalTree synTable t noConstraints False)
      where
        buildRes res = [SubText (txt, s) ind res |
                          s <- (subtext ind) (getResult res)]
-- This does arbitrary expansion of the (singular) word. If we allow anagrams
-- of duplications then the word has to be verbatim text, e.g. double gin
-- mixed... This requires another constructor or some other way of controlling
-- how duplications are evaluated.
    evalTree' (Duplicate txt singular _ ind ws)
       = [Duplicate (txt, s) (singular, s1) (singular, s2) ind ws |
            Charade (_, s) ind res1 res2 <- evalTree' (Charade txt ind t t),
            let s1 = getResult res1,
            let s2 = getResult res2]
       where
         t = Synonym singular
    evalTree' (Homophone txt ind t@(Synonym ws))
      =  [Homophone (txt, s) ind t' |
            s <- leftToRight synTable ws c homophones] ++
         concatMap buildRes (evalTree synTable t noConstraints False)
      where
        t' = Synonym (ws, concat ws)
        buildRes res = [Homophone (txt, s) ind res |
                          s <- homophones (getResult res)]
    evalTree' (Reversal txt ind t)
      = map buildRes (evalTree synTable t (resetPrefix c) False)
      where
        buildRes res = Reversal (txt, reverse (getResult res)) ind res
-- insertInto has been unfolded so we can use satisfiesPre(fix) before
-- generating the insertion texts
    evalTree' (Insertion txt ind t t')
      = [Insertion (txt, s1 ++ s ++ s2) ind res res' |
           let minL = minLength t synTable,
           let maxL = maxLength t synTable,
           res' <- evalTree synTable t'
                     ((adjustMin 2 . (>>- minL) . (<<- maxL) . resetPrefix) c) False,
           let s' = getResult res',
           let n = length s',
           (s1, s2) <- split2 s',
           satisfiesPre c s1,
           res <- evalTree synTable t (c +++ s1 <<< n) False,
           let s = getResult res]
-- Evaluating t' first appears to be much slower, so it stays this way round...
    evalTree' (Subtraction txt ind t t')
      = [Subtraction (txt, str) ind res res' |
           let minL = minLength t' synTable,
           let maxL = maxLength t' synTable,
           res <- evalTree synTable t noConstraints False,
           let s = getResult res,
           let n = length s,
           res' <- evalTree synTable t' (resetPrefix c >>> n) False,
           let s' = getResult res',
           str <- subtractFrom s s']
    evalTree' (Charade txt ind t t')
      = charade txt ind synTable t t' c

-- Just work left to right...
charade txt ind synTable t t' c
  = [Charade (txt, s ++ s') ind res res' |
       res <- evalTree synTable t (((>>- minL) . (<<- maxL)) c) False,
       let s = getResult res,
       let n = length s,
       res' <- evalTree synTable t' (c <<< n +++ s) False,
       let s' = getResult res']
  where
    minL = minLength t' synTable
    maxL = maxLength t' synTable

-- Apply op to all words from left to right. This is similar to charade above.
leftToRight synTable [w] c op
  = [s | s <- op w, satisfies c s]
leftToRight synTable (w : ws) c op
  = [s' ++ s'' | s' <- op w,
                 satisfiesPre c s',
                 s'' <- leftToRight synTable ws (c <<< length s' +++ s') op]
