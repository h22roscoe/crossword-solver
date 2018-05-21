import IndicatorPredicates
import Utilities
import Data.List
import Debug.Trace
import Types
import Databases

type Detail = ([String], Either String String)
type Words = [String]

data RT = Synonym Detail Words |
          Anagram Detail Words |
          FirstLetter Detail Words |
          LastLetter Detail Words |
          Odds Detail Words |
          Evens Detail Words |
          Duplicate Detail Words |
          ExampleOf Detail Words |
          IndirectSubText Detail RT |
          Homophone Detail RT |
          HiddenWord Detail Words |
          ReversedHiddenWord Detail Detail Words |
          Reversal Detail RT |
          Insertion Detail RT RT |
          Subtraction Detail RT RT |
          Charade Detail RT RT
        deriving (Eq, Ord, Show)

matches :: RT -> String -> Bool
matches (Synonym (_, Left s) ws) s'
  = s == s'
matches (Anagram (_, Left s) ws) s'
  = s == s'
matches (FirstLetter (_, Left s) ws) s'
  = s == s'
matches (LastLetter (_, Left s) ws) s'
  = s == s'
matches (Odds (_, Left s) ws) s'
  = s == s'
matches (Evens (_, Left s) ws) s'
  = s == s'
matches (Duplicate (_, Left s) ws) s'
  = s == s'
matches (ExampleOf (_, Left s) ws) s'
  = s == s'
matches (IndirectSubText (_, Left s) t) s'
  = s == s'
matches (Homophone (_, Left s) t) s'
  = s == s'
matches (HiddenWord (_, Left s) t) s'
  = s == s'
matches (ReversedHiddenWord (_, Left s) (_, Left s'') t) s'
  = s == s'
matches (Reversal (_, Left s) t) s'
  = s == s'
matches (Insertion (_, Left s) t t') s'
  = s == s'
matches (Subtraction (_, Left s) t t') s'
  = s == s'
matches (Charade (_, Left s) t t') s'
  = s == s'
matches (Synonym (_, Right s) ws) s'
  = s == s'
matches (Anagram (_, Right s) ws) s'
  = sameLetters s s'
matches (FirstLetter (_, Right s) ws) s'
  = s == s'
matches (LastLetter (_, Right s) ws) s'
  = s == s'
matches (Odds (_, Right s) ws) s'
  = s == s'
matches (Evens (_, Right s) ws) s'
  = s == s'
matches (Duplicate (_, Right s) ws) s'
  = s == s'
matches (ExampleOf (_, Right s) ws) s'
  = s == s'
--matches (SubText (_, Right s) t) s'
--  = s == s'
matches (IndirectSubText (_, Right s) t) s'
  = s == s'
matches (Homophone (_, Right s) t) s'
  = s == s'
matches (HiddenWord (_, Right s) t) s'
  = s == s'
matches (ReversedHiddenWord (_, Right s) (_, Right s'') t) s'
  = s == s'
matches (Reversal (_, Right s) t) s'
  = matches t (reverse s')
matches (Insertion (_, Right s) t t') s'
  = ins (getRes t) (getRes t')
  where
    ins (Left s1) (Right s2)
      = or [matches t' (l ++ r) | (l, r, _) <- splitOn (==) s1 s']
    ins (Right s1) (Left s2)
      = or [matches t s3 | (l, r, s3) <- splitOn sameLetters s1 s', s2 == l ++ r]
    ins (Right s1) (Right s2)
      = or [matches t s3 && matches t' (l ++ r) | (l, r, s3) <- splitOn sameLetters s1 s']
matches (Subtraction (_, Right s) t t') s'
  = sub (getRes t) (getRes t')
  where
    -- Note: s3 is subtext of the string we're matching against
    sub (Left s1) (Right s2)
      = or [matches t' (l ++ s1 ++ r) | (l, r, s3) <- splitOn (\x y -> True) "" s']
    sub (Right s1) (Left s2)
      = or [matches t s3 | (l, r, s3) <- splitOn sameLetters s1 s2, s' == l ++ r]
    sub (Right s1) (Right s2)
      = error "CAN'T DO!"
matches (Charade (_, Right s) t t') s'
  = matches t s'' && matches t' s'''
  where
    (s'', s''') = splitAt (length' s1) s'
    s1 = getRes t
    length' (Left s) = length s
    length' (Right s) = length s


rt1 = Insertion (["ins"], Right "tcartes") rt5 rt4
rt2 = (Synonym ([], Left "cat") ["pet"])
rt3 = (Synonym ([], Left "ster") ["fried"])
rt4 = (Anagram (["mixed"], Right "rtes") ["esrt"])
rt5 = (Anagram (["mixed"], Right "tca") ["act"])

rt6 = Subtraction (["sub"], Right "ster") rt10 rt8
rt7 = (Synonym ([], Left "xxd") ["pet"])
rt8 = (Synonym ([], Left "scatter") ["spread"])
rt9 = (Anagram (["mixed"], Right "raetcts") ["raetcts"])
rt10 = (Anagram (["mixed"], Right "tca") ["act"])

rt11 = Charade (["by"], Right "tracshore") rt13 rt12
rt12 = (Synonym ([], Left "horse") ["animal"])
rt13 = (Anagram (["mixed"], Right "trac") ["trac"])

getRes (Synonym (_, s) ws)
  = s
getRes (Anagram (_, s) ws)
  = s
getRes (FirstLetter (_, s) ws)
  = s
getRes (LastLetter (_, s) ws)
  = s
getRes (Odds (_, s) ws)
  = s
getRes (Evens (_, s) ws)
  = s
getRes (Duplicate (_, s) ws)
  = s
getRes (ExampleOf (_, s) ws)
  = s
getRes (IndirectSubText (_, s) t)
  = s
getRes (Homophone (_, s) t)
  = s
getRes (HiddenWord (_, s) ws)
  = s
getRes (ReversedHiddenWord (_, s) a ws)
  = s
getRes (Reversal (_, s) t)
  = s
getRes (Insertion (_, s) t t')
  = s
getRes (Subtraction (_, s) t t')
  = s
getRes (Charade (_, s) t t')
  = s
