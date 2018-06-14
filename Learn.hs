module Learn where

import Debug.Trace
import Data.List
import Data.Char
import Data.Binary
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Array.IArray
import qualified Data.Set as Set

import Utilities
import IndicatorPredicates
import Types
import Databases
import LengthFunctions
import Constraints
import qualified Evaluation
import qualified Parser as P
import qualified OldParser as OldP
import qualified Stemmer
import Benchmarks.Everyman
import Benchmarks.Guardian
import Data.Either

data AnswerTree = Text' String |
                Abbreviation' String |
                Synonym' String String |
                SynonymNeeded' String String |
                Anagram' String AnswerTree |
                Odds' String |
                Evens' String |
                ExampleOf' String |
                HiddenWord' String |
                ReversedHiddenWord' String |
                SubText' SubText String AnswerTree |
                Duplicate' String |
                Homophone' String AnswerTree |
                Reversal' String AnswerTree |
                Insertion' String AnswerTree AnswerTree |
                Subtraction' String |
                Charade' String AnswerTree AnswerTree
              deriving (Eq, Ord, Show)

index' :: Int -> Array Int [String] -> [String]
index' i table
  = if i < mn || i > mx then [] else table ! i
  where (mn, mx) = bounds table

-- The bool tells us whether we should look for new synonyms or not
match :: String -> Bool -> SynonymTable -> ParseTree -> [AnswerTree]
match s b st (Text t)
  | s == letters = [Text' s]
  | otherwise    = []
  where letters = filter (not . isSpace) (concat t)
match s b st (Abbreviation a)
  | elem s (abbreviations letters) = [Abbreviation' letters]
  | otherwise                      = []
  where letters = unwords a
match s b st (Synonym s')
  | syncond   = [Synonym' s letters]
  | othercond = [SynonymNeeded' s letters]
  | otherwise = []
  where letters   = unwords s'
        syncond   = elem s (index' (length s) (fromMaybe (error s) (lookup s' st)))
        othercond = b && isInWordlist s
match s b st (Anagram ws ind t)
  | any (sameLetters s) letters = [Anagram' s at]
  | otherwise                   = []
  where (letters, at) = gatherLetters t
match s b st (Odds ws ind op)
  | s == odds letters = [Odds' s]
  | otherwise         = []
  where letters = filter (not . isSpace) (concat op)
match s b st (Evens ws ind op)
  | s == evens letters = [Evens' s]
  | otherwise          = []
  where letters = filter (not . isSpace) (concat op)
match s b st (ExampleOf ws ind op)
  = match s b st (Synonym op) -- TODO check this
match s b st (HiddenWord ws ind op)
  | elem s (substrings letters) = [HiddenWord' s]
  | otherwise                   = []
  where letters = filter (not . isSpace) (concat op)
match s b st (ReversedHiddenWord ws ws' ind ind' op)
  | (not . null) recurse = [ReversedHiddenWord' s]
  | otherwise            = []
  where reversed = map reverse (reverse op)
        recurse  = match s b st (HiddenWord ws ind reversed)
match s b st (SubText ws ind (Text txt))
  = matchSubtext s b FirstLetters txt ++
    matchSubtext s b LastLetters txt ++
    matchSubtext s b RemoveMiddle txt ++
    matchSubtext s b RemoveEnds txt
  where
    matchSubtext str bool FirstLetters text
      | elem str things = [SubText' FirstLetters str (Text' str)]
      | otherwise       = []
      where things = map concat $ transpose (map firstLetters rightLenTxt)
            shortestLen = minimum (map length text)
            rightLenTxt = map (take shortestLen) text
    matchSubtext str bool LastLetters text
      | elem str things = [SubText' LastLetters str (Text' str)]
      | otherwise       = []
      where things = map concat $ transpose (map lastLetters rightLenTxt)
            shortestLen = minimum (map length text)
            rightLenTxt = map (\n -> drop ((length n) - shortestLen - 1) n) text
    matchSubtext str bool RemoveMiddle text
      | elem str things = [SubText' RemoveMiddle str (Text' str)]
      | otherwise       = []
      where things = concatMap removeMiddle text
    matchSubtext str bool RemoveEnds text
      | elem str things = [SubText' RemoveEnds str (Text' str)]
      | otherwise       = []
      where things = concatMap removeEnds text
match s b st (SubText ws ind t)
  = []
match s b st (Duplicate ws ws' ws'' ind op)
  | s == double = [Duplicate' s]
  | otherwise   = []
  where letters = filter (not . isSpace) (concat op)
        double  = letters ++ letters
match s b st (Homophone ws ind t)
  = [Homophone' s recurse | recurse <- concat [match x b st t | x <- homophones s]]
match s b st (Reversal ws ind t)
  = [Reversal' s recurse | recurse <- match (reverse s) b st t]
match s b st (Insertion ws ind t t')
  = [Insertion' s m m' |
      (s1, s2, s3) <- split3 s,
      m  <- match s2 b st t,
      m' <- match (s1 ++ s3) b st t']
match s b st tree@(Subtraction ws ind t t')
  = []{-if elem s (map getResult results) then [Subtraction' s] else []
  where results = Evaluation.evalTree st tree constraints True
        constraints = Constraints Nothing (Just n) (Just n)
        n = length s -}
    {-}= [Subtraction' s m m' |
      (s1, s2) <- split2 s ++ [("", s), (s, "")],
      si <- filter (\w -> isPrefixOf s1 w && isSuffixOf s2 w) (Set.toList allWords),
      let mid = fromJust $ stripSuffix s2 (fromJust (stripPrefix s1 si)),
      m  <- match mid b st t,
      m' <- match si b st t']-}
match s b st (Charade ws ind t t')
  = [Charade' s m m' |
      (l, r) <- split2 s,
      m  <- match l b st t,
      m' <- match r b st t']

data SubText = FirstLetters |
             LastLetters |
             RemoveMiddle |
             RemoveEnds
             deriving (Eq, Ord, Show)

subtextInd :: ClueText -> Maybe SubText
subtextInd ind
  = subtextInd' 0 ind
  where subtextInd' 0 ind'
          | firstLettersInd ind' = Just FirstLetters
          | lastLettersInd ind'  = Just LastLetters
          | removeMiddleInd ind' = Just RemoveMiddle
          | removeEndsInd ind'   = Just RemoveEnds
          | otherwise = subtextInd' 1 (map Stemmer.stem ind')
        subtextInd' 1 ind'
          | firstLettersInd ind' = Just FirstLetters
          | lastLettersInd ind'  = Just LastLetters
          | removeMiddleInd ind' = Just RemoveMiddle
          | removeEndsInd ind'   = Just RemoveEnds
          | otherwise = Nothing

gatherLetters :: ParseTree -> ([String], AnswerTree)
gatherLetters (Text ct) = ([filter (not . isSpace) (concat ct)], Text' (unwords ct))
gatherLetters (Abbreviation ct) = (abbreviations $ filter (not . isSpace) (concat ct), Abbreviation' (unwords ct))
gatherLetters (Charade ws ind t t')
  = (l ++ r, Charade' (unwords ws) at at')
  where (l, at)  = gatherLetters t
        (r, at') = gatherLetters t'
gatherLetters _ = error "Shouldn't be here"

earlyFinishMap f []
  = []
earlyFinishMap f (x : xs)
  = if (not . null) d then d else earlyFinishMap f xs
  where d = f x

getAllParses :: Clue -> (SynonymTable, [ParseTree])
getAllParses c = (st, map (\(_,_,_,t) -> t) ps)
  where (ps, st) = P.allParses c

getMostParses :: Clue -> (SynonymTable, [ParseTree])
getMostParses c = (st, map (\(_,_,_,t) -> t) ps)
  where (ps, st) = P.parsesWithoutSynonymLengths Always c

getSomeParses :: Clue -> (SynonymTable, [ParseTree])
getSomeParses c = (st, map (\(_,_,_,t) -> t) ps)
  where (ps, st) = P.parses Always c

getSomeParsesOld :: Clue -> (SynonymTable, [ParseTree])
getSomeParsesOld c = (st, map (\(_,_,_,t) -> t) ps)
  where (ps, st) = OldP.parses Always c

testClue :: Clue -> String -> Bool -> Either [[(String, String)]] [AnswerTree]
testClue clue ans b
  = if b then Left (listSynonymsToGet work) else Right work
  where lans = map toLower ans
        (st, ps) = getMostParses clue
        work = earlyFinishMap (match lans b st) (take 1000 ps)

results = [(clue, answer, concatMap (match answer False st) ps) |
  (clue, upAnswer) <- everyman,
  let answer = map toLower upAnswer,
  let (st, ps) = getSomeParses clue]

otherResults = [if null res then concatMap (match answer True st) ps else [] |
  (clue, answer, res) <- results,
  let (st, ps) = getMostParses clue]

listSynonymsToGet :: [AnswerTree] -> [[(String, String)]]
listSynonymsToGet ats
  = map snd $ filter ((< 3) . fst) $ sort $ map countNeededSynonyms ats

countNeededSynonyms :: AnswerTree -> (Int, [(String, String)])
countNeededSynonyms (SynonymNeeded' s s')
  = (1, [(s, s')])
countNeededSynonyms (SubText' _ _ at)
  = countNeededSynonyms at
countNeededSynonyms (Homophone' _ at)
  = countNeededSynonyms at
countNeededSynonyms (Reversal' _ at)
  = countNeededSynonyms at
countNeededSynonyms (Insertion' _ at at')
  = (l + l', r ++ r')
  where
    (l, r)   = countNeededSynonyms at
    (l', r') = countNeededSynonyms at'
-- countNeededSynonyms (Subtraction' _ at at')
--   = (l + l', r ++ r')
--   where
--     (l, r)   = countNeededSynonyms at
--     (l', r') = countNeededSynonyms at'
countNeededSynonyms (Charade' _ at at')
  = (l + l', r ++ r')
  where
    (l, r)   = countNeededSynonyms at
    (l', r') = countNeededSynonyms at'
countNeededSynonyms _
  = (0, [])
