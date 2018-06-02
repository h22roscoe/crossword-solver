module Learn where

import Debug.Trace
import Data.List
import Data.Char
import Data.Binary
import Data.List
import Data.List.Extra
import Data.Maybe
import qualified Data.Set as Set

import Utilities
import IndicatorPredicates
import Types
import Databases
import LengthFunctions
import qualified Evaluation
import qualified Parser as P
import qualified OldParser as OldP
import qualified Stemmer
import Benchmarks.Everyman
import Benchmarks.Guardian
import Data.Either

data AnswerTree = Text' String |
                Abbreviation' String |
                Synonym' String |
                SynonymNeeded' String String |
                Anagram' String |
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
                Subtraction' String AnswerTree AnswerTree |
                Charade' String AnswerTree AnswerTree
              deriving (Eq, Ord, Show)

-- The bool tells us whether we should look for new synonyms or not
match :: String -> Bool -> ParseTree -> [AnswerTree]
match s b (Text t)
  | s == letters = [Text' s]
  | otherwise    = []
  where letters = filter (not . isSpace) (concat t)
match s b (Abbreviation a)
  | elem s (abbreviations letters) = [Abbreviation' letters]
  | otherwise                      = []
  where letters = unwords a
match s b (Synonym s')
  | syncond   = [Synonym' letters]
  | othercond = [SynonymNeeded' s letters]
  | otherwise = []
  where letters   = unwords s'
        syncond   = elem s (synonyms letters)
        othercond = b && isInWordlist s
match s b (Anagram ws ind t)
  | any (sameLetters s) (gatherLetters t) = [Anagram' s]
  | otherwise                             = []
match s b (Odds ws ind op)
  | s == odds letters = [Odds' s]
  | otherwise         = []
  where letters = filter (not . isSpace) (concat op)
match s b (Evens ws ind op)
  | s == evens letters = [Evens' s]
  | otherwise          = []
  where letters = filter (not . isSpace) (concat op)
match s b (ExampleOf ws ind op)
  = match s b (Synonym op) -- TODO check this
match s b (HiddenWord ws ind op)
  | elem s (substrings letters) = [HiddenWord' s]
  | otherwise                   = []
  where letters = filter (not . isSpace) (concat op)
match s b (ReversedHiddenWord ws ws' ind ind' op)
  | (not . null) recurse = [ReversedHiddenWord' s]
  | otherwise            = []
  where reversed = map reverse (reverse op)
        recurse  = match s b (HiddenWord ws ind reversed)
match s b (SubText ws ind (Text txt))
  = if isJust subtextType then matchSubtext s b (fromJust subtextType) txt else []
  where
    subtextType = subtextInd ind
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
match s b (SubText ws ind t)
  = []
match s b (Duplicate ws ws' ws'' ind op)
  | s == double = [Duplicate' s]
  | otherwise   = []
  where letters = filter (not . isSpace) (concat op)
        double  = letters ++ letters
match s b (Homophone ws ind t)
  = [Homophone' s recurse | recurse <- concat [match x b t | x <- (s : (homophones s))]]
match s b (Reversal ws ind t)
  = [Reversal' s recurse | recurse <- match (reverse s) b t]
match s b (Insertion ws ind t t')
  = [Insertion' s m m' |
      (s1, s2, s3) <- split3 s,
      m  <- match s2 b t,
      m' <- match (s1 ++ s3) b t']
match s b (Subtraction ws ind t t')
  = []{-}= [Subtraction' s m m' |
      (s1, s2) <- split2 s ++ [("", s), (s, "")],
      si <- filter (\w -> isPrefixOf s1 w && isSuffixOf s2 w) (Set.toList allWords),
      let mid = fromJust $ stripSuffix s2 (fromJust (stripPrefix s1 si)),
      m  <- match mid b t,
      m' <- match si b t']-}
match s b (Charade ws ind t t')
  = [Charade' s m m' |
      (l, r) <- split2 s,
      m  <- match l b t,
      m' <- match r b t']

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

gatherLetters :: ParseTree -> [String]
gatherLetters (Text ct) = [filter (not . isSpace) (concat ct)]
gatherLetters (Abbreviation ct) = abbreviations $ filter (not . isSpace) (concat ct)
gatherLetters (Charade ws ind t t') = [l ++ r | l <- gatherLetters t, r <- gatherLetters t']
gatherLetters _ = error "Shouldn't be here"

earlyFinishMap f []
  = []
earlyFinishMap f (x : xs)
  = if (not . null) d then d else earlyFinishMap f xs
  where d = f x

getAllParses :: Clue -> [ParseTree]
getAllParses c = map (\(_,_,_,t) -> t) ps
  where (ps, _) = P.allParses c

getMostParses :: Clue -> [ParseTree]
getMostParses c = map (\(_,_,_,t) -> t) ps
  where (ps, _) = P.parsesWithoutSynonymLengths Always c

getSomeParses :: Clue -> [ParseTree]
getSomeParses c = map (\(_,_,_,t) -> t) ps
  where (ps, _) = P.parses Always c

getSomeParsesOld :: Clue -> [ParseTree]
getSomeParsesOld c = map (\(_,_,_,t) -> t) ps
  where (ps, _) = OldP.parses Always c

testClue :: Clue -> String -> Bool -> Either [[(String, String)]] [AnswerTree]
testClue clue ans b
  = if b then Left (listSynonymsToGet work) else Right work
  where lans = map toLower ans
        work = earlyFinishMap (match lans b) $ getMostParses clue

results = [(clue, answer, concatMap (match answer False) ps) |
  (clue, upAnswer) <- everyman,
  let answer = map toLower upAnswer,
  let ps = getSomeParses clue]

otherResults = [if null res then concatMap (match answer True) ps else [] |
  (clue, answer, res) <- results,
  let ps = getMostParses clue]

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
countNeededSynonyms (Subtraction' _ at at')
  = (l + l', r ++ r')
  where
    (l, r)   = countNeededSynonyms at
    (l', r') = countNeededSynonyms at'
countNeededSynonyms (Charade' _ at at')
  = (l + l', r ++ r')
  where
    (l, r)   = countNeededSynonyms at
    (l', r') = countNeededSynonyms at'
countNeededSynonyms _
  = (0, [])
