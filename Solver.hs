module Main where

import Debug.Trace
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import Data.Either
import Data.Array.IArray
import Control.Applicative
import Control.Monad

import Clues
import Parser
import Types
import Evaluation
import Databases
import Utilities
import IndicatorPredicates
import Stemmer
import Learn as L
import ReverseDictionary

main = error "No main"

--------------------------- EVALUATION ----------------------------

-- NOTE: The bool parameter determines whether or not we check if
-- a possible solution is in the word list.
solve c
  = displayOneSol c (filterSols table True (evaluate table ps) Nothing)
  where
    (ps, table) = parses Always c

solve' c
  = displayOneSol c (Just [a | a@(sol, (_, def, _, _), _) <- evaluate table ps, elem sol (lookUp def defs)])
  where
    (ps, table) = parses None c
    defs = getUniqueDefs c

backSolve c
  = [concat [L.match ans False st t | ans <- lookUp def defs] | (_, def, ind, t) <- ps]
  where
    (ps, st) = parses None c
    defs = getUniqueDefs c

backSolveHint c hint
  =  [concat [L.match ans False st t | ans <- lookUp def defs] | (_, def, ind, t) <- ps]
  where
    (ps, st) = parses None c
    defs = getUniqueDefsHint c hint

backSolve' c
  = [filter (\(_, _, _, l) -> (not.null) l) [(i, def, t, L.match ans False st t) | ans <- lookUp def defs] | (i, (_, def, ind, t)) <- zip [0..] ps]
  where
    (ps, st) = parses None c
    defs = getUniqueDefs c

getUniqueDefs c
  = [(d, filter ((== l) . length) (syns d)) | d <- defs]
  where
    syns d     = synonyms (unwords d)
    ct         = words $ cleanUp (fst c)
    l          = snd c
    defs       = nub [def | def <- substrings ct, end def ct, length def < 4]
    end def ct = isSuffixOf def ct || isPrefixOf def ct

getUniqueDefsHint c hint
  = [(d, filter hintFilter (filter ((== l) . length) (syns d))) | d <- defs]
  where
    hintFilter = createFilter hint
    syns d     = synonyms (unwords d)
    ct         = words $ cleanUp (fst c)
    l          = snd c
    defs       = nub [def | def <- substrings ct, end def ct, length def < 4]
    end def ct = isSuffixOf def ct || isPrefixOf def ct

createFilter :: String -> String -> Bool
createFilter [] []
  = True
createFilter ('*':cs) (_:ls)
  = createFilter cs ls
createFilter (c:cs) (l:ls)
  = if toLower c == l then createFilter cs ls else False

solveAll c
  = displayAllSols c (filterSols table True (evaluate table ps) Nothing)
  where
    (ps, table) = parses Always c

guess c
  = displayAllGuesses c (filterSols table False (evaluate table ps) Nothing)
  where
    (ps, table) = parses None c

-- def tells it the string for the definition
hint c def
  = displayAllGuesses c (filterSols table False (evaluate table ps) Nothing)
  where
    (ps, table) = parses (Meaning def) c

-- sol is the solution we're looking for
solveWordplay c sol
  = displayAllGuesses c (filterSols table False (evaluate table ps) (Just sol))
  where
    (ps, table) = parses None c

solveParseTree t@(c, _, _, _)
  = displayOneSol c (filterSols table True (evaluate table [t]) Nothing)
  where
    (ps, table) = parses None c

filterSols :: SynonymTable -> Bool -> [Answer] -> Maybe String -> Maybe [Answer]
filterSols synTable mustCheckSyn answers solution
  | null sols = Nothing
  | otherwise = Just sols
  where
    sols = filter check answers
    check :: Answer -> Bool
    check (sol, (clue, def, ind, t), res)
      | mustCheckSyn       = elem sol (syns (unwords def))
      | isNothing solution = isInWordlist sol
      | isJust solution    = sol == fromJust solution
    syns :: String -> [String]
    syns s
      = concat (elems (fromMaybe (error s) (lookup (words s) synTable)))

quote' s
  = "\"" ++ unwords s ++ "\""

quote s
  = "\"" ++ s ++ "\""

message s s'
  | s == s'   = quote s ++ " is verbatim text"
  | otherwise = "The text " ++ quote s ++ " resolves to " ++ quote s'

displayQuote :: String -> ClueText -> ClueText -> String -> Int -> Int -> IO()
displayQuote desc ind ws s n k
  = do
      indent n k ("I think " ++ quote' ind ++ " indicates " ++ desc ++ " with " ++
                  quote' ws ++ " as the target")
      indent n 1 ("The result is " ++ quote s)

indent n k s
  = putStrLn ((replicate (n * k) ' ') ++ s)

-- WARNING: You only need to stem the indicator when there is a stem cache...
subtextMessage ind
  | firstLettersInd ind' = "'take first letter(s)'"
  | lastLettersInd ind' = "'take last letter(s)'"
  | removeMiddleInd ind' = "'remove middle letter(s)'"
  | removeEndsInd ind' = "'remove start and end letter(s)'"
  where
    ind' = map stem ind

displayOneSol c
  = maybe (putStrLn ("The clue is: " ++ show c ++
                     "\nSorry, but I couldn't solve it\n"))
          (\sols -> (display c f  "is" (head sols)))
  where
    f meaning = "I think the answer is supposed to mean " ++ quote' meaning

displayAllSols c
  = maybe (putStrLn ("The clue is: " ++ show c ++
                     "\nSorry, but I couldn't solve it\n"))
          (\sols -> (mapM_ (display c f "is") sols))
  where
    f meaning = "I think the answer is supposed to mean " ++ quote' meaning

displayAllGuesses c
  = maybe (putStrLn ("The clue is: " ++ show c ++
                     "\nSorry, but I couldn't solve it\n"))
          (\sols -> (mapM_ (display c f "would be") sols))
  where
    f meaning = "I can't guarantee that the solution matches the definition" ++
                "\nHowever, if the meaning is " ++ quote' meaning ++
                " then from the rest of the clue..."

display c f linkText (sol, (_, meaning, link, parseTree), t)
  = do
      putStrLn ("The clue is: " ++ show c)
      putStrLn (f meaning)
      when (link /= []) $ putStrLn (quote' link ++ " separates the definition from the wordplay")
      displayTree t 0 1
      putStrLn ("So the final answer " ++ linkText ++
                " \"" ++ map toUpper sol ++ "\"\n------")

-- n is the indentation offset. k is a binary indicator: 0 means do not
-- indent the first line as we're sitting after a putStr; 1 means indent
-- as specified by the n. It's just for tidying up the formatting.
--
displayTree :: ResultTree -> Int -> Int -> IO ()
displayTree (Text (ws, s)) n k
  = do
      indent n k (quote s ++ " is verbatim text")
displayTree (Abbreviation (ws, s)) n k
  = do
      indent n k (quote s ++ " is an abbreviation of " ++ quote' ws)
displayTree (Synonym (ws, s)) n k
  = do
      indent n k (message (unwords ws) s)
displayTree (Anagram (txt, s) ind t) n k
  = do
      indent n k ("I think " ++ quote' ind ++ " indicates an anagram")
      displayTree t (n + 3) 1
      indent n 1 ("The required anagram is " ++ quote s)
displayTree (Odds (txt, s) ind ws) n k
  = displayQuote "'take odd-numbered letters'" ind ws s n k
displayTree (Evens (txt, s) ind ws) n k
  = displayQuote "'take even-numbered letters'" ind ws s n k
displayTree (HiddenWord (txt, s) ind ws) n k
  = displayQuote "a hidden word" ind ws s n k
displayTree (ReversedHiddenWord (txt, s) (txt', s') ind ind' ws) n k
  = do
      indent n k ("I think " ++ quote' ind ++ " indicates " ++
                "a hidden word")
      displayTree (Reversal (txt, s') ind' (Synonym (ws, concat ws))) n k
      indent n 1 ("This contains the letters " ++ quote s)
displayTree (ExampleOf (txt, s) ind ws) n k
  = displayQuote "an example of something" ind ws s n k
displayTree (SubText (txt, s) ind (Text (ws, s'))) n k
  = displayQuote (subtextMessage ind) ind ws s n k
displayTree (SubText (txt, s) ind t) n k
  = do
      indent n k ("I think " ++ quote' ind ++ " indicates " ++
                  subtextMessage ind)
      displayTree t (n + 3) 1
      indent n 1 ("The result is then " ++ quote s)
displayTree (Duplicate (txt, s) (txt', s1) (txt'', s2) ind ws) n k
  = do
      indent n k ("I think " ++ quote' ind ++ " indicates " ++
                "a duplication with " ++ quote' ws ++ " as the target")
      indent (n + 3) 1 ("The first resolves to " ++ quote s1)
      indent (n + 3) 1 ("The second resolves to " ++ quote s2)
      indent n 1 ("Combining them we get " ++ quote s)
displayTree (Homophone (txt, s) ind t) n k
  = do
      indent n k ("I think " ++ quote' ind ++ " indicates " ++
                "a homophone")
      displayTree t (n + 3) 1
      when (s /= s') $ indent n 1 (quote s' ++ " is a homophone of " ++ quote s)
  where
    s' = getResult t
displayTree (Reversal (txt, s) ind t) n k
  = do
      indent n k ("I think " ++ quote' ind ++ " indicates " ++
                "a reversal")
      displayTree t (n + 3) 1
      indent n 1 ("We now need to reverse the result, giving " ++ quote s)
displayTree (Insertion (txt, s) ind t t') n k
  = do
      indent n k ("I think " ++ quote' ind ++ " indicates " ++
                "the insertion of one word into another")
      displayTrees (n + 3) [t, t']
      indent n 1 ("For the insertion, the word " ++ quote s1 ++
                " needs to be inserted into the word " ++ quote s2)
      indent n 1 ("The required result is " ++ quote s)
  where
    s1 = getResult t
    s2 = getResult t'
displayTree (Subtraction (txt, s) ind t t') n k
  = do
      indent n k ("I think " ++ quote' ind ++ " indicates " ++
                "the removal of one word from another")
      -- mapM_ (displayTrees (n + 3) 1) (zip [1..] [t, t'])
      displayTrees (n + 3) [t, t']
      indent n 1 ("For the removal, the word " ++ quote s1 ++
                " needs to be removed from the word " ++ quote s2)
      indent n 1 ("The required result is " ++ quote s)
  where
    s1 = getResult t
    s2 = getResult t'
-- If there is no indicator then this is the start of a Charade tree...
displayTree t@(Charade _ [] _ _) n k
  = displayConcatenate (flattenCharade' t) n k
displayTree (Charade (txt, s) ind t t') n k
  = do
      indent n k ("I think " ++ quote' ind ++ " indicates " ++
                "a charade (juxtaposition) of two sub-clues")
      displayTrees (n + 3) [t, t']
      indent n 1 ("So, the two components to be juxtaposed are " ++
                quote s1 ++ " and " ++ quote s2)
      indent n 1 ("The result is " ++ quote s)
  where
    s1 = getResult t
    s2 = getResult t'

displayConcatenate ts n k
  = do
      indent n k ("I think we have the concatenation of " ++ show (length ts) ++
                " sub-clues")
      displayTrees (n + 3) ts
      indent n 1 ("Joining the results together we obtain " ++ quote s)
  where
    s = concatMap getResult ts

displayTrees n ts
  = mapM_ displayOneTree (zip [1..] ts)
  where
    displayOneTree (i, t)
      = do
          putStr (replicate n ' ' ++ show i ++ ": ")
          displayTree t (n + 3) 0

flattenCharade' (Charade _ [] t t')
  = flattenCharade' t ++ flattenCharade' t'
flattenCharade' t
  = [t]
