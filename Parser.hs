module Parser where

import Data.Maybe
import Data.List
import Data.Char
import Data.Array.IArray
import qualified Data.Map as Map

import Stemmer
import Clues
import Types
import IndicatorPredicates
import Utilities
import LengthFunctions
import Databases
import Debug.Trace
import qualified Classify


------------------ CLUE PARSING MECHANICS FUNCTIONS ------------------------

-- This version uses a stem cache for all 'substrings' (actually subtexts) in
-- the clue. It's a bit like the synonym table.
-- It makes a huge difference, but beware that SubText clue types are made up
-- of four different sub-operations, so you have to re-apply stemming whenever
-- you inspect the indicator outside of the parser (specifically in Solver and
-- Evaluation).

makeStemCache :: [ClueText] -> Map.Map ClueText ClueText
makeStemCache wss
  = Map.fromList (zip wss (map (map stem) wss))

get :: ClueText -> StemCache -> ClueText
get ws sc
  = maybe (error ("Stem cache lookup error: " ++ unwords ws)) id (Map.lookup ws sc)

-- Needed for Duplicate clues. Singular words are (may not be) in the
-- clue text so have to be handled separately. They get added to the
-- synonym table.
-- Could be done by a fold over the tree, but best if we use predicates
-- to pick out all the base cases instead of pattern matching i.e. fold here
-- would take ++, isDuplicate and (:[]).
singularWords :: ParseTree -> [[String]]
singularWords (Duplicate _ ws _ _ _)
  = [ws]
singularWords (Reversal _ _ t)
  = singularWords t
singularWords (Insertion _ _ t t')
  = singularWords t ++ singularWords t'
singularWords (Subtraction _ _ t t')
  = singularWords t ++ singularWords t'
singularWords (Charade _ _ t t')
  = singularWords t ++ singularWords t'
singularWords t
  = []

isValidDef
  = isInWordlist . unwords

sortByParseCost :: [Parse] -> [Parse]
sortByParseCost
  = map snd . sort . map (\p -> (evalCost p, p))

constrainParseLengths :: Int -> SynonymTable -> [Parse] -> [Parse]
constrainParseLengths n table ts
  = filter hasValidLength ts
  where
    hasValidLength (_, _, _, t)
      = n >= minLength t table && n <= maxLength t table

constrainParseLengthsMaybe :: Int -> SynonymTable -> [Parse] -> [Parse]
constrainParseLengthsMaybe n table ts
  = filter hasValidLength ts
  where
    hasValidLength (_, _, _, t)
      | hasMinLen && hasMaxLen = n >= fromJust minLen && n <= fromJust maxLen
      | hasMinLen              = n >= fromJust minLen
      | hasMaxLen              = n <= fromJust maxLen
      | otherwise              = False
      where
        minLen = minLengthMaybe t table
        maxLen = maxLengthMaybe t table
        hasMinLen = isJust minLen
        hasMaxLen = isJust maxLen

-- These allow flexibility when checking parses (we may want to tell it the
-- definition, for example).
checkCondition None ws
  = True
checkCondition Always ws
  = isValidDef ws
checkCondition (Meaning s) ws
  = unwords ws == s

-- The synonym table is needed in the evaluator, so is passed back
-- along with the parses.
parses :: Condition -> Clue -> ([Parse], SynonymTable)
parses cond c
  = parsesHelper constrainParseLengths cond c

parsesWithoutSynonymLengths :: Condition -> Clue -> ([Parse], SynonymTable)
parsesWithoutSynonymLengths cond c
  = parsesHelper constrainParseLengthsMaybe cond c

parsesHelper :: (Int -> SynonymTable -> [Parse] -> [Parse]) -> Condition -> Clue -> ([Parse], SynonymTable)
parsesHelper constrain cond c@(text, n)
  = (sortByParseCost (constrain n table ps), table)
  where
    table = makeSynonymTable text extraText
    extraText = concatMap getSingularWords ps
    ps = parse cond c (makeStemCache (substrings (words (cleanUp text))))
    getSingularWords (_, _, _, t) = singularWords t

allParses :: Clue -> ([Parse], SynonymTable)
allParses c@(text, n)
  = (sortByParseCost ps, table)
  where
    table = makeSynonymTable text extraText
    extraText = concatMap getSingularWords ps
    ps = parse Always c (makeStemCache (substrings (words (cleanUp text))))
    getSingularWords (_, _, _, t) = singularWords t

parse :: Condition -> Clue -> StemCache -> [Parse]
parse cond clue@(s, n) sc
  = parseWithoutIndicator cond clue ws sc table ++
    parseWithIndicator cond clue ws sc table
  where
    ws = words (cleanUp s)
    table = Classify.makeIndicatorTable ws

checkParses c@(text, n)
  = sortByParseCost (constrainParseLengths n table ps)
  where
    table = makeSynonymTable text extraText
    extraText = concatMap getSingularWords ps
    ps = parse Always c (makeStemCache (substrings (words (cleanUp text))))
    getSingularWords (_, _, _, t) = singularWords t

parseWithoutIndicator :: Condition -> Clue -> ClueText -> StemCache -> Classify.IndicatorTable -> [Parse]
parseWithoutIndicator cond clue ws sc table
  = [(clue, ws', [], p) | (ws', ws'') <- split2' ws,
                          checkCondition cond ws',
                          p <- parseClue ws'' sc table]

parseWithIndicator :: Condition -> Clue -> ClueText -> StemCache -> Classify.IndicatorTable -> [Parse]
parseWithIndicator cond clue ws sc table
  = [(clue, ws', ws'', p) | (ws', ws'', ws''') <- split3' ws,
                            checkCondition cond ws',
                            defIndicator (get ws'' sc),
                            p <- parseClue ws''' sc table]

-- Parsing with and without concat happens alternately down the tree: a concat
-- node cannot have another concat node as a direct descendent

type ClueTypeParser = ClueText -> StemCache -> Classify.IndicatorTable -> [ParseTree]

parseClue :: ClueText -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseClue text sc table
  = applyParser text table parseAllClueTypes sc

-- WARNING: If you remove abbreviations then anagrams must be able to take
-- synonyms as arguments, in which case you then need to rewrite synonyms
-- to text in a post-parse filter over anagrams.
-- You'll need this filter anyway to get rid of unwanted cases.
-- Note that Charades of text/abbreviations are supported through applyParser
-- After flattening out text (flattenText) several charade trees may collapse
-- to the same text. Hence nub...
-- NOTE: Should ideally work for duplicates, e.g. double gin mixed... but this
-- version doesn't
parseTextAbbr :: ClueText -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseTextAbbr text sc table
  = nub (applyParser text table parseTextAbbrOnly sc)
  where
    parseTextAbbrOnly :: ClueTypeParser
    parseTextAbbrOnly text sc table
      = parseText text sc ++
        parseAbbreviation text sc

-- For subtexts
-- Note that this excludes charades completely.
parseTextSyn :: ClueTypeParser
parseTextSyn text sc table
  = parseText text sc ++
    parseSynonym text sc ++
    parseExampleOf text pairs sc table
  where
    pairs = split2' text

-- partitions returns all subtext partitions; from these we can form all
-- possible concatenations using sequence (Data.List). For each concatenation
-- we build a Charade tree.
applyParser :: ClueText -> Classify.IndicatorTable -> ClueTypeParser -> StemCache -> [ParseTree]
applyParser text table parser sc
  = parser text sc table ++ concatMap makeCharades (partitions text)
  where
    makeCharades p = map (buildCharade text)
                         (sequence (map (\txt -> parser txt sc table) p))

-- Builds a balanced tree. Each Charade gets a copy of the top-level
-- text corresponding to the concatenation. This is for the no-indicator
-- case only.
-- flattenText collapses trees of text to just one text node.
buildCharade text ts
  = flattenText (build ts (length ts))
  where
    build [] n
      = error ("Charade build error " ++ show n)
    build [t] n
      = t
    build ts n
      = Charade text [] (build ts' n1) (build ts'' n2)
      where
        n1 = n `div` 2
        n2 = n - n1
        (ts', ts'') = splitAt n1 ts

-- For everything else (abreviations are subsumed by synonyms)...
-- We only need Text in special cases
-- Note: The L1-R2 indicators variants mean we need split3, not split3'
parseAllClueTypes :: ClueTypeParser
parseAllClueTypes text sc table
  = parseSynonym text sc ++
    parseAnagram text pairs sc table ++
    parseOdds text pairs sc table ++
    parseEvens text pairs sc table ++
    parseDuplicate text pairs sc table ++
    parseSubText text pairs sc table ++
    parseHiddenWord text pairs sc table ++
    parseExampleOf text pairs sc table ++
    parseHomophone text pairs sc table ++
    parseReversal text pairs sc table ++
    parseInsertion text triples sc table ++
    parseSubtraction text triples sc table ++
    parseCharade text triples sc table
  where
    pairs = split2' text
    triples = split3 text

parseText :: ClueText -> StemCache -> [ParseTree]
parseText text sc
  = [Text text]

parseSynonym :: ClueText -> StemCache -> [ParseTree]
parseSynonym text sc
  = [Synonym text]

parseAbbreviation :: ClueText -> StemCache -> [ParseTree]
parseAbbreviation text sc
  | hasAbbreviation s = [Abbreviation text]
  | otherwise         = []
  where
    s = unwords text

parseAnagram :: ClueText -> Pairs -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseAnagram text pairs sc table
  = [Anagram text ws p | (ws, ws') <- pairs,
                         anagramInd (get ws sc) || elem Classify.Anagram (fromJust (lookup ws table)),
                         p <- parseTextAbbr ws' sc table]

parseOdds :: ClueText -> Pairs -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseOdds text pairs sc table
  = [Odds text ws ws' | (ws, ws') <- pairs,
                        oddsInd (get ws sc) || elem Classify.Odds (fromJust (lookup ws table))]

parseEvens :: ClueText -> Pairs -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseEvens text pairs sc table
  = [Evens text ws ws' | (ws, ws') <- pairs,
                         evensInd (get ws sc) || elem Classify.Evens (fromJust (lookup ws table))]

parseExampleOf :: ClueText -> Pairs -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseExampleOf text pairs sc table
  = [ExampleOf text ws ws' | (ws, ws') <- pairs,
                             exampleOfInd (get ws sc) || elem Classify.ExampleOf (fromJust (lookup ws table))]

-- Hidden words must span all of the words and there must be at least two.
-- Special case: subtexts of reversals have their own constructor.
-- WARNING: If you try to do this using recursion then you can't
-- guarantee that the hidden word spans all words in the text.
parseHiddenWord :: ClueText -> Pairs -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseHiddenWord text pairs sc table
  = [fromJust t | (ws, ws') <- pairs,
                  length ws' > 1,
                  hiddenWordInd (get ws sc) || elem Classify.HiddenWord (fromJust (lookup ws table)),
                  p <- parseClue ws' sc table,
                  let t = makeHiddenWord ws p,
                  isJust t]
  where
    -- ws is now the text that might contain the (reversed) hidden
    -- word so must check that this has the required length...
    makeHiddenWord ind (Reversal text' ind' (Synonym ws))
      | length ws > 1 = Just (ReversedHiddenWord text text' ind ind' ws)
      | otherwise     = Nothing
    makeHiddenWord ind (Synonym text')
      = Just (HiddenWord text ind text')
    makeHiddenWord ind t
      = Nothing

-- We split the cases so that we can get different cost functions: the second
-- case is quite rare and can be expensive.
parseSubText :: ClueText -> Pairs -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseSubText text pairs sc table
  = [SubText text ws p | (ws, ws') <- pairs,
                         subTextInd (get ws sc) || elem Classify.Subtext (fromJust (lookup ws table)),
                         p <- parseTextSyn ws' sc table]

-- The extra argument is the singular version of ws'. There are two
-- of these because in general there may be a different synonym instance
-- for each and we need the results for the explanation.
parseDuplicate :: ClueText -> Pairs -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseDuplicate text pairs sc table
  = [Duplicate text s s ws ws' | (ws, ws') <- pairs,
                                 duplicateInd (get ws sc) || elem Classify.Duplicate (fromJust (lookup ws table)),
                                 let s = makeSingular ws']

parseHomophone :: ClueText -> Pairs -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseHomophone text pairs sc table
  = [Homophone text ws (Synonym ws') | (ws, ws') <- pairs,
                                       homophoneInd (get ws sc) || elem Classify.Homophone (fromJust (lookup ws table))]

parseReversal :: ClueText -> Pairs -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseReversal text pairs sc table
  = [Reversal text ws p | (ws, ws') <- pairs,
                          reversalInd (get ws sc) || elem Classify.Reversal (fromJust (lookup ws table)),
                          p <- parseClue ws' sc table]

-- This uses the L1-R2 indicator variants. It's probably worth separating
-- these out as the indicators are sometimes quite different from one another.
parseInsertion :: ClueText -> Triples -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseInsertion text triples sc table
  = [Insertion text ws p' p'' | (ws, ws', ws'') <- triples,
                                insertionIndL1 (get ws sc) || elem Classify.Insertion (fromJust (lookup ws table)),
                                p' <- parseClue ws' sc table,
                                p'' <- parseClue ws'' sc table] ++
    [Insertion text ws p' p'' | (ws, ws'', ws') <- triples,
                                insertionIndL2 (get ws sc) || elem Classify.Insertion (fromJust (lookup ws table)),
                                p' <- parseClue ws' sc table,
                                p'' <- parseClue ws'' sc table] ++
    [Insertion text ws p' p'' | (ws', ws, ws'') <- triples,
                                insertionIndC1 (get ws sc) || elem Classify.Insertion (fromJust (lookup ws table)),
                                p' <- parseClue ws' sc table,
                                p'' <- parseClue ws'' sc table] ++
    [Insertion text ws p' p'' | (ws'', ws, ws') <- triples,
                                insertionIndC2 (get ws sc) || elem Classify.Insertion (fromJust (lookup ws table)),
                                p' <- parseClue ws' sc table,
                                p'' <- parseClue ws'' sc table] ++
    [Insertion text ws p' p'' | (ws', ws'', ws) <- triples,
                                insertionIndR1 (get ws sc) || elem Classify.Insertion (fromJust (lookup ws table)),
                                p' <- parseClue ws' sc table,
                                p'' <- parseClue ws'' sc table] ++
    [Insertion text ws p' p'' | (ws'', ws', ws) <- triples,
                                insertionIndR2 (get ws sc) || elem Classify.Insertion (fromJust (lookup ws table)),
                                p' <- parseClue ws' sc table,
                                p'' <- parseClue ws'' sc table]

-- TO DO: Remove anagrams from the arguments. Nobody would ever do this and
-- they can be expensive, e.g. ("instrument made from two circles", 5)
-- (("instrument made from two circles",5),["circles"],[],Subtraction
-- ["instrument","made","from","two"] ["from"] (Synonym ["two"]) (Anagram
-- ["instrument","made"] ["made"] (Text ["instrument"])))

parseSubtraction :: ClueText -> Triples -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseSubtraction text triples sc table
  = [Subtraction text ws p' p'' | (ws, ws', ws'') <- triples,
                              subtractionIndL1 (get ws sc) || elem Classify.Subtraction (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table] ++
    [Subtraction text ws p' p'' | (ws, ws'', ws') <- triples,
                              subtractionIndL2 (get ws sc) || elem Classify.Subtraction (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table] ++
    [Subtraction text ws p' p'' | (ws', ws, ws'') <- triples,
                              subtractionIndC1 (get ws sc) || elem Classify.Subtraction (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table] ++
    [Subtraction text ws p' p'' | (ws'', ws, ws') <- triples,
                              subtractionIndC2 (get ws sc) || elem Classify.Subtraction (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table] ++
    [Subtraction text ws p' p'' | (ws', ws'', ws) <- triples,
                              subtractionIndR1 (get ws sc) || elem Classify.Subtraction (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table] ++
    [Subtraction text ws p' p'' | (ws'', ws', ws) <- triples,
                              subtractionIndR2 (get ws sc) || elem Classify.Subtraction (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table]


-- This uses the L1-R2 indicator variants. It's probably worth separating
-- these out as the indicators are sometimes quite different from one another.
parseCharade :: ClueText -> Triples -> StemCache -> Classify.IndicatorTable -> [ParseTree]
parseCharade text triples sc table
  = [Charade text ws p' p'' | (ws, ws', ws'') <- triples,
                              charadeIndL1 (get ws sc) || elem Classify.Charade (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table] ++
    [Charade text ws p' p'' | (ws, ws'', ws') <- triples,
                              charadeIndL2 (get ws sc) || elem Classify.Charade (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table] ++
    [Charade text ws p' p'' | (ws', ws, ws'') <- triples,
                              charadeIndC1 (get ws sc) || elem Classify.Charade (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table] ++
    [Charade text ws p' p'' | (ws'', ws, ws') <- triples,
                              charadeIndC2 (get ws sc) || elem Classify.Charade (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table] ++
    [Charade text ws p' p'' | (ws', ws'', ws) <- triples,
                              charadeIndR1 (get ws sc) || elem Classify.Charade (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table] ++
    [Charade text ws p' p'' | (ws'', ws', ws) <- triples,
                              charadeIndR2 (get ws sc) || elem Classify.Charade (fromJust (lookup ws table)),
                              p' <- parseClue ws' sc table,
                              p'' <- parseClue ws'' sc table]


-------------- COST EVALUATION -------------

evalCost :: Parse -> Int
evalCost (clue, s, ind, t)
  = cost t * (lengthPenalty (unwords s))

lengthPenalty ws
  = 60 + (length (words ws))

cost :: ParseTree -> Int
cost (Synonym ws)
  = 80 * length ws
cost (Anagram _ _ ws)
  = 10
cost (Odds _ _ ws)
  = 10
cost (Evens _ _ ws)
  = 10
cost (SubText _ _ (Text ws))
  = 20
-- General subtexts are rare, so we'll try everything else first...
cost (SubText _ _ t)
  = 1000
cost (HiddenWord _ _ ws)
  = 20
cost (ReversedHiddenWord _ _ _ _' ws)
  = 20
cost (Duplicate _ _ _ _ ws)
  = 20
cost (Homophone _ _ ws)
  = 10
cost (ExampleOf _ _ ws)
  = cost (Synonym ws)
cost (Reversal _ _ t)
  = 10 + cost t
cost (Insertion _ _ t t')
  = 10 + cost t + cost t'
cost (Subtraction _ _ t t')
  = 100 + cost t + cost t'
cost t@(Charade _ [] _ _)
  = 20 * (length ts) + sum (map cost ts)
  where
    ts = flattenCharade t
cost (Charade _ _ t t')
  = 40 + cost t + cost t'

flattenCharade (Charade _ [] t t')
  = flattenCharade t ++ flattenCharade t'
flattenCharade t
  = [t]

flattenText (Charade text [] t t')
  = removeText (Charade text [] (flattenText t) (flattenText t'))
flattenText t
  = t

removeText (Charade text [] (Text txt) (Text txt'))
  = Text (txt ++ txt')
removeText t
  = t
