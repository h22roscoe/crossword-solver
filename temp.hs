module Parser where 

import Data.List  
import Data.Char
import Data.Array.IArray

import Clues
import Types
import IndicatorPredicates
import Utilities
import LengthFunctions
import Databases
import Debug.Trace


------------------ CLUE PARSING MECHANICS FUNCTIONS ------------------------

isValidDef 
  = isInWordlist . unwords

parse :: Condition -> Clue -> [Parse]
parse cond clue@(s, n) 
  = parseWithoutIndicator cond clue ws ++ parseWithIndicator cond clue ws 
  where
    ws = words (cleanUp s)

parseWithoutIndicator :: Condition -> Clue -> Words -> [Parse]
parseWithoutIndicator cond clue ws 
  = [(clue, ws', [], p) | 
       (ws', ws'') <- split2' ws, 
       checkCondition cond ws',
       p <- parseClue ws''] 

parseWithIndicator :: Condition -> Clue -> Words -> [Parse]
parseWithIndicator cond clue ws 
  = [(clue, ws', ws'', p) | 
       (ws', ws'', ws''') <- split3' ws,
       checkCondition cond ws',
       isA defIndicator ws'', 
       p <- parseClue ws''']

checkCondition None ws
  = True
checkCondition Always ws
  = isValidDef ws
checkCondition (Meaning s) ws
  = unwords ws == s

-- Parsing with and without concat happens alternately down the tree: a concat
-- node cannot have another concat node as a direct descendent

parseClue :: [String] -> [ParseTree]
parseClue ws
  = parseClueTypes ws ++ concatMap makeConcats (partitions ws)
  where
    makeConcats p = map Concatenate (sequence (map parseClueTypes p))

parseClueTypes :: [String] -> [ParseTree]
parseClueTypes ws
  = parseSynonym ws ++
    parseAnagram pairs ++
    parseFirstLetter pairs ++
    parseLastLetter pairs ++
    parseOdds pairs ++
    parseEvens pairs ++
    parseDuplicate pairs ++
    parseHomophone pairs ++
    parseRemoveMiddle pairs ++
    parseRemoveEnds pairs ++
    parseSubText pairs ++
    parseHiddenWord pairs ++
    parseExampleOf pairs ++
    parseReversal pairs ++
    parseInsertion triples ++
    parseSubtraction triples ++
    parseCharade triples 
  where
    pairs = split2' ws
    triples = split3' ws

parseSynonym :: [String] -> [ParseTree]
parseSynonym ws
  = [Synonym ws] 

parseAnagram :: Pairs -> [ParseTree]
parseAnagram pairs
  = [Anagram ws ws' |
      (ws, ws') <- pairs,
      anagramInd ws] 

parseFirstLetter :: Pairs -> [ParseTree]
parseFirstLetter pairs
  = [FirstLetter ws ws' |
      (ws, ws') <- pairs,
      firstLetterInd ws] 

parseLastLetter :: Pairs -> [ParseTree]
parseLastLetter pairs
  = [LastLetter ws ws' |
      (ws, ws') <- pairs,
      lastLetterInd ws] 

parseOdds :: Pairs -> [ParseTree]
parseOdds pairs
  = [Odds ws ws' |
      (ws, ws') <- pairs,
      oddsInd ws] 

parseEvens :: Pairs -> [ParseTree]
parseEvens pairs
  = [Evens ws ws' |
      (ws, ws') <- pairs,
      evensInd ws] 

parseRemoveMiddle :: Pairs -> [ParseTree]
parseRemoveMiddle pairs
  = [RemoveMiddle ws ws' |
      (ws, ws') <- pairs,
      removeMiddleInd ws] 

parseRemoveEnds :: Pairs -> [ParseTree]
parseRemoveEnds pairs
  = [RemoveEnds ws ws' |
      (ws, ws') <- pairs,
      removeEndsInd ws] 

parseSubText :: Pairs -> [ParseTree]
parseSubText pairs
  = [SubText ws ws' |
      (ws, ws') <- pairs,
      subTextInd ws] 

-- Hidden words must span at least two words, otherwise it will be
-- picked up by other clue types
parseHiddenWord :: Pairs -> [ParseTree]
parseHiddenWord pairs
  = [HiddenWord ws ws' |
      (ws, ws') <- pairs,
      length ws' > 1,
      hiddenWordInd ws] 

parseDuplicate :: Pairs -> [ParseTree]
parseDuplicate pairs
  = [Duplicate ws ws' |
      (ws, ws') <- pairs,
      duplicateInd ws]

parseHomophone :: Pairs -> [ParseTree]
parseHomophone pairs
  = [Homophone ws ws' |
      (ws, ws') <- pairs,
      homophoneInd ws] 

parseExampleOf :: Pairs -> [ParseTree]
parseExampleOf pairs
  = [ExampleOf ws ws' |
      (ws, ws') <- pairs,
      exampleOfInd ws] 

parseReversal :: Pairs -> [ParseTree]
parseReversal pairs
  = [Reversal ws p | 
      (ws, ws') <- pairs, 
      reversalInd ws, p <- parseClue ws'] 

<<<<<<< Updated upstream
parseInsertion :: Triples -> [ParseTree]
parseInsertion triples
  = [Insertion ws p' p'' | 
      (ws, ws', ws'') <- triples, insertionInd ws,
=======
parseInsertion triples triples'
  = {- [Insertion ws p' p'' | 
      (ws, ws', ws'') <- triples, insertionIndL1 ws,
>>>>>>> Stashed changes
      p' <- parseClue ws', p'' <- parseClue ws''] ++
    [Insertion ws p' p'' | 
      (ws', ws, ws'') <- triples, insertionInd ws,
      p' <- parseClue ws', p'' <- parseClue ws''] ++
    [Insertion ws p' p'' | 
<<<<<<< Updated upstream
      (ws', ws'', ws) <- triples, insertionInd ws,
      p' <- parseClue ws', p'' <- parseClue ws''] 
=======
      (ws', ws, ws'') <- triples, insertionIndC1 ws,
      p' <- parseClue ws', p'' <- parseClue ws''] ++
    [Insertion ws p' p'' | 
      (ws'', ws, ws') <- triples, insertionIndC2 ws,
      p' <- parseClue ws', p'' <- parseClue ws''] ++
    [Insertion ws p' p'' | 
      (ws', ws'', ws) <- triples, insertionIndR1 ws,
      p' <- parseClue ws', p'' <- parseClue ws''] ++
    [Insertion ws p' p'' | 
      (ws'', ws', ws) <- triples, insertionIndR2 ws,
      p' <- parseClue ws', p'' <- parseClue ws''] +++
-}
    [Insertion ("?" : ws) p' p'' |
      (ws, ws', ws'') <- triples', 
      p' <- parseClue ws', p'' <- parseClue ws'']
>>>>>>> Stashed changes

parseSubtraction triples
  = [Subtraction ws p' p'' | 
      (ws, ws', ws'') <- triples, subtractionInd ws,
      p' <- parseClue ws', p'' <- parseClue ws''] ++
    [Subtraction ws p' p'' | 
      (ws', ws, ws'') <- triples, subtractionInd ws,
      p' <- parseClue ws', p'' <- parseClue ws''] ++
    [Subtraction ws p' p'' | 
      (ws', ws'', ws) <- triples, subtractionInd ws,
      p' <- parseClue ws', p'' <- parseClue ws''] 

parseCharade triples
  = [Charade ws p' p'' |
      (ws, ws', ws'') <- triples, charadeInd ws,
      p' <- parseClue ws', p'' <- parseClue ws''] ++
    [Charade ws p' p'' |
      (ws', ws, ws'') <- triples, charadeInd ws,
      p' <- parseClue ws', p'' <- parseClue ws''] ++
    [Charade ws p' p'' |
      (ws', ws'', ws) <- triples, charadeInd ws,
      p' <- parseClue ws', p'' <- parseClue ws'']

parses :: Condition -> SynonymTable -> Clue -> [Parse]
parses cond table c@(s, n) 
  = (sortByParseCost . 
     constrainParseLengths n table .
     parse cond) c

checkParses c@(s, n)
  = (sortByParseCost .
     constrainParseLengths n table .
     parse Always) c
  where
    table = makeSynonymTable c

-------------- COST EVALUATION -------------

evalCost :: Parse -> Int
evalCost (clue, s, ind, t)
  = cost t * (lengthPenalty (unwords s))

lengthPenalty ws
  = 60 + (length (words ws))

cost :: ParseTree -> Int
cost (Synonym strings)
  = 80 * length strings
cost (Anagram ind strings)
  = 10
cost (FirstLetter ind strings)
  = 20
cost (LastLetter ind strings)
  = 20
cost (Odds ind ws)
  = 10
cost (Evens ind ws)
  = 10
cost (RemoveMiddle ind t)
  = 60
cost (RemoveEnds ind t)
  = 60
cost (SubText ind t)
  = 60
cost (HiddenWord ind strings)
  = 40
cost (Duplicate ind strings)
  = 20
cost (Homophone ind strings)
  = 10
cost (ExampleOf ind strings)
  = cost (Synonym strings)
cost (Reversal ind t)
  = 10 + cost t
cost (Insertion ind t1 t2)
  = 10 + cost t1 + cost t2
cost (Subtraction ind t1 t2)
  = 30 + cost t1 + cost t2
cost (Charade ind t1 t2)
  = 40 + cost t1 + cost t2
cost (Concatenate ts)
  = 20 * (length ts) + sum (map cost ts)

-- Comment out the zip line after 'labels' and insert 'ts' and you remove 
-- the cost calculation. Some clues are much slower.
sortByParseCost :: [Parse] -> [Parse]
sortByParseCost ts
  = map snd . sort . map (\p -> (evalCost p, p)) $ ts

constrainParseLengths :: Int -> SynonymTable -> [Parse] -> [Parse]
constrainParseLengths n table ts
  = filter hasValidLength ts
  where 
    hasValidLength (_, _, _, t)
      = n >= minLength t table && n <= maxLength t table 


