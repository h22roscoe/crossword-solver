module IndicatorPredicates where

import Utilities
import Indicators
import Data.List
import Stemmer
import Debug.Trace

-- Stemmed noise comprises words that can safely be removed before checking for 
-- key indicator words.
-- Stemmed Ind1 words are *single* words (stemmed) that can be taken as
-- indicators in their own right, but that might also qualify a second word or 
-- phrase, e.g. changing shape. Here "change" is an Ind1 word and "shape" an
-- Ind2 word.

-- WARNING: This can massively increase the number of parses if you're not
-- careful. It is very easy for common words, e.g. "to" to end up being 
-- recognised as an indicator.
-- You must remember that Ind1 words get stripped from the Ind2 words, so it's
-- easy for, e.g. "to make" to become "to" if "make" is in Ind1.
-- Short plurals, e.g. "cars" get stemmed to "car" and is confused with "care", 
-- "caring" etc.
-- HACK: "cars" is a special case in the Stemmer.

stemAll
  = nub . map stem

thin :: [String] -> [String] -> [String] -> [[String]]
thin stemmedNoise stemmedInds
  = nub . map (removeWords (stemmedNoise ++ stemmedInds) . map stem . words)

removeWords :: [String] -> [String] -> [String]
removeWords stemmedWords
  = filter (not . (`elem` stemmedWords))

-- First remove the noise. If there is nothing left it's not an indicator.
-- Then remove the qualifiers. If there is nothing left we've found an
-- indicator. Otherwise look again to the main indicators (Ind2). It's
-- taken as an indicator if the phrase matches or every word in it
-- is a recognised indicator, e.g. "incredibly bad".
-- ws is already stemmed.
isIndicator noise ind1 ind2 ws
  -- | isInd = trace (unwords ws) $ True
  -- | otherwise = False
  = isInd
  where
    isInd = checkNull (removeWords noise ws)
    checkNull [] = False
    checkNull ws = checkNull' (removeWords ind1 ws)
    checkNull' ws = null ws || elem ws ind2 || and [elem [w] ind2 | w <- ws]

defNoiseStemmed
  = stemAll defNoise
defInd1Stemmed
  = stemAll defInd1
defInd2Stemmed
  = thin defNoiseStemmed defInd1Stemmed defInd2 

anagramNoiseStemmed
  = stemAll anagramNoise
anagramInd1Stemmed
  = stemAll anagramInd1
anagramInd2Stemmed
  = thin anagramNoiseStemmed anagramInd1Stemmed anagramInd2

oddsNoiseStemmed
  = stemAll oddsNoise
oddsInd1Stemmed
  = stemAll oddsInd1
oddsInd2Stemmed
  = thin oddsNoiseStemmed oddsInd1Stemmed oddsInd2

evensNoiseStemmed
  = stemAll evensNoise
evensInd1Stemmed
  = stemAll evensInd1
evensInd2Stemmed
  = thin evensNoiseStemmed evensInd1Stemmed evensInd2

exampleOfNoiseStemmed
  = stemAll exampleOfNoise
exampleOfInd1Stemmed
  = stemAll exampleOfInd1
exampleOfInd2Stemmed
  = thin exampleOfNoiseStemmed exampleOfInd1Stemmed exampleOfInd2

hiddenWordNoiseStemmed
  = stemAll hiddenWordNoise
hiddenWordInd1Stemmed
  = stemAll hiddenWordInd1
hiddenWordInd2Stemmed
  = thin hiddenWordNoiseStemmed hiddenWordInd1Stemmed hiddenWordInd2

reversalNoiseStemmed
  = stemAll reversalNoise
reversalInd1Stemmed
  = stemAll reversalInd1
reversalInd2Stemmed
  = thin reversalNoiseStemmed reversalInd1Stemmed reversalInd2

subTextNoiseStemmed
  = stemAll subTextNoise
subTextInd1Stemmed
  = stemAll subTextInd1
subTextInd2Stemmed
  = thin subTextNoiseStemmed subTextInd1Stemmed subTextInd2

firstLettersNoiseStemmed
  = stemAll firstLettersNoise
firstLettersInd1Stemmed
  = stemAll firstLettersInd1
firstLettersInd2Stemmed
  = thin firstLettersNoiseStemmed firstLettersInd1Stemmed firstLettersInd2

lastLettersNoiseStemmed
  = stemAll lastLettersNoise
lastLettersInd1Stemmed
  = stemAll lastLettersInd1
lastLettersInd2Stemmed
  = thin lastLettersNoiseStemmed lastLettersInd1Stemmed lastLettersInd2

removeMiddleNoiseStemmed
  = stemAll removeMiddleNoise
removeMiddleInd1Stemmed
  = stemAll removeMiddleInd1
removeMiddleInd2Stemmed
  = thin removeMiddleNoiseStemmed removeMiddleInd1Stemmed removeMiddleInd2

removeEndsNoiseStemmed
  = stemAll removeEndsNoise
removeEndsInd1Stemmed
  = stemAll removeEndsInd1
removeEndsInd2Stemmed
  = thin removeEndsNoiseStemmed removeEndsInd1Stemmed removeEndsInd2

duplicateNoiseStemmed
  = stemAll duplicateNoise
duplicateInd1Stemmed
  = stemAll duplicateInd1
duplicateInd2Stemmed
  = thin duplicateNoiseStemmed duplicateInd1Stemmed duplicateInd2

homophoneNoiseStemmed
  = stemAll homophoneNoise
homophoneInd1Stemmed
  = stemAll homophoneInd1
homophoneInd2Stemmed
  = thin homophoneNoiseStemmed homophoneInd1Stemmed homophoneInd2

insertionNoiseStemmed
  = stemAll insertionNoise
insertionInd1Stemmed
  = stemAll insertionInd1
insertionInd2L1Stemmed
  = thin insertionNoiseStemmed insertionInd1Stemmed insertionInd2L1
insertionInd2L2Stemmed
  = thin insertionNoiseStemmed insertionInd1Stemmed insertionInd2L2
insertionInd2C1Stemmed
  = thin insertionNoiseStemmed insertionInd1Stemmed insertionInd2C1
insertionInd2C2Stemmed
  = thin insertionNoiseStemmed insertionInd1Stemmed insertionInd2C2
insertionInd2R1Stemmed
  = thin insertionNoiseStemmed insertionInd1Stemmed insertionInd2R1
insertionInd2R2Stemmed
  = thin insertionNoiseStemmed insertionInd1Stemmed insertionInd2R2

subtractionNoiseStemmed
  = stemAll subtractionNoise
subtractionInd1Stemmed
  = stemAll subtractionInd1
subtractionInd2L1Stemmed
  = thin subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2L1
subtractionInd2L2Stemmed
  = thin subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2L2
subtractionInd2C1Stemmed
  = thin subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2C1
subtractionInd2C2Stemmed
  = thin subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2C2
subtractionInd2R1Stemmed
  = thin subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2R1
subtractionInd2R2Stemmed
  = thin subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2R2

charadeNoiseStemmed
  = stemAll charadeNoise
charadeInd1Stemmed
  = stemAll charadeInd1
charadeInd2L1Stemmed
  = thin charadeNoiseStemmed charadeInd1Stemmed charadeInd2L1
charadeInd2L2Stemmed
  = thin charadeNoiseStemmed charadeInd1Stemmed charadeInd2L2
charadeInd2C1Stemmed
  = thin charadeNoiseStemmed charadeInd1Stemmed charadeInd2C1
charadeInd2C2Stemmed
  = thin charadeNoiseStemmed charadeInd1Stemmed charadeInd2C2
charadeInd2R1Stemmed
  = thin charadeNoiseStemmed charadeInd1Stemmed charadeInd2R1
charadeInd2R2Stemmed
  = thin charadeNoiseStemmed charadeInd1Stemmed charadeInd2R2

allInds
  = [defIndicator, anagramInd, firstLettersInd, lastLettersInd, oddsInd,
     evensInd, removeMiddleInd, removeEndsInd, subTextInd, hiddenWordInd,
     duplicateInd, homophoneInd, exampleOfInd, reversalInd, insertionIndL1,
     insertionIndL2, insertionIndC1, insertionIndC2, insertionIndR1, insertionIndR2,
     subtractionIndL1, subtractionIndL2, subtractionIndC1, subtractionIndC2, 
     subtractionIndR1, subtractionIndR2, charadeIndL1, charadeIndL2, 
     charadeIndC1, charadeIndC2, charadeIndR1, charadeIndR2]

defIndicator 
  = isIndicator defNoiseStemmed defInd1Stemmed defInd2Stemmed 

anagramInd  
  = isIndicator anagramNoiseStemmed anagramInd1Stemmed anagramInd2Stemmed 

firstLettersInd
  = isIndicator firstLettersNoiseStemmed firstLettersInd1Stemmed firstLettersInd2Stemmed

lastLettersInd
  = isIndicator lastLettersNoiseStemmed lastLettersInd1Stemmed lastLettersInd2Stemmed

oddsInd
  = isIndicator oddsNoiseStemmed oddsInd1Stemmed oddsInd2Stemmed

evensInd
  = isIndicator evensNoiseStemmed evensInd1Stemmed evensInd2Stemmed

removeMiddleInd
  = isIndicator removeMiddleNoiseStemmed removeMiddleInd1Stemmed removeMiddleInd2Stemmed

removeEndsInd
  = isIndicator removeEndsNoiseStemmed removeEndsInd1Stemmed removeEndsInd2Stemmed

subTextInd
  = isIndicator subTextNoiseStemmed subTextInd1Stemmed subTextInd2Stemmed

hiddenWordInd
  = isIndicator hiddenWordNoiseStemmed hiddenWordInd1Stemmed hiddenWordInd2Stemmed

duplicateInd
  = isIndicator duplicateNoiseStemmed duplicateInd1Stemmed duplicateInd2Stemmed

homophoneInd
  = isIndicator homophoneNoiseStemmed homophoneInd1Stemmed homophoneInd2Stemmed

exampleOfInd
  = isIndicator exampleOfNoiseStemmed exampleOfInd1Stemmed exampleOfInd2Stemmed

reversalInd
  = isIndicator reversalNoiseStemmed reversalInd1Stemmed reversalInd2Stemmed

insertionIndL1
  = isIndicator insertionNoiseStemmed insertionInd1Stemmed insertionInd2L1Stemmed
insertionIndL2
  = isIndicator insertionNoiseStemmed insertionInd1Stemmed insertionInd2L2Stemmed
insertionIndC1
  = isIndicator insertionNoiseStemmed insertionInd1Stemmed insertionInd2C1Stemmed
insertionIndC2
  = isIndicator insertionNoiseStemmed insertionInd1Stemmed insertionInd2C2Stemmed
insertionIndR1
  = isIndicator insertionNoiseStemmed insertionInd1Stemmed insertionInd2R1Stemmed
insertionIndR2
  = isIndicator insertionNoiseStemmed insertionInd1Stemmed insertionInd2R2Stemmed

subtractionIndL1
  = isIndicator subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2L1Stemmed
subtractionIndL2
  = isIndicator subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2L2Stemmed
subtractionIndC1
  = isIndicator subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2C1Stemmed
subtractionIndC2
  = isIndicator subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2C2Stemmed
subtractionIndR1
  = isIndicator subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2R1Stemmed
subtractionIndR2
  = isIndicator subtractionNoiseStemmed subtractionInd1Stemmed subtractionInd2R2Stemmed

charadeIndL1
  = isIndicator charadeNoiseStemmed charadeInd1Stemmed charadeInd2L1Stemmed
charadeIndL2
  = isIndicator charadeNoiseStemmed charadeInd1Stemmed charadeInd2L2Stemmed
charadeIndC1
  = isIndicator charadeNoiseStemmed charadeInd1Stemmed charadeInd2C1Stemmed
charadeIndC2
  = isIndicator charadeNoiseStemmed charadeInd1Stemmed charadeInd2C2Stemmed
charadeIndR1
  = isIndicator charadeNoiseStemmed charadeInd1Stemmed charadeInd2R1Stemmed
charadeIndR2
  = isIndicator charadeNoiseStemmed charadeInd1Stemmed charadeInd2R2Stemmed

