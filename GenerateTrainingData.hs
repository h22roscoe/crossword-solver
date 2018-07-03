module GeneratingTrainingData where

import Indicators
import Data.List

allIndicators = [
  {- anagramIndicators ++ -} anagramInd1 ++ anagramInd2,
  {- oddCharacterIndicators ++ -} oddsInd1 ++ oddsInd2,
  {- evenCharacterIndicators ++ -} evensInd1 ++ evensInd2,
  {- hiddenWordIndicators ++ -} hiddenWordInd1 ++ hiddenWordInd2,
  {- subTextIndicators ++ -} subTextInd1 ++ subTextInd2,
  {- duplicateIndicators ++ -} duplicateInd1 ++ duplicateInd2,
  {- homophoneIndicators ++ -} homophoneInd1 ++ homophoneInd2,
  {- exampleOfIndicators ++ -} exampleOfInd1 ++ exampleOfInd2,
  {- reversalIndicators ++ -} reversalInd1 ++ reversalInd2,
  {- insertionIndicators ++ -} insertionInd1 ++ insertionInd2C1 ++ insertionInd2R1 ++ insertionInd2L1 ++ insertionInd2C2 ++ insertionInd2R2 ++ insertionInd2L2,
  {- subtractionIndicators ++ -} subtractionInd1 ++ subtractionInd2C1 ++ subtractionInd2R1 ++ subtractionInd2L1 ++ subtractionInd2C2 ++ subtractionInd2R2 ++ subtractionInd2L2,
  {- charadeIndicators ++ -} charadeInd1 ++ charadeInd2C1 ++ charadeInd2R1 ++ charadeInd2L1 ++ charadeInd2C2 ++ charadeInd2R2 ++ charadeInd2L2,
  defInd1 ++ defInd2]

main = writeFile "./data/indicators.csv" $ genCsvFile allIndicators

{- | Generate CSV data for a file.  The resulting string can be
written out to disk directly. -}
genCsvFile :: [[String]] -> String
genCsvFile inp =
    unlines . map csvline $ inp
    where csvline :: [String] -> String
          csvline l = concat . intersperse "," . map csvcells $ l
          csvcells :: String -> String
          csvcells "" = ""
          csvcells c = '"' : convcell c ++ "\""
          convcell :: String -> String
          convcell c = concatMap convchar c
          convchar '"' = "\"\""
          convchar x = [x]
