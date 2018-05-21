{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ReverseDictionary where

import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import GHC.Generics
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.String.Utils
import Data.Char
import Data.List
import Debug.Trace
import System.IO.Unsafe

data Datamuse = Datamuse {
                  word :: String,
                  score :: Int,
                  numSyllables :: Maybe Int,
                  tags :: Maybe [String]
                }
             deriving (Show, Generic)

instance FromJSON Datamuse where
  parseJSON = withObject "datamuse" $ \o -> do
     word         <- o .: "word"
     score        <- o .: "score"
     numSyllables <- optional (o .: "numSyllables")
     tags         <- optional (o .: "tags")
     return Datamuse {
                      word = word,
                      score = score,
                      numSyllables = numSyllables,
                      tags = tags
                    }

data Query = MeansLike [String] -- Means like constraint: require that the results have a meaning related to this string value, which can be any word or sequence of words.
           | SoundsLike [String] -- Sounds like constraint: require that the results are pronounced similarly to this string of characters.
           | SpelledLike [String] -- Spelled like constraint: require that the results are spelled similarly to this string of characters, or that they match this wildcard pattern.
           | Rel Code [String] -- Related word constraints: require that the results, when paired with the word in this parameter, are in a predefined lexical relation indicated by [code].
           | V Vocab -- Identifier for the vocabulary to use. If none is provided, a 550,000-term vocabulary of English words and multiword expressions is used.
           | Topics [String] -- Topic words: An optional hint to the system about the theme of the document being written. Results will be skewed toward these topics. At most 5 words can be specified. Space or comma delimited. Nouns work best.
           | LC String -- Left context
           | RC String -- Right context
           | Max Int -- Max number of results

data Code = JJA -- Popular nouns modified by the given adjective, per Google Books Ngrams	e.g. gradual → increase
          | JJB -- Popular adjectives used to modify the given noun, per Google Books Ngrams e.g. beach → sandy
          | SYN -- Synonyms (words contained within the same WordNet synset) e.g. ocean → sea
          | TRG -- "Triggers" (words that are statistically associated with the query word in the same piece of text.) e.g. cow → milking
          | ANT -- Antonyms (per WordNet) e.g late → early
          | SPC -- "Kind of" (direct hypernyms, per WordNet) e.g. gondola → boat
          | GEN -- "More general than" (direct hyponyms, per WordNet) e.g. boat → gondola
          | COM -- "Comprises" (direct holonyms, per WordNet) e.g. car → accelerator
          | PAR -- "Part of" (direct meronyms, per WordNet) e.g. trunk → tree
          | BGA -- Frequent followers (w′ such that P(w′|w) ≥ 0.001, per Google Books Ngrams) e.g. wreak → havoc
          | BGB -- Frequent predecessors (w′ such that P(w|w′) ≥ 0.001, per Google Books Ngrams) e.g. havoc → wreak
          | RHY -- Rhymes ("perfect" rhymes, per RhymeZone) e.g. spade → aid
          | NRY -- Approximate rhymes (per RhymeZone) e.g. forest → chorus
          | HOM -- Homophones (sound-alike words) e.g. course → coarse
          | CNS -- Consonant match e.g. sample → simple
          deriving (Show)

data Vocab = EnWiki
           | Spanish

instance Show Vocab where
  show EnWiki  = "enwiki"
  show Spanish = "es"

makeQuery :: [Query] -> String
makeQuery qs
  = intercalate "&" (map makeQuery' qs)
  where makeQuery' (MeansLike strs)   = "ml=" ++ (intercalate "+" strs)
        makeQuery' (SoundsLike strs)  = "sl=" ++ (intercalate "+" strs)
        makeQuery' (SpelledLike strs) = "sp=" ++ (intercalate "+" strs)
        makeQuery' (Rel code strs)    = "rel_" ++ (map toLower (show code)) ++ "=" ++ (intercalate "+" strs)
        makeQuery' (V vocab)          = "v=" ++ (show vocab)
        makeQuery' (Topics strs)      = "topics=" ++ (intercalate "+" strs)
        makeQuery' (LC str)           = "lc=" ++ str
        makeQuery' (RC str)           = "rc=" ++ str
        makeQuery' (Max m)            = "max=" ++ (show m)

baseURL :: String
baseURL = "https://api.datamuse.com/words?"

getDatamuse :: [Query] -> IO [Datamuse]
getDatamuse q
  = fmap (fromMaybe [] . decode) $ simpleHttp $ baseURL ++ (makeQuery q)

getWords :: [Query] -> IO [String]
getWords = fmap (map word) . getDatamuse

queryDatamuse :: [Query] -> [Datamuse]
queryDatamuse q
  = trace ("Querying datamuse..." ++ makeQuery q) $ unsafePerformIO $ do getDatamuse q

query :: [Query] -> [String]
query = map word . queryDatamuse
