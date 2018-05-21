module Utilities where

import Data.List
import Debug.Trace
import Data.Array.IArray
import Data.Char

import Types

-- We'll keep apostrophes in...
cleanUp :: String -> String
cleanUp s
  = filter (not . flip elem removeChars) (map (replaceHyphen . toLower) s)
  where
    replaceHyphen '-' = ' '
    replaceHyphen c = c
    removeChars = ",:;?!"

-- Designed for indicators and synonyms etc.
declutter [s]
  = [s]
declutter s
  = filter (not . flip elem noise) s

noise
  = ["a", "all", "in", "to", "is", "by", "of", "its", "for", "the", "as",
     "out", "from", "was", "has", "be", "being", "let", "put"]

cartesianAppend [xs] 
  = xs
cartesianAppend (xs : xss)
  = [x ++ y | x <- xs, y <- cartesianAppend xss]

perms []
  = [[]]
perms xs
  = [x : ys | x <- xs, ys <- perms (xs \\ [x])]

isSubstring s s'
  = or [take (length s) t == s | t <- tails s']

lookUp x t
  = maybe [] id (Prelude.lookup x t)

sameLetters s s'
  = null (s \\ s') && null (s' \\ s)

prefixes 
  = tail . inits

suffixes s
  = take (length s) (iterate tail s)

substrings :: [a] -> [[a]]
substrings s
  = [i | t <- tails s, i <- tail (inits t)]

partitions' [] 
  = [[]]
partitions' (x:xs) 
  = [[x] : p | p <- partitions' xs] ++
    [(x : ys) : yss | (ys : yss) <- partitions' xs]

partitions :: [a] -> [[[a]]]
partitions 
  = init . partitions'

splitOn :: ([a] -> [a] -> Bool) -> [a] -> [a] -> [([a], [a], [a])]
splitOn p s []
  = []
splitOn p s lst@(c : cs)
  | p prefix s = ([], drop (length s) lst, prefix) : rest
  | otherwise = rest
  where
    prefix = take (length s) lst
    rest = [(c : l, r, pre) | (l, r, pre) <- splitOn p s cs]

notSingleton [x]
  = False
notSingleton xs
  = True

extractMiddle (x : xs)
  = (x, middle, head y)
  where
    n = length xs
    (middle, y) = splitAt (n - 1) xs

mirror2 :: [(a, a)] -> [(a, a)]
mirror2 xs
  = xs ++ [(y, x) | (x, y) <- xs]

mirror3 :: [(a, a, a)] -> [(a, a, a)]
mirror3 xs
  = xs ++ [(z, y, x) | (x, y, z) <- xs]


split2 :: [a] -> [([a], [a])]
split2 xs
  = [(take n xs, drop n xs) | n <- [1..length xs - 1]]

split3 :: [a] -> [([a], [a], [a])]
split3 xs
  = [(take n xs, ys, zs) | n <- [1..length xs - 1], (ys, zs) <- split2 (drop n xs)]

split2' :: [a] -> [([a], [a])]
split2'
  = mirror2 . split2

split3' :: [a] -> [([a], [a], [a])]
split3'
  = mirror3 . split3

anagrams :: String -> [String]
anagrams s
  = anagrams' s \\ [s]
anagrams' []
  = [[]]
anagrams' s
  = [c : s' | c <- nub s, s' <- anagrams' (s \\ [c])]

insertInto [] s
  = []
insertInto s s'
  = [s1 ++ s ++ s2 | (s1, s2) <- split2 s'] 

subtractFrom :: String -> String -> [String]
subtractFrom s []
  = []
subtractFrom s s'@(c : s'')
  = maybe fail succeed (stripPrefix s s')
  where
    fail = [c : s''' | s''' <- subtractFrom s s'']
    succeed suffix = suffix : [c : s''' | s''' <- subtractFrom s s'']

firstLetters s 
  = tail (inits s)

-- Pre: The list is non-empty
-- If the text ends with "'s" then take off the "'s"...
lastLetters :: String -> [String]
lastLetters s 
  | isPossessiveNoun = lastOf s1
  | otherwise        = lastOf s
  where
    (s1, s2) = splitAt (length s - 2) s
    isPossessiveNoun = s2 == "'s"
    lastOf s = tail (take (length ss - 1) ss)
             where ss = tails s

-- Direct solution...
removeMiddle s
  | length s < 3 = []
removeMiddle l@(_ : _ : s)
  = concat (f (prefixes l) (tails s) 1)
  where 
    f :: [[a]] -> [[a]] -> Int -> [[[a]]]
    f ps [] n = [] 
    f ps ([] : ts) n = []
    f ps (t : ts) n = [p ++ t | p <- take n ps] : f ps ts (n + 1)

removeEnds s
  | length s < 3 = []
  | otherwise    = s' : substrings s'
  where
    (_, s', _) = extractMiddle s

odds []
  = []
odds [x]
  = [x]
odds (x : x' : xs)
  = x : odds xs   

evens (x : x' : xs)
  = x' : evens xs   
evens xs
  = []

getResult :: ResultTree -> String
getResult (Text (_, s))
  = s
getResult (Abbreviation (_, s))
  = s
getResult (Synonym (_, s))
  = s
getResult (Anagram (_, s) _ _)
  = s
getResult (Odds (_, s) _ _)
  = s
getResult (Evens (_, s) _ _)
  = s
getResult (Duplicate (_, s) _ _ _ _)
  = s
getResult (ExampleOf (_, s) _ _)
  = s
getResult (SubText (_, s) _ _)
  = s
getResult (Homophone (_, s) _ _)
  = s
getResult (HiddenWord (_, s) _ _)
  = s
getResult (ReversedHiddenWord (_, s) _ _ _ _)
  = s
getResult (Reversal (_, s) _ _)
  = s
getResult (Insertion (_, s) _ _ _)
  = s
getResult (Subtraction (_, s) _ _ _)
  = s
getResult (Charade (_, s) _ _ _)
  = s

-- This needs to be done properly!
makeSingular :: [String] -> [String]
makeSingular ws
  | lastTwo == "ss" = ws
  | last endWord == 's' = take (n - 1) ws ++ [take (m - 1) endWord]
  | otherwise = ws
  where
    endWord = last ws
    n = length ws
    m = length endWord
    lastTwo = drop (m - 2) endWord
