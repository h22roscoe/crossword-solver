module Display where

import Types
import Utilities

showDef :: Parse -> String
showDef (d, tree, n) 
  = "Definition: " ++ show d ++ " \n" ++ showTree tree 1 ++ " \n\n" 

showTree :: ParseTree -> Int -> String
showTree (Juxtapose x y) n 
  = spaces n ++ showTreeL x (n+1) ++ showTreeL y (n+1)
showTree (Concatenate xs) n 
  = spaces n ++ "Cons \n" ++ concat (map (\x -> (showTreeL x (n+1))) xs)
showTree (Anagram anagrinds strings) n 
  = spaces n ++ "Anagram (" ++ concatWithSpaces anagrinds ++ ") " ++ concat strings
showTree (Insertion ind t1 t2) n 
  = spaces n ++ "Insert ("++ concatWithSpaces ind++") \n" ++ showTreeL t1 (n+1) ++ spaces n ++ "into" ++ " \n" ++ showTreeL t2 (n+1)
showTree (Subtraction ind t1 t2) n 
  = spaces n ++ "Subtract ("++ concatWithSpaces ind++") \n" ++ showTreeL t1 (n+1) ++ spaces n ++ "from" ++ " \n" ++ showTreeL t2 (n+1)
showTree x n 
  = spaces n ++ show x 

spaces 0 = ""
spaces n = "    " ++ spaces (n-1)

showTreeL x n = showTree x n ++ "\n"

print_this def = (putStr . showDef) def

print_all defs = mapM print_this defs

answer_string (Answer x y) = x
just_answers = map answer_string
