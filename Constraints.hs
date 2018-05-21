module Constraints where

import Debug.Trace
import Databases

data Constraints 
  = Constraints {prefixConstraint :: Maybe String, 
                 minConstraint    :: Maybe Int, 
                 maxConstraint    :: Maybe Int}
  deriving Show

infixl 6 <<< 
c <<< n = shiftBounds c (-n)

infixl 6 >>> 
c >>> n = shiftBounds c n

infixl 6 <<+ 
c <<+ n = addToMin c n

infixl 6 >>+ 
c >>+ n = addToMax c n

infixl 6 <<- 
c <<- n = addToMin c (-n)

infixl 6 >>- 
c >>- n = addToMax c (-n)

infixl 6 +++
c +++ s = extendPrefix c s

add :: Int -> Maybe Int -> Maybe Int
add x
  = fmap ((max 0) . (+ x))

addString :: String -> Maybe String -> Maybe String
addString s 
  = fmap (++s)

geqMin :: Constraints -> Int -> Bool
geqMin c n 
  = maybe True (n>=) (minConstraint c)

leqMax :: Constraints -> Int -> Bool
leqMax c n 
  = maybe True (n<=) (maxConstraint c)

satisfiesPre :: Constraints -> String -> Bool
satisfiesPre c s 
  = maybe True (\s' -> isPrefix (s' ++ s)) (prefixConstraint c)

satisfiesLen :: Constraints -> Int -> Bool
satisfiesLen c n
  = geqMin c n && leqMax c n

satisfies :: Constraints -> String -> Bool
satisfies c s
  = satisfiesLen c n && satisfiesPre c s
  where
    n = length s

extendPrefix :: Constraints -> String -> Constraints
extendPrefix (Constraints p mn mx) s
  = Constraints (addString s p) mn mx

shiftBounds :: Constraints -> Int -> Constraints
shiftBounds (Constraints p mn mx) n
  = Constraints p (add n mn) (add n mx)

noConstraints :: Constraints
noConstraints 
  = (Constraints Nothing Nothing Nothing)

resetPrefix :: Constraints -> Constraints
resetPrefix (Constraints p mn mx)
  = Constraints Nothing mn mx

setMin :: Int -> Constraints -> Constraints
setMin n (Constraints p mn mx)
  = Constraints p (Just n) mx 

-- Sets min to the maximum of n and the current value
adjustMin :: Int -> Constraints -> Constraints
adjustMin n c@(Constraints p (Just mn) mx)
  | n > mn = Constraints p (Just n) mx
  | otherwise = c
adjustMin n c
  = c

resetMin :: Constraints -> Constraints
resetMin (Constraints p mn mx)
  = Constraints p Nothing mx 

resetMax :: Constraints -> Constraints
resetMax (Constraints p mn mx)
  = Constraints p mn Nothing 

addToMax :: Constraints -> Int -> Constraints
addToMax (Constraints p mn mx) n
  = Constraints p mn (add n mx)

addToMin :: Constraints -> Int -> Constraints
addToMin (Constraints p mn mx) n
  = Constraints p (add n mn) mx
