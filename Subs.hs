module Main where 

remove xs ys = let n = (find_in xs ys 0 0) in if n == -1 then [] else remove_from ys n (length xs)

remove_from ys 0 0 = ys
remove_from (y:ys) 0 m = remove_from ys 0 (m-1)
remove_from (y:ys) n m = y:(remove_from ys (n-1) m)

find_in [] ys n f = n
find_in xs [] n f = -1
find_in (x:xs) (y:ys) n 0 = if x==y 
							then find_in xs ys n 1 
							else find_in (x:xs) (ys) (n+1) 0
find_in (x:xs) (y:ys) n 1 = if x==y 
							then find_in xs ys n 1
							else -1