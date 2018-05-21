mirror2 :: [(a, a)] -> [(a, a)]
mirror2 xs
  = xs ++ [(y, x) | (x, y) <- xs]

mirror3 :: [(a, a, a)] -> [(a, a, a)]
mirror3 xs
  = xs ++ [(z, y, x) | (x, y, z) <- xs]

partitions :: [a] -> [[[a]]]
partitions []
  = [[]]
partitions (x : xs)
  = [[x] : p | p <- partitions xs] ++ [(x : ys) : yss | (ys : yss) <- partitions xs]

split2 :: [a] -> [([a], [a])]
split2 xs
  = [(x, y) | [x, y] <- partitions xs]

split3 :: [a] -> [([a], [a], [a])]
split3 xs
  = [(x, y, z) | [x, y, z] <- partitions xs]

split2' :: [a] -> [([a], [a])]
split2'
  = mirror2 . split2

split3' :: [a] -> [([a], [a], [a])]
split3'
  = mirror3 . split3

