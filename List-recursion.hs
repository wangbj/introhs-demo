import Prelude hiding (sum, foldl)

list4 = [7, 6, 3, 5, 2, 8, 9, 1]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort bigger
  where
    smaller = filter (<= x) xs
    bigger  = filter ( > x) xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

prod :: (Num a) => [a] -> a
prod [] = 1
prod (x:xs) = x * prod xs

-- avoid boilerplate

-- ^left fold
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

sum'  = foldl (+) 0
prod' = foldl (*) 1

len = foldl go 0
  where
    go r x = 1 + r
    -- (b -> a -> b)
-- =>
-- :t const =>
-- const :: a -> b -> a
len' = foldl (const . succ) 0
