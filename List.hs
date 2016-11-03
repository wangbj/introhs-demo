module List (
    List (..)
  , isEmpty
  , notEmpty
  , head
  , tail
  , singleton
  , length
  , concat
  , reverse
  , map
  , filter
  ) where

import Prelude hiding (head, tail, length, concat, reverse, map, filter, null, sum, foldl)
import Data.Char (ord, chr, toUpper)

-- ^ A list is either empty, or an item concat to another list
data List a = Nil | a :. (List a) -- ^a is type variable

list0 = Nil

list1, list2 :: List Int
list1 = 1 :. 2:. 3 :. 4 :. 5 :. 6 :. 7 :. 8 :. Nil
list2 = 7 :. 8 :. 9 :. 10 :. Nil

charList1, charList2 :: List Char
charList1 = 'h' :. 'e' :. 'l' :. 'l' :. 'o' :. Nil
charList2 = 'w' :. 'o' :. 'r' :. 'l' :. 'd' :. Nil

-- multi-clause defintion
isEmpty Nil = True
isEmpty _   = False

notEmpty = not . isEmpty -- function composition

head :: List a -> a
head Nil = error "empty list"
head (x :. xs) = x

tail :: List a -> List a
tail Nil = error "empty list"
tail (x :. xs) = xs

singleton :: a -> List a
singleton x = x :. Nil

length' :: List a -> Int
length' Nil = 0
length' (x :. xs) = 1 + (length' xs)

length :: List a -> Int
length xs
  | isEmpty  xs = 0           -- pattern matching
  | notEmpty xs = 1 + length (tail xs)

concat :: List a -> List a -> List a
concat Nil ys = ys
concat xs Nil = xs
concat (x :. xs) yys = x :. concat xs yys

reverse :: List a -> List a
reverse Nil = Nil
reverse ( x :. xs) = reverse xs `concat` (x :. Nil)

-- ^ generalize recursions
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f ( x :. xs) = f x :. map f xs

filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (x :. xs)
  | p x       = x :. filter p xs
  | not (p x) = filter p xs

toUpperString :: List Char -> List Char
toUpperString = map toUpper

--
instance (Show a) => Show (List a) where
  show xxs = "[" ++ go xxs ++ "]"
    where
      go Nil = []
      go (x :. xs) = show x ++ "," ++ go xs

infixr 1 :.

