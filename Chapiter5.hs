module Chapiter5 (Deque(..), TestDeque(..)) where

import Prelude hiding (head, tail, last, init)

import qualified Data.List as L

class Deque d where
  empty :: d a
  isEmpty :: d a -> Bool

  head :: d a -> a
  last :: d a -> a

  tail :: d a -> d a
  init :: d a -> d a

  cons :: d a -> a -> d a
  snoc :: d a -> a -> d a

data TestDeque a = D [a] [a]

instance (Show a) => Show (TestDeque a) where
  show (D xs ys) = "Deque " ++ show (xs ++ reverse ys)


showDeque :: (Show a) => TestDeque a -> String
showDeque (D xs ys) = "D " ++ show xs ++ " " ++ show ys

splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = L.splitAt pivot xs
        where pivot = length xs `div` 2

-- | Note: keeping an invariant (ie: always an element in the first list) would
-- |       simplify a lot these functions
instance Deque TestDeque where
  empty = D [] []
  isEmpty (D [] []) = True
  isEmpty _ = False

  head (D [] ys) = L.last ys
  head (D xs _) = L.head xs
  last (D xs []) = L.last xs
  last (D _ ys) = L.head ys

  tail (D [] []) = error "Empty deque"
  tail (D [] ys) = D (L.tail . reverse $ xs') ys'
    where (ys', xs') = splitInHalf ys
  tail (D [_] ys) = D (reverse xs') ys'
    where (ys', xs') = splitInHalf ys
  tail (D (_:xs) ys) = D xs ys

  init (D [] []) = error "Empty deque"
  init (D xs []) = D xs' (L.tail . reverse $ ys')
    where (xs', ys') = splitInHalf xs
  init (D xs [_]) = D xs' (reverse ys')
    where (xs', ys') = splitInHalf xs
  init (D xs (_:ys)) = D xs ys

  cons (D xs ys) e = D (e:xs) ys
  snoc (D xs ys) e = D xs (e:ys)
