data Tree a = Tree { value :: a
                   , left :: Tree a
                   , right :: Tree a }
              | Empty deriving Show


testTree :: Tree Int
testTree = Tree 10
              (Tree 3
                  (Tree 1
                      Empty
                      (Tree 2 Empty Empty))
                  Empty)
              (Tree 20
                  Empty
                  (Tree 30 Empty Empty))
              

-- Exercise 2.2
member :: (Eq a, Ord a) => a -> Tree a -> Bool
member e t = member' t t
  where member' candidate Empty = value candidate == e
        member' candidate curr@(Tree v l r) =
          if e < v then member' candidate l
          else member' curr r
          
  

