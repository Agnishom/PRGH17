* We define a binary tree like this: ```data Tree a = Empty | Fork (Tree a) a (Tree a)```.
  * The depth of the tree is the length of the longest path from the root to a leaf. Define `depth :: Tree a -> Int` that computes the depth of a given tree.
  * The diameter of the tree is the longest path in the tree (that does not repeat any vertices). Define `diameter :: Tree a -> Int` that computes the diameter of a given tree.

* A `Tree Int` is a tree with some coins on some of its vertices, given by the corresponding `Int` at that node. You may assume that the number of coins are always non-negative.
  * A monkey is trying to climb down a (finite) `Tree Int`, starting from the root. Compute `maxCoinsDescend :: Tree Int -> Int`, the maximum number of coins it could collect along a path starting from the root.
  * What if he were to just traverse any path in the tree (that does not repeat vertices)? Compute `maxCoinsPath :: Tree Int -> Int`, the maximum number of coins it could collect along any path.

* We shall represent a `BoringTree` as a nested sequence of parantheses.
  * Make `BoringTree` an instance of the typeclass `Show`, where `show :: BoringTree -> String` returns this representation.

* Create an infinite tree `funTree :: Tree Int` such that at level `n` of the tree, all nodes hold the value `n`.
  * Hint: How would you create an infinite list [1,2,3..] without using [1..]?
  * Define `chopOff :: Int -> Tree a -> Tree a`, that chops off all but the first (n+1) layers from the root. Your chopOff should terminate on an infinite tree as well.

```
examples =
  [
    depth Empty == 0,
    depth (Fork Empty () Empty) == 1,
    depth (Fork Empty () (Fork Empty () (Fork Empty () Empty))) == 3,
    diameter Empty == 0,
    diameter (Fork Empty () Empty) == 2,
    diameter (Fork Empty () (Fork Empty () (Fork Empty () Empty))) == 4,
    maxCoinsDescend Empty == 0,
    maxCoinsDescend (Fork Empty 10 Empty) == 10,
    maxCoinsDescend (Fork Empty 10 (Fork Empty 20 (Fork Empty 30 Empty))) == 60,
    maxCoinsPath Empty == 0,
    maxCoinsPath (Fork Empty 10 Empty) == 10,
    maxCoinsPath (Fork Empty 10 (Fork Empty 20 (Fork Empty 30 Empty))) == 60,
    show Leaf == ".",
    show (Leaf `Branch` Leaf) == "(..)",
    show ((Leaf `Branch` Leaf) `Branch` (Leaf `Branch` Leaf)) == "((..)(..))",
    chopOff 0 funTree == Fork Empty 1 Empty,
    chopOff 1 funTree == Fork (Fork Empty 2 Empty) 1 (Fork Empty 2 Empty),
    chopOff 3 funTree == Fork (Fork (Fork (Fork Empty 4 Empty) 3 (Fork Empty 4 Empty)) 2 (Fork (Fork Empty 4 Empty) 3 (Fork Empty 4 Empty))) 1 (Fork (Fork (Fork Empty 4 Empty) 3 (Fork Empty 4 Empty)) 2 (Fork (Fork Empty 4 Empty) 3 (Fork Empty 4 Empty)))
  ]
```
