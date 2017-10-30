* Data Structures which are popular in Functional Programming Paradigms are often different from the usual data structures in other settings. A canonical example is Haskell's list vs arrays.
  * The real difficulty comes from the fact that there is no notion of memory modification or state in the philosophy of Haskell. Hence, data structures are immutable.
    * This is an example from [Wikipedia](https://en.wikipedia.org/wiki/Persistent_data_structure).
    Suppose `xs = [0, 1, 2]` and `ys = [3, 4, 5]`. Then, they are represented in memory this way.
    ![](https://upload.wikimedia.org/wikipedia/commons/thumb/9/98/Purely_functional_list_before.svg/168px-Purely_functional_list_before.svg.png)
    * When we go `zs = xs ++ ys`, the memory looks like the following:
    ![](https://upload.wikimedia.org/wikipedia/commons/2/2e/Purely_functional_list_after.svg)
  * A very interesting textbook that discusses the design principles of such data structures is: [Purely Functional Data Structures by Chris Okasaki](http://library.cmi.ac.in/cgi-bin/koha/opac-detail.pl?biblionumber=9572)

* Suppose `data STree a = Empty | Node (STree a) a (STree a)`. Define `isBoundedSTree :: (Ord a) => Maybe a -> Maybe a -> STree a -> Bool`, where `isBoundedSTree lower higher tree` is `True`, if and only if `tree` is a legit binary search tree with values of nodes between `lower` and `higher`. If either `lower` or `higher` is `Nothing`, there is no lower bound or higher bound (respectively), on the values.
  * How much time does your method `isBoundedSTree` take to evaluate on a search tree of `n` nodes?
  * Can you define `isSTree :: (Ord a) => STree a -> Bool` that we defined in class in terms of `isBoundedSTree`?  

* Define `insertionSequence :: (Ord a) => STree a -> [a]`, such that `insertionSequence tree` will return a list containing the elements of `tree` in such a order that inserting them in the same sequence into an empty tree will reconstruct the original tree. That is, `foldl insert Empty (insertionSequence tree) == tree`.

* Recall the definitions of `inOrder :: STree a -> [a]`, `postOrder`, `preOrder` from class. Given `inOrder tree`, can one reconstruct the tree uniquely? If not, why not? Can one reconstruct the tree given `(inOrder tree, postOrder tree)`? How about `(postOrder tree, preOrder tree)`

* Modify the Binary Search Tree data structure to make it behave like a Frequency Distribution Table. It should support the following operations:
  * `insert`: If the given element is present in the structure, increment it's count, otherwise, add it with count 1.
  * `delete`: If the given element is present in the structure, decrement it's count. Otherwise, raise an error.
  * `count`: Check how many copies of the current element are in the structure.
