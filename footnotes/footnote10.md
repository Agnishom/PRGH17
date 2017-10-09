* Let's revisit your definition of `elem` using folds from `footnote9`.
  * Suppose you define ```isPrime x = x `myElem` primes``` where `primes` were an infinite list of `primes`, and `myElem` is your definition for `elem` using folds. Which fold should you use to make sure that this definition of `isPrime` terminates whenever `x` is actually a prime?

* Folds are rather useful operations. We'll later see how to generalize them to arbitrary data structures, when we see more data structures in Haskell.
  * Here is another fold, the _tree like_ fold which I found on the [Haskell Wiki page on folds](https://wiki.haskell.org/Fold#Tree-like_folds):
    ```haskell
    foldt f z []     = z
    foldt f _ [x]    = x
    foldt f z xs     = foldt f z (pairs f xs)
      where
        pairs f (x:y:t)  = f x y : pairs f t
        pairs _ t        = t
    ```
      * Here is an example which illustrate what it does:
        ```
        *Main> foldt (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])
        "((((1+2)+(3+4))+((5+6)+(7+8)))+(((9+10)+(11+12))+13))"
        ```
      * Analyze and understand the above fold. We'll use this to do something cool in the next bullet.

* In footnote5, we saw how to sort a list using a technique called _insertion sort_. We'll see a different way of doing this, called the merge sort.
  * Define `sorted :: [Int] -> Bool` (using folds!) that will return `True` iff the list given is sorted.
    * Do you know why `sorted :: [a] -> Bool` wouldn't work? We'll answer this question in a later lecture.
  * Suppose you have two lists `xs :: [Int]` and `ys :: [Int]`, such that `sorted xs == True` and `sorted ys == True`. Define `merge :: [Int] -> [Int] -> [Int]` such that `merge xs ys` is a new list consisting of elements all elements from `xs` and `ys` in a sorted permutation.
    * Fill in (rewrite the undefined's)the following definition for `merge`
    ```haskell
    merge :: [Int] -> [Int] -> [Int]
    merge [] xs           = undefined
    merge ys []           = undefined
    merge (x:xs) (y:ys)
      | x <= y            = undefined
      | otherwise         = undefined
    ```
      * Given two finite lists of size `m` and `n`, can you characterize how much time merging them together takes?
        * We'll discuss this more precisely in a later lecture.
  * Now that we have `merge`, one first approach to define sorting would be this:
  ```haskell
  mySort :: [Int] -> [Int]
  mySort = foldr (\x ys-> merge [x] ys) []
  ```
  * But, a more interesting idea would be this:
    *
      * Break the list into two sublists.
      * Sort them recursively
      * Merge them.
    * This translates this into code as follows:
      ```haskell
      mergesort xs = foldt merge [] [[x] | x <- xs]
      ```
      * Work through an example to appreciate how this works. Is this any better than `insertionSort`?

* In class, we talked about `flip` and `const`. These are called [_combinators_](https://wiki.haskell.org/Combinator). They are functions which do not involve any variables which are not their argument.
  * How do you interpret the following expressions:
    * `flip const 6`
    * `flip const const` (Hint: Try the previous problem first)
  * A certain subset of combinators (interpretted without types) can actually form a structure so rich that it would be powerful enough to express any computation on natural numbers. See [SKI Combinator Calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) and [BCKW System](https://en.wikipedia.org/wiki/B,_C,_K,_W_system)
    * *To Mock a Mockingbird* is a nice read on this. We mentioned this in footnote2.
