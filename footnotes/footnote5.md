
* Try implementing `interleavingSplit :: [Int] -> ([Int], [Int])` where the first list contains all the even indices and the second list contains all the odd indices.
  * Remember indexing starts from 0.

* We are going to try to sort a given list with [insertion sort](https://en.wikipedia.org/wiki/Insertion_sort).
  * Implement a function `insert :: Int -> [Int] -> Int` such that whenever `xs` is sorted, `insert x xs` is sorted, for any `x :: Int`.
    * Hint: What do you do when `xs` is `[]`?
    * Hint: When can you add `x` directly to the front of `xs`?
  * Implement `insertionSort' :: [Int] -> [Int] -> [Int]` by `insertionSort' ys (x:xs) = insertionSort' (insert x ys) xs`. (This is not the full definition. You should complete the actual definition.)
  * Define `insertionSort :: [Int] -> [Int]` by `insertionSort = insertionSort' []`

* Shankar Ram sent me two versions of insertion sort.
  * One which doesn't work.
  ```haskell
  sortlist1 :: [Int] -> [Int]
  sortlist1 l
      | tail l == [] = l
      | otherwise = sortlist l []
  
  sortlist :: [Int] -> [Int] -> [Int]
  sortlist (x:xs) [] = sortlist xs (x:[])
  sortlist [] l = l
  sortlist (x:xs) (y:ys)
      | x<=y = sortlist xs (x:(y:ys))
      | otherwise = y:(sortlist (x:xs) ys)
    ```
  * One which does work.
  ```haskell
  sortlist1 :: [Int] -> [Int]
  sortlist1 l
      | tail l == [] = l
      | otherwise = sortlist l []
  
  sortlist :: [Int] -> [Int] -> [Int]
  sortlist (x:xs) [] = sortlist xs (x:[])
  sortlist [] l = l
  sortlist (x:xs) (y:ys)
      | x<=y = sortlist xs (x:(y:ys))
      | otherwise = sortlist xs (y:(sortlist (x:[]) ys))
  ```
    * Analyze why one works and the other doesn't.
    * You might have already heard that reading programs from other programmers is harder than writing programs for yourself :).

* On coding style. Remember that reading code is much harder than writing code. Ergo, the 'good' programmer writes readable and maintainable code which is well commented.
  * Use [`where` clauses](http://learnyouahaskell.com/syntax-in-functions#where) for auxilary functions.
  * Indent blocks of code appropriately.
  * Use [comments](https://wiki.haskell.org/Commenting) whenever you do something non-trivial.
    * Remember what seems trivial to you now might not seem trivial to you when you review your code after a few months.
    * There is a joke that I heard about this: When I wrote my code, only God and I knew how it worked. Now, only God does.
  * Use names that convey some information about the function or constant.
    * [Why do mathematicians use single letter variables?](https://math.stackexchange.com/questions/24241/why-do-mathematicians-use-single-letter-variables)