* Now that you know about list comprehensions, try defining `gridWalks` from Footnote 4.
  * Can you translate this to a definition that does not use list comprehensions?

* Hackage defines the function `scanl` in the following way:
  `scanl :: (b -> a -> b) -> b -> [a] -> [b]`is similar to foldl, but returns a list of successive reduced values from the left:
    > ```scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]```
  * Implement `scanl` by yourself.
    * Use your definition of `scanl` to prove that `last (scanl f z xs) == foldl f z xs`.
  * Using `scanl`, define a function `suffixes :: String -> [String]` that lists all the suffixes of a string.
  * (Optional) Figure out why this definition of Fibonacci Numbers work: ```fib = 0:scanl (+) 1 fib```

* Consider the two following definitions. What do they do?
  * `f1 xs = foldr (++) [] xs`
  * `f2 xs = foldl f [] xs where f ys y = (y:ys)`
  
* Define the following functions using `foldr`: `concat`, `filter`, `map`, `elem`.