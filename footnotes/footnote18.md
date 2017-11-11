* In Monday's class, we had been discussing the problem of reading lines until we see a blank line, and then putting them in `IO [String]`. We saw a naive way to do it, but was looking for a more elegant abstraction for the underlying "loop". Here are two possible solutions.
  * Define a `doWhileM` as follows:
    ```
    doWhileM :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
    doWhileM p []     = return []
    doWhileM p (x:xs) = do
        s <- x
        if p s
        then do
            l <- doWhileM p xs
            return (s:l)
        else
            return []
    ```
      * Then express your function as follows:
        ```        
        myReadList :: IO [String]
        myReadList = doWhileM (/= "") (repeat getLine)
        ```
  * Another solution is to use [`unfoldWhileM`](http://hackage.haskell.org/package/monad-loops-0.4.3/docs/Control-Monad-Loops.html#v:unfoldWhileM) from `Control.Monad`.
    * The documentation describes `unfoldWhileM` as follows:
      > Repeatedly evaluates the second argument until the value satisfies the given predicate, and returns a list of all values that satisfied the predicate. Discards the final one (which failed the predicate).

        * Go ahead and try defining your own `unfoldWhileM`
    * Using `unfoldWhileM`, we can express `myReadList` as follows:
      ```
      myReadList :: IO [String]
      myReadList = unfoldWhileM (/= "") getLine
      ```
* In the spirit of the last bullet, it might be a good idea to explain what unfoldings are. You can get `unfoldr` from `Data.List`.
  * `unfoldr` is the dual to `foldr`. While `foldr` takes a list and reduces it to a summary value, `unfoldr` takes a seed value, and builds a list out of it.
    * The related concepts in category theory are known as [Catamorphism](https://en.wikipedia.org/wiki/Catamorphism) and [Anamorphism](https://en.wikipedia.org/wiki/Anamorphism).
  * Let's look at an example to illustrate this:
    ```
    fibs :: [Int]
    fibs = unfoldr (\(x, y) -> Just (x, (y, x+y))) (0,1)
    ```
  * The way to think about the type signature of `unfoldr` is as follows:
    `unfoldr :: (state -> Maybe (value, state)) -> state -> [value]`
    * The `Maybe` is to denote that a state have no successors, in which case, you get nothing.
    * Based on this information, can you define `unfoldr` by yourself?
  * Like folds, unfolds are not restricted to just lists either. Here is an unfold that works on trees.
    ```
    data Tree a = Empty | Fork (Tree a) a (Tree a)
      deriving Show

    unfold :: (b -> Maybe (b,a,b)) -> b -> Tree a
    unfold unspool x = case unspool x of
                       Nothing -> Empty
                       Just (l,x,r) -> Fork (unfold unspool l) x (unfold unspool r)
    ```
    * Using this `unfold`, we can solve the problem of `funTree` from Assignment 3 as follows:
      ```
      funTree = ana (\ n -> Just (n+1, n, n+1)) 0
      ```
