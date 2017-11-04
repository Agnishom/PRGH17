* Write an action `reverseLines :: IO ()` that reads lines sequentially and keeps printing their reverse, until it sees the line saying "END"

* Haskell is sometimes referred to as the best imparative language. ([Why?](https://stackoverflow.com/questions/6622524/why-is-haskell-sometimes-referred-to-as-best-imperative-language))
  * Let's look at an example from [a blog](http://conscientiousprogrammer.com/blog/2015/12/11/24-days-of-hackage-2015-day-11-monad-loops-avoiding-writing-recursive-functions-by-refactoring/). Here is how one would define a function `whileM_ :: (Monad m) => m Bool -> m a -> m ()`. This is supposed to be exactly like a while loop in an imperative language.
  ```
  whileM_ p f = go
    where go = do
            x <- p
            if x
                then f >> go
                else return ()
  ```

    * You might have used some similar idea to solve the problem in the previous bullet.
  * Using the `whileM_` combinator above, one could write some code like this:
    ```
    logIn3 :: IO ()
    logIn3 = do
      putStrLn "% Enter password:"
      whileM_ (do
                 guess <- getLine
                 return (guess /= "secret")
              ) $ do
        putStrLn "% Wrong password!"
        putStrLn "% Try again:"
      putStrLn "$ Congratulations!"
    ```

* Consider the prelude function `sequence_` which takes in a list of actions, and combines them sequentially.
  * Can you implement it with `do` syntax with recursion?
  * Can you define it with `foldr` and `(>>)`?

* If you have seen some imperative language before: Is the Haskell `return` function similar to the imperative `return` statement?
  * Spoiler Alert: No!

* It might not always be considered a good idea to use the `do` notation. Why'd you use `do { text <- getLine; return text }`, when you can use `getLine` to the same effect?
  * See: [Do notation considered harmful?](https://wiki.haskell.org/Do_notation_considered_harmful)
  * Anecdote: In programming language theory, it is common practice to use the phrase [considered harmful](https://en.wikipedia.org/wiki/Considered_harmful). Edwiger Dijkstra started off this by publishing a paper called "GOTO statement considered harmful", which someone responded to by publishing an article, "'GOTO statement considered harmful' considered harmful", to which somone again responded to saying "'''GOTO statement considered harmful' considered harmful', considered harmful?"
