* In the last to last class, we saw the `($)` function. We primarily described it as a tool for cutting down parantheses. You could try using the `$` operator in a nice way to do the following.
    * Say `fs :: [a -> b]` and `x :: a`. Define `applyAll :: a -> [a -> b] -> [b]` such that `applyAll x [f1, f2, ..] == [f1 x, f2 x, ..]`
      * Hint: Can you do define `applyAll` in a clean way using `map` and `($ x)`?
    * Again, say we have `fs :: [a -> b]` and `x :: a`. Define `applyNested :: a -> [a -> b]` such that `applyNested x [f1, f2, ..] == f1 (f2 ( f3 ... ( x )))`
      * Hint: Can you define `applyNested` in a clean way using `fold` and `($)`?

* Another interesting idiom that is used in functional programming to avoid parantheses is this:
  * Instead of writing `take 3 (reverse (filter even [1..10]))`, one could write one of these:
    * `take 3 . reverse . filter even $ [1..10]`
    * `take 3 $ reverse $ filter even $ [1..10]`
  * However, the following does not typecheck, because of the way the precedence of these operators are defined:
    * `take 3 . reverse . filter even [1..10]`

* You might find the following tool useful for debugging your haskell programs.
  * `import Debug.Trace` to use the `trace` function.
    * `trace :: String -> a -> a` when called prints the string in its first argument, before returning the second argument as its result.
  * Suppose you had the following function:
    ```haskell
    f a b = ...
    ```
      * Modify it as follows, so that you can check when this function is evaluated with what arguments.
        ```haskell
        f a b
          | trace ("f " ++ show a ++ " " ++ show b) False = undefined
        f a b = ...
        ```
  * Notice that the function `trace` does not behave in the way an usual haskell function would. It has some side effects, i.e, it changes the state of the world. We'll see how Haskell deals with these cleanly, later in this course.
    * Nonetheless, one could use `trace` in their code to trace which arguments their functions are being evaluated on, among other things.
  * You might want to read about other debugging strategies [on the Haskell wiki](https://wiki.haskell.org/Debugging).

* Which of the following, if any, are true?
  * \(O(\log_2 n) \subseteq O(\log_3 n)\)
  * \(O(\log_3 n) \subseteq O(\log_2 n)\)
  * If \(\log f(n) = O(\log g(n))\), then \(f(n) = O(g(n))\)
  * Say, \(f_1(n) = O(g_1(n))\) and  \(f_2(n) = O(g_2(n))\), then
    * \(f_1(n) + f_2(n) = O(g_1(n) + g_2(n)\)
    * \(f_1(n) \times f_2(n) = O(g_1(n) \times g_2(n)\)

* There are \(n\) students standing in a row who wish to sort themselves according to their height. They come up with the following algorithm. Is the algorithm correct? If so, how much time (how many phases) do the students take to sort themselves?
  * In the odd phase, students in positions 1 and 2, 3 and 4, 5 and 6 and so on swap themselves if a taller guy is in front of a shorter guy.
  * In the even phase, students in position 2 and 3, 4 and 5, 6 and 7 and so on swap themselves if a taller guy is in front of a shorter guy.
  * The algorithm is for the students to run the odd and even phases alternatively, until everybody thinks that the guy in front of them are shorter than them.
    * ![An Illustration](https://ds055uzetaobb.cloudfront.net/image_optimizer/5be8d5416b7f7a2e60d02a0a3034f063b8d0be41.gif)
    * The reason I like to think about this algorithm is that each phase happens parallely and the combined effort of the students makes themselves sort very rapidly.
