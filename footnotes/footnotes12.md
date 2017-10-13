* I got this teaser from some of you who had been discussing Haskell a day before the midsem
  * Which (if any) of these two typecheck?
    * `map . filter`
    * `filter . map`
  * For the function which does typecheck, give an intuitive description of what it does and how.

* Figuring out what makes a Haskell program run fast in practice can be tricky! (See [footnote3](https://github.com/Agnishom/PRGH17/blob/master/footnotes/footnote03.md) for a simple way of measuring the running time.) Some of you have shown us some code, whose efficiency we were unable to explain. If any of you do have an explanation, please let us know.
  * [Here](https://gist.github.com/Agnishom/c45f90c53eb0e3c2152bc960735fa03e#file-isprime-hs), Diganta and Devang have defined two functions for primality checking, one with list comprehensions, and the other with naive recursion. For some reason, the one with list comprehensions is way faster!
  * Siddharth [implemented](https://gist.github.com/Agnishom/c45f90c53eb0e3c2152bc960735fa03e#file-selectleader-hs) the `selectLeader` function (from the second assignment) in two ways, with `take` and `drop`, and with just plain recursion. Again, for some reason, the `take`-`drop` implementation is much faster.
  * [This](https://stackoverflow.com/questions/11466284/how-is-this-fibonacci-function-memoized/) is an interesting StackOverflow post that discusses an interesting example of a function, which appears to be faster when defined in Pointfree style.

* As briefly mentioned in the previous bullet, the [Pointfree Style](https://wiki.haskell.org/Pointfree), is a style of functional programming where functions are defined without explicitly talking about the input. While sometimes elegant, this could easily ofuscate code.
  * As an example, consider rewriting `sum xs = foldr (+) 0 xs` as `sum = foldr (+) 0`
    * As an extreme example, consider rewriting `dotProduct v1 v2 = sum (zipWith (*) v1 v2)` as `dotProduct = (sum .) . zipWith (*)`!
  * You could try asking [pointfree.io](http://pointfree.io/) to convert a code fragment to pointfree, in an automated way.
  * But doesn't pointfree have more `.`s?
    * Yes, it does. But it is called pointfree because specifying the function does not require us to specify the point on which the function is acting.
      * Also see, [Pointfree Topology](http://www.wikiwand.com/en/Pointless_topology)
  * [Can every function be expressed in point-free form?](https://stackoverflow.com/questions/13184294/can-any-function-be-reduced-to-a-point-free-form)
    * Closely related objects are combinators, which we mentioned in [footnote10](https://github.com/Agnishom/PRGH17/blob/master/footnotes/footnote10.md)

* You saw the [8-Queens Problem](https://en.wikipedia.org/wiki/Eight_queens_puzzle) in class.
  * Here is my [attempt](https://codereview.stackexchange.com/questions/177823/n-queens-with-haskell) for a haskell solution.
  * Here are some other related problems and variants that come to mind:
    * Can you solve the problem for 10 Queens?
      * (Possibly not a programming problem) What is the largest value of N for which N Queens does not have a solution?
    * Can you find the number of configurations for 8 Queens?
      * Can you count the number of configurations, _ignoring rotations and reflections_?
    * Can you solve the puzzle for Rooks?
      * Can you solve the puzzle for Amazons (a chess piece that combines the power of a knight and a queen)? ([Does your answer match with mine?](https://brilliant.org/problems/n-queens-reloaded/))
  * (Open Ended) Are there other interesting strategies to solve this problem? Maybe with some heuristic? Or by trying to iteratively repair a partial solution? [By reformulating the problem as an integer linear program](https://math.stackexchange.com/questions/1537689/linear-constraints-to-placing-n-queens-on-an-n-x-n-chessboard)?
  * There are constraint solving tools like Minizinc, Picat, etc where the solver solves the problem for you once you model the problem for it. [Here](https://github.com/MiniZinc/minizinc-benchmarks/blob/master/queens/queens.mzn) is how it is done in Minizinc.
  * Consider this problem: Given an N by N board, on which a few queens have been placed already, can it be extended to a full N-Queens solution? This problem is difficult, [in a certain precise sense](https://blogs.cs.st-andrews.ac.uk/csblog/2017/08/31/n-queens-completion-is-np-complete/)

* You probably have seen in class, how to figure out whether a graph is connected. Can you define a function that checks if the graph contains a cycle?
