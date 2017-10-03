* Define a function `digitSum` that returns the sum of all the digits of the decimal representation of the number. E.g, `digitSum 1234 == 10`, `digitSum 0 == 0`
  * Take this one step forward. Define [`digitalRoot`](https://en.wikipedia.org/wiki/Digital_root) which repeatedly sums the digit until you have just one digit. E.g, `digitalRoot 65536 == digitalRoot (6 + 5 + 5 + 3 + 6) == digitalRoot 25 == digitalRoot (2 + 5) == digitalRoot 7 = 7`

* You have defined `gcd` in class. Use this as an auxilary function to define `lcm`. 
  * `gcd` and `lcm` are already bult-in functions in the prelude. You might want to call your functions `myGcd` and `myLcm` for convenience.
  * Define `threeGCD` that gives you the gcd of three numbers.

* Let's try to write a function `mySqrt :: Float -> Float -> Float`, where `y = mySqrt eps x` satisfies `abs ((y^2) - x) <= eps`. We shall use the bisection method, which is roughly sketched below.
  * Consider the function `f y = y^2 - x`. We'd like to solve to know what value of `y` satisfies `f y < eps`.
  * Choose two values `a` and `b` such that `f a` is negative, but `f b` is positive. Consider `mid = (a+b)/2`
  * 
    * If `abs (f mid) < eps`, you are done!
    * Otherwise
      * If `f mid` is positive, try looking between `a` and `mid`.
      * If `f mid` is negative, try looking between `mid` and `b`.
  * Repeat!

* On an `m` by `n` grid, you start at the origin, and have to walk up to the top right corner. For example, here are all the six ways you can do this on a `2 x 2` grid.
  * ![](http://mathworld.wolfram.com/images/eps-gif/YoungDiagramLatticePaths_700.gif)
  * Define `gridWalk m n` that solves the problem for an `m x n` grid.
    * **Hint:** If you somehow knew `gridWalk (m-1) n` and `gridWalk m (n-1)`, does it help?
  * Use a similar idea to implement `choose n k` which stands for the number of ways to choose `k` objects out of `n` objects.
    * Can you directly use `gridWalk` to implement `choose`?
    * Can you directly use `choose` to implement `gridWalk`?

* (Optional) Solve problems in the [Functional Programming section on Hackerrank](https://www.hackerrank.com/domains/fp/intro)
    * You might want to solve other problems in [Hackerrank](https://www.hackerrank.com/), [SPOJ](http://spoj.com), [Project Euler](https://projecteuler.net/archives), [DailyProgrammer's Subreddit](https://www.reddit.com/r/dailyprogrammer/), etc as well.