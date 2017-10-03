* You may have realised that lists in Haskell, can be thought of either the empty list `[]` or some `a:[a]` where `a` is some type. This idea will be explained in greater detail later in class. Here are some things to think about, though.
  * Although this looks like an inductive definition, an actual inductive definition could not have given you infinite lists. This is really [coinduction](https://en.wikipedia.org/wiki/Coinduction). You could read about this idea from the following resources if you want to:
    * [What is coinduction? - CS StackExchange](https://cs.stackexchange.com/questions/525/what-is-coinduction)
    * [Data and Codata - Neighborhood of Infinity (a blog post)](http://blog.sigfpe.com/2007/07/data-and-codata.html)
    * [Coinduction and Bisimulation by David Sangiorgi (CMI Library Link)](http://library.cmi.ac.in/cgi-bin/koha/opac-detail.pl?biblionumber=8830&query_desc=kw%2Cwrdl%3A%20Coinduction%20and%20Bisimulation)

* Count the number of even elements in a finite list. Implement `evenCount`.
  * (Optional) Generalize. Suppose `p :: a -> Bool` is a predicate. Define `pCount :: (a -> Bool) -> [a] -> Int` to be the count of all those elements in a list which satisfy `p`.

* Generalize the `gcd :: Int -> Int -> Int` that you have defined previously to `nGcd :: [Int] -> Int` such that `nGcd xs` is the gcd of the elements of a finite list of integers.
  * Also generalize `lcm` to `nLcm`.

* With the help of `fib` that you have implemented before, implement `nFib :: Int -> [Int]` where `nFib n` will give you the first `n` fibonacci numbers.
  * (Optional) Implement `fibs :: [Int]` where `fibs` is an infinite list such that `fibs !! n == fibs n`

* Define `bigEnough :: Int -> [Int] -> Bool` such that `bigEnough n xs == True` iff `xs` has at least `n` elements.
  * Use `length` to directly define this.
  * Directly use recursion to define this, to get the hang of it. 
    * Can you think of a reason why this strategy might be more preferred in some cases?

* Implement `nOnes :: Int -> [Int]` where `nOnes n` returns a list of `n` `1`'s. So, e.g, `nOnes 5 == [1, 1, 1, 1, 1]` in the following ways:
  * By directly recursing on `n`.
  * (Optional) Implement an infinite list `ones :: [Int]` as `ones = 1:ones`. Then, define `nOnes` as `nOnes n = take n ones`
  * (Optional) Implement `nOnes` as `nOnes n = take n [1, 1 ..]`
  
* (Optional) Previously you had only counted the monotonic number of lattice paths from `(0, 0)` to `(m, n)`. Can you actually figure out what all those paths would be?
  * Define `gridWalks :: (Int, Int) -> [[(Int, Int)]]` which is a list of all the walks from  `(0, 0)` to `(m, n)`. You might want to use [list comprehensions](http://learnyouahaskell.com/starting-out#im-a-list-comprehension).
  * This problem was suggested by Arijit Shaw.

* (Optional) Kushal Vijay Padole and Indraneel Pratap Mukhopadhyaya suggested me the following way of computing `lcm a b`. Please implement this.
  * Generate a list of multiples of `a`.
  * For every element, check whether it is also divisible by `b`.
  * Take the first such element.