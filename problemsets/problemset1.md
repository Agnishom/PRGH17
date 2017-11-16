* (Easy) Define `isPrime :: Integer -> Bool` that takes an `Integer` and evaluates to `True` if the integer was a prime, and False otherwise.

```
isPrime 9831655609 == True
isPrime 8128       == False
```

* (Less easy) The [Goldbach Conjecture](https://en.wikipedia.org/wiki/Goldbach%27s_conjecture) claims that every even integer greater than 2 can be expressed as a sum of two primes. Define `goldbachPartition :: Integer -> (Integer, Integer)` that given `a :: Integer` produces `b, c :: Integer` such that `isPrime b == isPrime c == True` and `a == b + c`.

```
goldbachPartition 8128910 == (103,8128807)
goldbachPartition 8128 == (5, 8123)
```

* (A little less easy) Define `allCycles :: [a] -> [[a]]`, which produces all the cyclic permutations of a given list. You might want to use the Prelude function `cycle` as a subroutine.

```
allCycles [1, 2, 3] == [[1,2,3],[2,3,1],[3,1,2]]
allCycles [] == [[]]
```
