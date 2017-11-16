* (Easy) [99 Bottles of Beer](https://en.wikipedia.org/wiki/99_Bottles_of_Beer) is a folk song that is often used in programming exercises to demonstrate loops. Define an action `bottlesOfBeer :: Int -> IO ()` that prints this song with the given number of bottles.
  * Notice the behaviour of the song when there is 1 bottle left, and when we run out of bottles.

```
*Main> bottlesOfBeer 10
10 bottles of beer on the wall, 10 bottles of beer.
Take one down, pass it around, 9 bottles of beer on the wall...
9 bottles of beer on the wall, 9 bottles of beer.
Take one down, pass it around, 8 bottles of beer on the wall...
8 bottles of beer on the wall, 8 bottles of beer.
Take one down, pass it around, 7 bottles of beer on the wall...
7 bottles of beer on the wall, 7 bottles of beer.
Take one down, pass it around, 6 bottles of beer on the wall...
6 bottles of beer on the wall, 6 bottles of beer.
Take one down, pass it around, 5 bottles of beer on the wall...
5 bottles of beer on the wall, 5 bottles of beer.
Take one down, pass it around, 4 bottles of beer on the wall...
4 bottles of beer on the wall, 4 bottles of beer.
Take one down, pass it around, 3 bottles of beer on the wall...
3 bottles of beer on the wall, 3 bottles of beer.
Take one down, pass it around, 2 bottles of beer on the wall...
2 bottles of beer on the wall, 2 bottles of beer.
Take one down, pass it around, 1 bottle of beer on the wall...
1 bottle of beer on the wall, 1 bottle of beer.
Take one down, pass it around, no bottles of beer on the wall...
No more bottles of beer on the wall, no more bottles of beer.
```

* (Slightly less easy) Define an action `fizzBuzz :: Int -> Int -> IO ()` which returns an action which when executed prints the numbers from `m` to `n` sequentially, such that if the current number is divisible by 3, `Fizz` is printed, if the current number is `5`, `Buzz` is printed, and if it is both, then `FizzBuzz` is printed. If it is not divisible by either, just the number is printed.
  * Here is the result of running `fizzBuzz 10 30`:
  ```
  *Main> fizzBuzz 10 30
  Buzz
  11
  Fizz
  13
  14
  FizzBuzz
  16
  17
  Fizz
  19
  Buzz
  Fizz
  22
  23
  Fizz
  Buzz
  26
  Fizz
  28
  29
  FizzBuzz
  ```

* (Less Easy) Define `reverseCapitalizeHalf :: IO ()` that takes in a string in the first line, and then another string in the second line. If the string taken in at first is `"R"`, it reverses the second string, if the string taken in at first is `"H"`, it prints the first ``length line `div` 2`` characters of the second string, if it is "C", it capitalizes the second string and prints it, and if the first string is something else, it prints `"Haskell is awesome"`

```
*Main> reverseCapitalizeHalf
R
Hey
yeH
*Main> reverseCapitalizeHalf
C
haskell
HASKELL
*Main> reverseCapitalizeHalf
H
haskell
has
*Main> reverseCapitalizeHalf
sbvui
agnishom
Haskell is awesome
```

* (Not not easy) Define an action `averageNumbers :: IO ()` that takes an integer `n` as an input in the first line, and then accepts `n` more lines consisting of numbers which are interpretted as `Double`s. And then, prints their average.
  * To clarify, assume that the first line can always be legally parsed with `readLn :: IO Int` and the subsequent lines with `readLn :: IO Double`
  * Divide the numbers after you have added them, not vice versa. (Real number arithmetic is distributive, Floating Point Arithmetic is not necessarily so)

```
*Main> averageNumbers
6
15
19
18.2
18.1
9.0
2.133
13.572166666666666
```

* (Medium) Write a function `fileSearch :: String -> String -> IO (Maybe Int)` that checks if any line of a given file is the specified string. If it does, then the result should be `Just x` wrapped in `IO` where `x` is the line number where it occurs first, and `Nothing` wrapped in `IO` otherwise.

Consider the following files

`1.in`
```
alpha
beta
gamma
```

`2.in`
```
alpha
beta
alpha
gamma
```

`3.in`
```
beta
gamma
```

`4.in`
```
alphaalpha
gamma
```

This is how your program should behave on these files:

```
*Main> fileSearch "1.in" "alpha"
Just 1
*Main> fileSearch "1.in" "gamma"
Just 3
*Main> fileSearch "2.in" "alpha"
Just 1
*Main> fileSearch "3.in" "alpha"
Nothing
*Main> fileSearch "4.in" "alpha"
Nothing
```
