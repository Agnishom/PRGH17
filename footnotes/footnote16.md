* Suppose `action1 :: IO ()` and `action2 :: IO ()` are two action items. Define `chain :: IO () -> IO () -> IO ()` such that ```action1 `chain` action2``` is an action that does `action1` first, and then `action2`.
  * Can you generalize this where `action1` is of type `IO a` and `action2` is of type `IO b`?

* Think: Can we have a function `extract :: IO a -> a`?

* Write a program that reads lines and prints them in reverse order, until it sees the line "end", in which case it quits. You may want to use the prelude function `getLine :: IO String`.

* Write a program that uses `getClockTime :: IO ClockTime` from the module `System.Time` to print the current time, when run.

* Write (and compile) a [guessing game](https://doc.rust-lang.org/book/second-edition/ch02-00-guessing-game-tutorial.html). The guessing game should think of a number between 1 and 100 and ask the user to guess it, and give hints on whether it is too big or too small. Here is a sample transcript of what the gameplay should be like:
  ```
  Guess the number!
  The secret number is: 61
  Please input your guess.
  10
  You guessed: 10
  Too small!
  Please input your guess.
  99
  You guessed: 99
  Too big!
  Please input your guess.
  foo
  Please input your guess.
  61
  You guessed: 61
  You win!
  ```
    * You may use the following method to get a random value of type `IO Int`
      ```
      import qualified System.Random

      getRandom :: IO Int
      getRandom = do
        x <- randomIO :: Int
        return (x `mod` 100 + 1)
      ```
    * You may want to use `read :: Read a => String -> a` to convert the read string to an integer.
