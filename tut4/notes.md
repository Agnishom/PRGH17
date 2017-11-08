## Pureness

* Functions are abstract rules that transform data from one type to the other.
  * Computation is unwinding definitions and rewriting.
  * Programs are compositions of functions.
  * Functions always map the same input to the same output

* What do we not have?
  * Side effects
    * State
    * IO
    * Randomized Computations
    * Time Dependent Programs
  * No canonical mechanism for specifying order of evaluation.
    * Functions will produce the same value anyway.
  * Functions are just rules, they do not change the world.
    * But real world programs _need_ side effects

## IO Actions

* Will box all such IO things within `IO` "wrappers", and call them actions.
  * We'll call these "wrappers" _monads_, which is something we'll explain soon.

* For example, `getLine :: IO String` is an action that is like a "recipe" to produce a string.
  * Just like we compose functions, we'll look at how to compose such "recipes" to form new recipes.
  * The program is just such an action, which is carried out when the program is executed.

* Think of `IO a` as some transformation on the real world that extracts an `a` out of it. That is, think of them as `RealWorld -> (RealWorld, a)`.

## A Mandatory [Hello World](https://en.wikipedia.org/wiki/%22Hello,_World!%22_program) program

```
main :: IO ()
main = putStrLn "Hello World!"
```

* The unit type `()` denotes that the value wrapped inside this IO Monad is not so interesting.

## But how do we use the values from IO Actions?

* Consider `getLine :: IO String`. `getLine`, when executed, takes a value from `STDIN` and wraps it in an `IO`. Also, consider `putStrLn :: String -> IO ()`, which given a string, produces an action, which when executed prints the string to `STDOUT` (and wraps nothing interesting back in).
  * How do we combine these two things? That is, how do we make an action that, when executed, gets something from `STDIN`, and prints it to `STDOUT`?
  * Plan: Use `getLine` to get a `String` wrapped in an `IO`. Unwrap it, and pass it to `putStrLn`.

* We do not want to unwrap `IO String` to get a `String`. There are two ways to think about this:
  * `IO String` has a string wrapped inside. But the wrapped `String` is impure, we do not want it to spread around the impurity!
  * `IO String` is just a recipe to produce a string. Given the recipe, you cannot produce a string (you do not execute anything, you only transform things!), you can only produce other recipes that does something using this recipe.

* `(>>=) :: IO a -> (a -> IO b) -> IO b` is exactly what we want.
  * `>>=` takes a wrapped value of type `a`, and a function that produces a value of type `b` wrapped given an `a`, and extracts the `a` and passes it to this function.
  * What we wanted is simple! ` getLine >>= putStrLn`

* `(>>) :: IO a -> IO b -> IO b` takes two actions, and produces an action which when executed, does both of them one after the other, and returns the result of the second one.
  * For example, `putStrLn "Enter your name" >> getLine` is an action of type `IO String`. It asks you for your name, and wraps it inside an `IO`.

* `return :: a -> IO a`, just takes an `a` and wraps it.

## Physical Analogies

### The Burrito Analogy

```
data Vegetables = Carrots | Beans

data Burrito filling = Burrito filling

instance Monad Burrito where
  return :: filling -> Burrito filling
  return f = Burrito f

  (>>=) :: Burrito fillingA -> (fillingA -> Burrito fillingB) -> Burrito fillingB
  (Burrito f) >>= chef = chef f
```

![](http://chrisdone.com/images/comics/monads_are_burritos.png)

### The Bucket Brigade Analogy

> Monads are bucket brigades:
>
> 1. Each operation is a person standing in line; i.e. there's an unambiguous sequence in which the operations take place.
> 2. Each person takes one bucket as input, takes stuff out of it, and puts new stuff in the bucket. The bucket, in turn, is passed down to the next person in the brigade (through the bind, or `>>=`, operation).
> 3. The return operation is simply the operation of putting stuff in the bucket.
> 4. In the case of sequence (``>>``) operations, the contents of the bucket are dumped before they're passed to the next person. The next person doesn't care what was in the bucket, they're just waiting to receive it.
> 5. In the case of monads on ``()``, a ticket is being passed around inside the bucket. It's called "the Unit", and it's just a blank sheet of paper.
> 6. In the case of IO monads, each person says something aloud that's either utterly profound or utterly stupid – but they can only speak when they're holding the bucket.

## Do Notation

```
main :: IO ()
main = putStrLn "Please enter your name:"
      >> getLine
      >>= \name ->
        putStrLn ("Hello, " ++ name ++ ", how are you?")

```

* Sometimes just using `(>>=)` and `return` may obfuscate the code. So, we have some syntactic sugar.

```
main :: IO ()
main = do
        putStrLn "Please enter your name:"
        name <- getLine
        putStrLn ("Hello, " ++ name ++ ", how are you?")
```

* The value wrapped around by the do block is the value that is wrapped around by the last action in it.

* Let's define an action which when executed asks for your name and returns it capitalized, wrapped in an IO

```
import Data.Char

getNameCapitalized :: IO String
getNameCapitalized = do
                      putStrLn "Please enter your name:"
                      name <- getLine
                      return (map toUpper name)
```

* Let's define an action which when executed asks for your name and checks if you are `"Kishlaya"`, if so, then it says, `"Hello, Kishlaya"`, otherwise, it says, `"I do not know you."`.
  * How do we do that? Of course, we can have a function that takes a string, and conditionally returns an action.

```
checkForKishlaya :: String -> IO ()
checkForKishlaya name = if name == "Kishlaya"
                        then putStrLn "Hello, Kishlaya"
                        else putStrLn "I do not know you."

main :: IO ()
main = do
          putStrLn "Please enter your name:"
          name <- getLine
          checkForKishlaya name
```

* But we could put the whole thing in the do block already.

```
main :: IO ()
main = do
          putStrLn "Please enter your name:"
          name <- getLine
          if name == Kishlaya
          then putStrLn "Hello, Kishlaya"
          else putStrLn "I do not know you"
```

* Suppose we wanted to silently quit if it was not Kishlaya. What do we do?
  * Well, nothing really. But we still need to give a legit `IO` action. Well, we just produce `IO ()`.

```
main :: IO ()
main = do
          putStrLn "Please enter your name:"
          name <- getLine
          if name == Kishlaya
          then putStrLn "Hello, Kishlaya"
          else return () -- silently quit
```

* We allow `let` statements inside `do` blocks like this:

```
main =
 do name <- getLine
    let loudName = makeLoud name
    putStrLn ("Hello " ++ loudName ++ "!")
    putStrLn ("Oh boy! Am I excited to meet you, " ++ loudName)
```


## Some Pitfalls

* If you have made it so far into the note, great job! That is all there is to IO.
  * Let's look at some pitfalls now.

* Why doesn't this work?
  ```
  main =
   do putStrLn "What is your name? "
      putStrLn ("Hello " ++ getLine)
  ```
  * Because you need to unwrap the `String` from the `IO String` first!

* Does this work?
  ```
  main =
   do putStrLn getLine
  ```

* Why doesn't this work?
  ```
  import Data.Char (toUpper)

  main =
   do name <- getLine
      loudName <- (map toUpper) name
      putStrLn ("Hello " ++ loudName ++ "!")
      putStrLn ("Oh boy! Am I excited to meet you, " ++ loudName)
  ```
  * Because `(map toUpper) name` is not wrapped in an IO, so cannot really extract using `<-`.
  * This works: `loudName <- return $ (map toUpper) name`. Recall that `return` wraps things
  * But better to just use: `let loudName = (map toUpper) name`. You can have such `let` clauses in `do` blocks
  * The following also works, but is not really recommended (aesthetic reasons):
    ```
    do name <- getLine
       let loudName = makeLoud name
       in  do putStrLn ("Hello " ++ loudName ++ "!")
              putStrLn (
                  "Oh boy! Am I excited to meet you, "
                      ++ loudName)
    ```

* What is wrong with this?
  ```
  main = do
    x <- putStrLn "Please enter your name:"
    name <- getLine
    y <- putStrLn ("Hello, " ++ name ++ ", how are you?")
  ```
    *  The last statement in a `do` construct must be an expression, you must produce an action.

## References

### From where I (shamelessly) copied stuff

* [Monads are Burritos](http://chrisdone.com/images/comics/monads_are_burritos.png)
* [What are monads in non-programming terms?](https://stackoverflow.com/questions/3261729/monad-in-non-programming-terms)
* [Simple Input and Output](https://en.wikibooks.org/wiki/Haskell/Simple_input_and_output)

### Other helpful resources

* [A Gentle Introduction to Haskell: IO](https://www.haskell.org/tutorial/io.html)
* [Input and Output - Learn You a Haskell](http://learnyouahaskell.com/input-and-output)
