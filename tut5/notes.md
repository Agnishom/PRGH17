## What is the point?

* Are any of these necessary to get code running? No!
  * Are these necessary to get write elegant and beautiful idiomatic Haskell? Absolutely.

* Think of them as Design Patterns.
  * What really matters in the end, is being able to use them in programs.

## Monoids

### Some Motivating Examples

* Suppose I have a list of strings, and I want to `concat`, them. How do I express that using a fold?
  * `foldr (++) strings ""`
* I have a bunch of pictures, I want to put them on top of each other. How do I do that with the [Diagram](https://archives.haskell.org/projects.haskell.org/diagrams/) library?
  * ```
    mconcat  (fmap(circle . (/20)) [1..5])
    <> triangle (sqrt 3 / 2) # lwL 0.01 # fc yellow
    <> circle 0.5 # lwL 0.02 # fc deepskyblue
    ```
  * ![](https://upload.wikimedia.org/wikipedia/commons/8/87/Monoids_diagrams_demo.svg)

### The Pattern

* We have an empty diagram, the diagram of nothing. We have an empty string.
* We have a way to compose together diagrams, by drawing them one after the other. We have a way to compose lists, by concatenating lists.
* Composition is associative, but not necessarily commutative. For example, given three lists, we might concatenate them in two ways:
  * `[1,2,3] ++ [4,5,6] ++ [7,8,9] == [1,2,3,4,5,6] ++ [7,8,9] == [1..9]`
  * `[1,2,3] ++ [4,5,6] ++ [7,8,9] == [1,2,3] ++ [4,5,6,7,8,9] == [1..9]`
  * But changing the order of the lists changes things.

### The Monoid Laws

* Here are the signatures:

```
class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty
````

* We'll call `mappend` as `(<>)` as an infix operator, for simplicity of notation.
  * Available from `Data.Monoid`

* And, these are the laws:

```
(x <> y) <> z = x <> (y <> z) -- associativity
mempty <> x = x               -- left identity
x <> mempty = x               -- right identity
```

* But what are laws, again?
  * Laws are really just definitions. When we call a type a monoid, what we really mean by that is that it has a `<>` and a `mempty` that follow the laws.
  * Therefore, it is the duty of the programmer to prove the Monoid laws when he writes a monoid instance.

### Some More Examples of Monoids

* `Bool`, with `True` as `mempty` and `&&` as `mappend`.
  * What other ways are there to write Monoid instances of `Bool`?
* How can you form a Monoid out of Integers?
* Functions of type `a -> a` (called Endomorphisms) form a monoid. Can you guess what the `mappend` and `mempty` are?

### Again, why is this useful?

* Elegant pattern that you can see everywhere.
* You can do `mconcat` whenever there is a list of values which belong to teh same Monoid.
  * Can have genearlized `mconcats`, which is`fold :: Monoid m => t m -> m`. For example, you maybe able to fold a tree up, somehow.
* `f` is a monoid homomorphism if it respects `(<>)`. That is, `f (x <> y) = f x <> f y`. Recognizing a homomorphism might help you refactor code nicely.
  * For example, say that rotating a diagram `rotate` is expensive. Then rather than using `rotate diagram1 <> rotate diagram2`, it might be a better idea to do `rotate (diagram1 <> diagram2)` instead, if you knew that `rotate` is a homomorphism.

## Functors

### The Motivation

* The following is valid: `map (+1) [1,2,3]`
* It goes inside the list, and does (+1) to every element.
* Wouldn't it be nice if we could write, in the same way, say, `fmap (+1) (Just 2)`?
* Or something like that for Sets, Trees, and more?

### The Pattern and The Functor Laws

![](https://ds055uzetaobb.cloudfront.net/image_optimizer/b56c76f586b29ff2a688bc7b0bb6ec419cf7b95e.jpg)

* Functors are abstractions of `map`s.

```
fmap id = id
fmap (g . f) = fmap g . fmap f
```

We shall use the infix shorthand `(<$>)` for `fmap`.

### IO is a Functor too

```
import Data.Char

capitalize = map toUpper
getLoudLine = capitalize <$> getLine
```

## Monads

### IO

We have seen a lot of IO [last time](https://github.com/Agnishom/PRGH17/tree/master/tut4)

### Maybe

Chains of Computations. You have seen this in class.

Look at [Footnote 14](https://github.com/Agnishom/PRGH17/blob/master/footnotes/footnote14.md) for a discussion on this.

### List

* The List monad somehow corresponds to non-deterministic computations.
* This is an example from *Learn You a Haskell*. Suppose I want to find out all the squares in a chessboard which the knight could be at after three moves.


```
type KnightPos = (Int,Int)  

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]  

in3 :: KnightPos -> [KnightPos]
in3 start =  moveKnight `concatMap` (moveKnight `concatMap` (moveKnight `concatMap` [start]) )
```

* In the list Monad, `return x = [x]` and `(>>=) = flip concatMap`.
* Let's rewrite this in Monadic style.

```
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
```

* In fact, we can write `moveKnight` as a non-deterministic computation using `do` notation.

```
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    if (c' `elem` [1..8] && r' `elem` [1..8])  
    then return (c',r')  
    else [] --ignoring this c

in3 :: KnightPos -> [KnightPos]  
in3 start = do   
    first <- moveKnight start  
    second <- moveKnight first  
    moveKnight second  
```

### State

#### Motivating Examples

##### Stack

```
type Stack = [Int]  

pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  

push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)  
```

* But how do you do a bunch of operations to a stack in a nice way? Say, you want to push `3` and pop the last two items.

```
stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2  
```  

* This is not elegant. What if we wanted to do 10 operations?

##### Pseudorandom Number Generator (PRNG)

* How does a computer generate Random Numbers?
  * Well, it cannot, until it has the ability to get some randomness from some source.
  * However, it can generate "random looking" numbers, i.e, given a sequence of numbers that it generates, it'd be "hard" to predict what the next number is.

* Start with some seed value. Everytime you want a new number, you update it.

* A very simple example is the Linear Congruential Generator.
  * Fix parameters `m`, `c`, `modulus`.
  * Initialize the "state" of the generator with `seed`.
  * When asked for a number, output the current state, and update state to ``(state * m + c) `mod` modulus``.

```
data RandGen = RandGen {curState :: Int, multiplier :: Int, increment :: Int, modulus :: Int}

runRandom :: RandGen -> (Int, RandGen)
runRandom gen1 = (v, gen2)
  where
    v = curState gen1
    v2 = (v * (multiplier gen1) + (increment gen1)) `mod` (modulus gen1)
    gen2 = gen1 { curState = v2}

threeRandoms :: RandGen -> (Int, Int, Int)
threeRandoms gen =
  let (r1, g2) = runRandom gen
      (r2, g3) = runRandom g2
      (r3, _ ) = runRandom g3
  in (r1, r2, r3)

```

* Again, this is not elegant.

#### The Pattern

* Think of a stateful computation as a function that takes a `state`, and chages extracts a `value` of it, and changes the `state`.

![](https://ds055uzetaobb.cloudfront.net/uploads/7D5xawP5qN-stateful-computation.png)

```
newtype State s v = State { runState :: s -> (v,s) }  
```

* Notice the resemblance between `runState :: s -> (v,s)`, `runRandom :: RandGen -> (Int, RandGen)`, `pop :: Stack -> (Int,Stack)`.
  * We'll have `State Stack Int`, which means, we'll use the `Stack`s for maintaining the states, and our values will be `Int`s.
  * We'll have `State RandGen Int`, which means we'll use the `RandGen`s to keep track of the Generators, and will get `Int`s out of them.
* Guess what? We'll form a `Monad` out of `State s`.
  * `return val` should give you a stateful computation that does not alter the state at all, and just produces the `val`
  * `(>>=) :: State s a -> (a -> State s b) -> State s b`, tries to chain together two stateful computations.

```
instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  -- run the first stateful computation, and call the result a
                                        (State g) = f a -- now, use this result to create a new stateful computation
                                    in  g newState  
```                               

#### Back To Examples

##### Stack

* Instead of  writing `Stack -> (a, Stack)`, we'll write `State Stack a`

```
pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  

push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs)
```     

* Now, `stackManip` is really just a stateful computation

```

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    pop  
    pop  
```

```
ghci> runState stackManip [5,8,2,1]  
(5,[8,2,1])
```

* Let's think of another stack manipulation, which involves some conditionals.

```
stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8  
```

```
ghci> runState stackStuff [9,0,2,1,0]  
((),[8,3,0,2,1,0])  
```

##### Pseudorandom Number Generator

* Again, we can rewrite `runRandom` with `State`.

```
runRandom :: State RandGen Int
runRandom = State $ f
  where
    f gen1 = (v, gen2)
        where
          v = curState gen1
          v2 = (v * (multiplier gen1) + (increment gen1)) `mod` (modulus gen1)
          gen2 = gen1 { curState = v2 }
```

* Then, we can rewrite `threeRandoms` as follows:

```
threeRandoms :: State RandGen (Int, Int, Int)
threeRandoms = do
    a <- runRandom
    b <- runRandom
    c <- runRandom
    return (a,b,c)
```

```
ghci> let r = RandGen 10 6152761 122 1000007
ghci> runState threeRandoms r
((10,527305,928721),RandGen {curState = 349697, multiplier = 6152761, increment = 122, modulus = 1000007})
```

* We can even have an infinite random number stream:

```
infiniteRandoms :: State RandGen [Int]
infiniteRandoms = do
  x <- runRandom
  xs <- infiniteRandoms
  return (x:xs)
```

```
ghci> take 10 $ fst $ runState infiniteRandoms r
[10,527305,928721,349697,2430,104695,803911,624734,84012,138940]
```

##### A Quick Implementation of State

Find one [here]()

### So, what are Monads, really?

* Monads are a design pattern that is frequently used in Haskell code.
* Formally, Monad is a typeclass that has the following type signature and follow the following rules:

#### Signature

```
class Monad m where
        return :: a -> m a
        (>>=)  :: m a -> (a -> m b) -> m b
```

(This is not the signature actually used. But I am using this here to avoid technicalities)

#### Monad Laws

```
m >>= return     =  m                        -- right unit
return x >>= f   =  f x                      -- left unit

(m >>= f) >>= g  =  m >>= (\x -> f x >>= g)  -- associativity
```

* With `(>=>)`, defined as follows, associativity makes more sense.

```
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g
```

```
return >=> f = f >=> return -- identity of return
(f >=> g) >=> h  =  f >=> (g >=> h) -- associativity
```

#### I want another Monad Analogy, anyway

* [Functors, Applicatives, and Monads in Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
* [Monad in Non-Programming Terms](https://stackoverflow.com/questions/3261729/monad-in-non-programming-terms)
* [Monads are like Burritos](https://blog.plover.com/prog/burritos.html)

That said, the analogies are never substitute for actual examples. And monads might be elegant, but they are not mysterious anyway. 


## References

1. [How important are the abstract concepts?](https://softwareengineering.stackexchange.com/questions/95966/how-important-are-haskells-advanced-concepts-like-monads-and-applicative-functo)
2. [Haskell Wikibook/Monoids](https://en.wikibooks.org/wiki/Haskell/Monoids)
3. [Haskell Wikibook/Functor](https://en.wikibooks.org/wiki/Haskell/The_Functor_class)
4. [Learn You a Haskell/A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads)
5. [Learn You a Haskell/For a Few Monads More](http://learnyouahaskell.com/for-a-few-monads-more#state)
