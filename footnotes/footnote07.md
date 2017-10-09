* Lazy Evaluation is one of the most interesting things about Haskell. And it is useful too. Here are a few definitions involving laziness. Figure out how and why they work:
  *
  ```haskell
  cycle :: [a] -> [a]
  cycle xs                = xs' where xs' = xs ++ xs'
  ```
  *
  ```haskell
  primes = filterPrime [2..] 
    where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
  ```

* You can use the `:sprint` command in GHCi to view thunks, i.e, to view how much of an expression has been evaluated and how much is still just symbolic.
  *
  ```
  Prelude> let xs = zip [1..10] ['a'..'z'] :: [(Int, Char)]
  Prelude> :sprint xs
  xs = _
  Prelude> take 4 xs
  [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
  Prelude> :sprint xs
  xs = (1,'a') : (2,'b') : (3,'c') : (4,'d') : _
  Prelude> length xs
  10
  Prelude> :sprint xs
  xs = [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),
        (8,'h'),(9,'i'),(10,'j')]
  ```

* Although laziness is fundamental to Haskell's philosophy, one might require eagerness for a real world application. (You'll be given some examples in later lectures.) Here are some ways to break laziness in Haskell
  * The function `seq :: a -> b -> b` takes two arguments and returns the last argument, but evaluates the first argument to [Weak Head Normal Form](https://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form)
  ```
  > let xs = map (+1) [1..10] :: [Int]

  > :sprint xs
  xs = _

  > seq xs ()
  ()

  > :sprint xs
  _ : _

  > length xs
  10

  > :sprint xs
  [_,_,_,_,_,_,_,_,_,_]
  ```
    * Notice that this is sort of strange, because the function `seq` has a side effect. What that means is that this function cannot be fully understood in terms of what outputs it returns given the inputs.
  * One can use bang patterns to indicate that a particular argument of a function needs to be evaluated to WHNF whenever it appears in an expression.
  ```haskell
  {-# LANGUAGE BangPatterns #-}

  sum :: Num a => [a] -> a
  sum xs = go xs 0
    where go []     !acc = acc
          go (x:xs) !acc = go xs (acc + x)
  ```
    * Notice that we did something by `{-# LANGUAGE BangPatterns #-}`. This is enabling a [GHCi extension](https://wiki.haskell.org/Language_Pragmas)
      * You could also do this by using the flag `-XBangPatterns` while calling ghci on your shell.
      
* A certain value called `undefined` is present in Haskell. It is often written as `⊥` in literature. `undefined` is supposed to be capturing the idea of computations that do not result in a valid value, but end up in an error or a computation that does not halt.
  * This value is strange because it belongs to every type. This often breaks nice properties of type theory.
    * However, in a certain precise sense, it is okay to assume that there is no value that does such a thing. This is often referred to as [Loose and Fast reasoning is Morally Correct](https://pdfs.semanticscholar.org/a316/3d9097a87a713f0dd3f154f139e19dcb2a82.pdf)
  * [Denotational Semantics](https://en.wikibooks.org/wiki/Haskell/Denotational_semantics) is the formal theory of what a Haskell program (or some other program) is supposed to mean. You could find out more about this in the linked wiki page.
  * A function `f` is strict in it's argument if `f undefined = undefined`.
    * Notice that this need not be the case for any arbitrary function, thanks to lazy evaluation.
      * Say `one :: a -> Int` and `one x = 1`. Then, `one (1::Int/0)` still evaluates to `1`, even though `(1::Int/0)` is undefined.
      
* If this topic interests you, here are some resources that you could look at.
  * Attend [Implementation of Functional Programming Languages](http://www.cmi.ac.in/~madhavan/courses/fpl2016/), offered in even semesters.
  * [Implementation of Functional Programming Languages](https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/) by Simon Peyton Jones
    * A more practical [approach](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/?from=http%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fpj-lester-book%2F)