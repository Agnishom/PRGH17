* When using polymorphic types like `a -> a`, what we really mean is `forall a . a -> a`. That means your definition cannot distinguish between what type it was passed in its implementation. This is called *Parametricity*.
  * So, how many functions of the type `f :: a -> a` can you have?
    * Just one! The function `id`. This is because given an object of type `a`, there is no way to construct another object of type `a` other than to replicate it, because we have no way of knowing what other functions work on this type.
    * These are called [Free Theorems](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/).
  * Can you have a function of the type `a -> b`?
  * What can you say about functions of the type `a -> [a]`?
  * [Can there be function `f :: ??` such that `f g == True` if and only if `g` is a function?](https://stackoverflow.com/questions/10258488/determine-whether-a-value-is-a-function-in-haskell)
  * We can actually bypass parametrcity using [typeclasses](http://learnyouahaskell.com/types-and-typeclasses#typeclasses-101). This will be talked about in a later lecture.

* What you are seeing here is just first order polymorphism. You could actually ask to have types that will only take in polymorphic values as input.
  * You could enable this feature by enabling the language extension `{-# LANGUAGE RankNTypes #-}`.
  * ~~Consider the following type `[a -> a]`. What this really means is `forall a . [a]`. So, `[(+1), (+2), (+ 3)]`, `[('a':), ('b':), ('c':)]`, etc all could be a list of this type.~~
    * ~~However, consider the following type: `[forall a . a -> a]`. This type is a promise that anything inside the list must be a _polymorphic_ function itself. So, the only values of this type are `[id]`, `[id, id]`, `[]`, etc.~~
  * Consider the type `type T = forall a. (a -> a) -> Int` and the following function `f :: T`, `f g = 7`. Then `f (+1)`, `f id`, `f not`, etc are well typed. (`f (:)` is not, though.)
    * However, consider the type `type U = (forall a . a -> a) -> Int` with the same definition, `u :: U`, `u v = 7`. Then, only `u id` is well typed;  `u (+1)`, `u not` are not.
   

* Functional Programming has an intimate connection with constructive mathematics ([intuitionistic logic](https://en.wikipedia.org/wiki/Intuitionistic_logic)). This phenomenon is called the [Curry Howard Isomorphism](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)
  * The Curry Howard Isomorphism tells us that types are analogous to propositions and programs are analogous to proofs. A proposition is true if and only if there is a value inhabiting the corresponding type.
    * For example, you cannot hope to prove that `forall a b . a -> b`. This is why there is no way to implement a function of this type.
    * The following are axioms in certain proof systems. See if you can provide a function of the following types:
      * `a -> (b -> a)`
      * `(a -> (b -> c)) -> ((a -> b) -> (a -> c)) `
  * Type theory in general is an important tool for logic. Many interactive theorem provers use type theory to prove theorems.
    * A notable example would be [Coq](https://en.wikipedia.org/wiki/Coq).
  * Prof Suresh often offers a course called [Proofs and Types](http://www.cmi.ac.in/~spsuresh/teaching/pat16/)

* There is more to the Haskell typesystem. In fact, it is so powerful that at its full power, it is Turing Complete: what this means is that one could actually write programs at the type-level, instead of the value level.
  * We'll talk more about types in subsequent lectures and footnotes.
  * Read the [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell/Polymorphism) to find out stuff about Polymorphism and RankNTypes
