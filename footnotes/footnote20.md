* Here are a couple of resources on the web about Functors, Applicatives and Monads that I found useful:
  * Haskell Wikibook: [Applicative Functors](https://en.wikibooks.org/wiki/Haskell/Applicative_functors), [Monads](https://en.wikibooks.org/wiki/Haskell/Prologue:_IO,_an_applicative_functor), [The Functor Class](https://en.wikibooks.org/wiki/Haskell/The_Functor_class)
  * [The Monad Challenges](http://mightybyte.github.io/monad-challenges/) - These are a series of exercises that are based on the principle, "Monads cannot be explained, they can only be discovered". If you have time, and are motivated, I strongly recommend working through this.
  * [Functors, Applicatives and Monads in Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)


* Here are a few of my favourite resources on Category theory, that build up on Haskell or similar programming paradigms. In all honesty, these material is slightly off-track to programming and might not necessarily make you a better programmer.
  * [Categories and Haskell](https://github.com/jwbuurlage/category-theory-programmers) - Rather terse presentation, with a rather broad coverage.
  * [Haskell Wikibook/Category Theory](https://en.wikibooks.org/wiki/Haskell/Category_theory) - Rigorous but short introduction.
  * [Category Theory for Programmers](https://github.com/hmemcpy/milewski-ctfp-pdf) - An interesting read, a series of blog posts turned into a pdf.

* Can you write an `fmap` of the correct type that obeys the second functor law, but not the first one?

* An useful shorthand for `fmap` is `<$>`.
  * Compare using something like `(+10) $ 30` to writing `(+10) <$> [1..30]`.

* Let's recall the Applicative laws.
  1. Identity: `pure id <*> v = v`
  2. Homomorphism: `pure f <*> pure x = pure (f x)`
  3. Interchange: `u <*> pure y = pure ($ y) <*> u`
  4. Composition: `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`

    * Write applicative instances for the following type constructors and verify the laws:
        * `Maybe`
        * `Either e`, for a fixed `e`
        * `((->) r)`, for a fixed `r`

* Can you prove the following theorem using the functor and applicative laws: `f <$> x = pure f <*> x`?
  * I am not sure how to prove this. If you can, let me know.

* [Here](https://stackoverflow.com/questions/7220436/good-examples-of-not-a-functor-functor-applicative-monad) is an interesting StackOverflow thread about type constructors that are not functors, functors but not applicatives, applicatives but not monads.
