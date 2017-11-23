* In class, we saw applications of several laws of `filter`, `map`, etc to reason about programs. This is known as equational reasoning.
  * Notice that the reason that this is possible is because of [referential transperancy](https://stackoverflow.com/questions/210835/what-is-referential-transparency), the fact that using the same name, one is always referring to the same thing. This would not have been possible in a setting with mutable variables.
  * Other than coming up with a more optimized definition, one could also use equational reasoning to prove properties of programs.
    * I found [this](http://www.haskellforall.com/2013/12/equational-reasoning.html) to be an interesting resource.
    * Another worthwhile resource is the chapter on "Proofs", in [Thinking Functionally with Haskell](https://www.amazon.in/Thinking-Functionally-Haskell-Richard-Bird/dp/1107452643)
  * There are tools called Proof Assistants, such as [Coq](https://coq.inria.fr/), which allow proving theorems about programs, and often extract correct code from the proofs.
    * A notable example is [CompCert](https://en.wikipedia.org/wiki/CompCert), a verified C compiler.
    * An worthwhile resource demonstrating the Coq Proof Assistant is [Software Foundations](https://softwarefoundations.cis.upenn.edu/current/index.html)

* A real world Haskell Program might require mutable variables and state. We saw some introductory usage of state in Tutorial 5.
  * Other ways to actually implement state are `IORef`s, `MVar`s, and `ST` Monads (which let you have a pure function, even after using states).
  * There are also [`MArrays`](https://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array-MArray.html), which are Mutable Arrays, which unlike lists can be accessed randomly.
  * Check out [this](https://en.wikibooks.org/wiki/Haskell/Mutable_objects) chapter of the Haskell Wikibook for details on Mutable Objects.
