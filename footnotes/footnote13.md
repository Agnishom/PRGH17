* Looking for (Real World) Haskell Projects to work on?
  * Check out the [Haskell Bounty Program](http://www.vacationlabs.com/haskell-bounty-program/)! They have projects that will take you 4-5 weeks long and will pay you upon successful completion.
  * Check out [Simula](https://github.com/SimulaVR/Simula), a 3D window manager. According to [this reddit post](https://www.reddit.com/r/haskell/comments/76esri/looking_for_haskell_projects_to_work_on_in_spare/dodpd2i/), they are willing to sponsor a free HTC Vive _if it helps you get your feet wet_.

* Do `data Solution = Solution [Int]` and `type Solution = [Int]` have the same behavior? If not, what is the difference? Is there any circumstance when you'd prefer the former style over the later?
  * `data Solution = [Int]` is not even correct syntax!

* An alternative to the `data` keyword is the `newtype` keyword. However, we are allowed to use `newtype` only when there is exactly one constructor and one field. For example, `newtype Polynomial = Poly [Int]` is semantically the same as `data Polynomial = Poly [Int]`, as we used in [Tutorial 2](https://agnishom.github.io/PRGH17/tut2/index.html). However, something like `newtype N = N Int Int`, `newtype N = N` or `newtype N = M | N` are not valid syntax.
  * But what is the point of the `newtype` keyword when we already have `data` which is more general?
    * When a new type with possibly multiple fields are constructed, the data is structured through a tree of pointers. For example, for the type `data Book = Book (Int, Int)`, the internal representation in memory is somewhat like this: ![https://i.stack.imgur.com/FwKcy.png](https://i.stack.imgur.com/FwKcy.png). So, two actually access the `Int` inside, the runtime has to go through the series of pointers to find out where the `Int` is.
      * If there is just one constructor, the chain of pointers is not really necessary. By pulling the value up the tree and removing the extra pointer, `newtype` avoids the extra overhead. So, one way to think about this is that when you say `data Book = Book Int`, the _tag_ `Book` remains even during the runtime, however, when you say `newtype Book = Book Int`, the tag is removed during the runtime, and only helps the programmer make sure that his program is correctly typed.
      * Another way to remove these _indirections_ is to use the `{-# UNPACK #-}` pragma.
        * [GHC User Guide on the Unpack Pragma](https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/pragmas.html)
        * [A helpful StackOverflow answer](https://stackoverflow.com/questions/33931991/what-does-the-unpack-pragma-do-in-this-case)
    * As a result of removing a layer of boxing, the value constructor produced by `newtype` is strict, as opposed to the value constructor produced by `data`, which is lazy. (For a discussion of what strictness is, see [footnote07](https://github.com/Agnishom/PRGH17/blob/master/footnotes/footnote07.md)) This [answer](https://stackoverflow.com/a/12064372/1955231) explains this with an example.
      * You could also make the constructor strict by using bang patterns. For example, `data P a b = P !a !b`. (For a discussion on bang patterns, see [footnote07](https://github.com/Agnishom/PRGH17/blob/master/footnotes/footnote07.md))

* In [Tutorial 2](https://agnishom.github.io/PRGH17/tut2/index.html), we created a polynomial type and instantiated it as a part of several type classes. Try doing something similar for a type called `Complex`, representing complex numbers.
  * Define `data Complex a = a :+ a`. Instead of using `:+`, you could use anything that begins with a `:` as an infix operator.
    * See what the [GHC user guide](https://downloads.haskell.org/~ghc/7.2.1/docs/html/users_guide/data-type-extensions.html#infix-tycons) says on this.
  * Make `Complex a` an instance of `Show` where the complex numbers are shown in the familiar `a + bi` form.
  * Make `Complex a` an instance of `Num` defining the required arithmetic operations.
  * Does `deriving Eq` give you a sufficiently satisfactory behavior on this type?
  * What other typeclasses do you think you need to make your type a part of?
    * Alternatively, if you are too bored of this exercise, or want to see the usual implementations, see [Data.Complex](https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Complex.html)
