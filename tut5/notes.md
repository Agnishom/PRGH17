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

## References

1. [How important are the abstract concepts?](https://softwareengineering.stackexchange.com/questions/95966/how-important-are-haskells-advanced-concepts-like-monads-and-applicative-functo)
2. [Haskell Wikibook/Monoids](https://en.wikibooks.org/wiki/Haskell/Monoids)
