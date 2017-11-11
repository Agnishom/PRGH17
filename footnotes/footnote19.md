* Recall the Functor laws.
    1. `fmap id = id`
    2. `fmap (g . f) = fmap g . fmap f`
  * Verify that they hold good for the list functor.
  * Verify that they hold good for the `Maybe` functor.
  * Define an instance of `Functor` for a Tree type. Verify that the functor laws hold for your definition of `fmap` on trees.

* Consider the following type `newtype FromInt a = FromInt (Int -> a)`. We can make this an instance of `Functor` as follows:
  ```
  instance Functor FromInt where
    fmap :: (a -> b) -> FromInt a -> FromInt b
    fmap h (FromInt f) = FromInt (h . f)
  ```
  * Verify that the functor laws hold good for the above `fmap`.

* In light of this example, also consider this type `newtype MakeInt b = MakeInt (a -> Int)`.
  * One cannot make `MakeInt` an instance of `Functor` abiding the functor laws.
    * I've not seen a formal proof of this yet. However, [School of Haskell] seems to have a nice article on this.
  * However, it can be made an instance of something that is known as `Contravariant`. Contravariant Functors are defined in `Data.Functor.Contravariant` as follows:
    ```
    class Contravariant f where
      contramap :: (a -> b) -> f b -> f a
    ```
    * `Contravariant`s are required to satisfy these laws:
      1. `contramap id = id`
      2. `contramap f . contramap g = contramap (g . f)`
    * So, we can define `MakeInt` an instance of `Contravariant` as follows:
      ```
      instance Contravariant MakeInt where
        contramap h (MakeInt f) = MakeInt (f . h)
      ```

* Here is a more familiar instance of the above examples in a setting that might be more familiar to you. This was inspired by Dhruv.
  * Suppose `W` is a fixed vector space. Given a vector space `V`, denote by `Hom(V, W)` denote all the vector homomorphisms, i.e, linear maps from `V` to `W`.
  * Here, `Hom(_, W)` is a functor that sends an object(think value in Haskell) in the category(think type) of vector spaces to an object in the category of sets, namely, the set of linear maps to `W`.
  * But why do we call this a functor? Because given a map `T :: U -> V` from a vector space `U` to `V`, the Hom functor transforms it into a map `T' :: Hom(V, W) -> Hom(U, W)` as follows: Given a linear map from `t` in `Hom(V, W)`, `T'` transforms it to a linear map in `Hom(U, w)` by sending `t` to `t . T`.
    * You can check that this functor respects the functor laws, i.e, it respects compositions.
  * This example might sound confusing in english. But this is exactly the same as the code above, which is about functors in the category Hask (the category of Haskell types), rather than that of Linear spaces.

* Given the free theorem (see [footnote 8](https://github.com/Agnishom/PRGH17/blob/5e00616b4aa069bfd4984e147be95b835b4bd354/footnotes/footnote08.md)) for the type of `fmap`, one can show that the second functor law holds whenever the first law does. This means that parametricity enforces the second law whenever the first one holds.
  * Check out [this](https://github.com/quchen/articles/blob/master/second_functor_law.md) article for an outline of the proof.
