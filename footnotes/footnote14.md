* Now that you know about recursive data types, check out [Problems 54A to 60](https://wiki.haskell.org/99_questions/54A_to_60) and [Problems 61 to 69](https://wiki.haskell.org/99_questions/61_to_69) for some easy exercises.
* Recall the function `lookup :: Eq a => a -> [(a, b)] -> Maybe b` that looks up values from a list using keys. Suppose there are three such databases, `phonebook :: [(Name, PhoneNumber)]`, `governmentDatabase :: [(PhoneNumber, AadharID)]`, `taxOwed :: [(AadharID, Double)]` where, `type Name = String`, `type PhoneNumber = Integer`, `type AadharID = String`. And for some reason, you wish to lookup how much tax a person owes, given his name. Design `lookupTax :: Name -> Maybe Double` that will look through this chain of databases to get the tax a person owes, if it can be found; during this lookup, either the first lookup might fail, or the second might, or the third might, in which case return `Nothing`.
  * Use successive `lookup`s and case analyses to solve the problem above, first.
  * Now, define `bind :: Maybe a -> (a -> Maybe b) -> Maybe b` as some function that binds together two consecutive operations, in a way that if the first operation returns a `Nothing`, the combination of the second operation along with the first returns a `Nothing` as well and otherwise, it takes this value, and feeds it in to the second operation. Do you see any resemblence of this pattern with that of the lookups? Yes, when the lookups fail at any point, the subsequent lookups do not matter, we get a Nothing at the end anyway, otherwise, we use this value to lookup the next database.
    * Complete the definition of `bind` using hints from the discussion above:
    ```
    bind :: Maybe a -> (a -> Maybe b) -> Maybe b
    bind output1 safeFunction = case output1 of
                                Nothing     -> undefined -- fill this in
                                Just output -> undefined -- fill this in
    ```
  * Now that we have defined `bind`, our job is way painless. We can now define, `lookupTax` easily.
    ```
    lookupTax :: Name -> Maybe Double
    lookupTax name = lookup name phonebook
                          `bind` \phonenumber -> lookup phonenumber governmentDatabase
                                  `bind` \aadhar -> lookup aadhar taxOwed
    ```
  * Actually, this function `bind` that we defined is already defined in the prelude as `(>>=) :: Monad m => m a -> (a -> m b) -> m b`. In our example, we used this for the special case where `m` was the `Maybe` type constructor. This is a more general theme that we shall explore later.
  * Check out [this](https://codereview.stackexchange.com/questions/177823/n-queens-with-haskell) implementation of the N-Queens problem to see how the `>>=` operator is being tacitly used.

* See [here](https://stackoverflow.com/questions/6364409/why-does-haskells-head-crash-on-an-empty-list-or-why-doesnt-it-return-an) for a discussion on why the default `head` is not the "safe" `head`.
