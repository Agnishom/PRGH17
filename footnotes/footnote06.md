* As a continuation of what I mentioned about code readabilitiy last time, the opposite end of that would be [obfuscation](https://en.wikipedia.org/wiki/Obfuscation_(software))
  * There have been [contests](https://wiki.haskell.org/Obfuscation) involving obfuscating Haskell code.
  * Another related coding exercise is the [Code Golf](https://codegolf.stackexchange.com/questions/tagged/haskell), writing programs with as few characters as possible.

* You have seen the `type` keyword in Haskell already. And you probably have been told that `type String = [Char]`
  * You should know that `['a', 'b'] == "ab"` and `['a', ',', 'b'] == "a,b"`, and that `"a,b" /= "ab"`. `""` are just syntactic sugar used for list of `Char`s. So, `"a"` is not the same as `'a'` in Haskell.
    * This might be trivial to many of you, but it is not something you'd might know if you are a beginner programmer.
    * Also, `'c'` and `c` are not the same thing. The former is a value of type `Char` and the later is probably a name of some function or value.
  * Suppose `f :: Int -> [Char]` and `g :: String -> Bool`. Then, is the expression `g (f 1)` actually well typed?

* An useful function `map` takes a function `f` and a list `xs`, and applies `f` to every element of `xs`.
  * For example, `map abs [-1,-2,5,3] == [1, 2, 5, 3]`
  * Implement `map`. (Or just wait until the next few lectures!)

* You now already know about lists and lists of lists. Here is an interesting idea called the [Von Neumann Construction of Natural Numbers](https://en.wikipedia.org/wiki/Natural_number#Von_Neumann_construction)
  * Create a function `f` such that `f 0 = []`, `f 1 = [[]]`, `f 2 = [[], [[]]]` and so on.
  Basically, I mean this:
  ```haskell
  f 0 = []
  f n = map f [0..n-1]
  ```
  * Does this program work? Why not? Is it possible to fix it?

* Exhibit a non-empty list of functions, `[Int -> Int]`. 
  * Implement some function `f :: Int -> [Int -> Int]` of your choice that gives you a list of such functions of the length of the given integer. For example, `f 3` should contain three functions of the type `Int -> Int`

* Now, say you have a list `xs :: [Int -> Int]` and an `x :: Int`, implement a function `f :: Int -> [Int -> Int] -> [Int]`, that will apply each function to the supplied integer.
  * For example, let's say `xs = [g1, g2, g3]` then `f x xs = [(g1 x), (g2 x), (g3 x)]`
