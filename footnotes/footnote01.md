- We viewed computation just as the process of rewriting. Is this notion powerful enough? Can we use such a model to compute whatever we like?
  - It turns out that we can. Functional programming is based on an abstraction called the [Lambda Calculus](https://brilliant.org/wiki/lambda-calculus/)
  - To have a sense of the multitude of the strange systems that are really just as powerful as traditional computers, see [Esoteric Languages](https://en.wikipedia.org/wiki/Esoteric_programming_language). BrainF and FRACTRAN are particularly interesting. [Rule 110](https://en.wikipedia.org/wiki/Rule_110) could be an honorable mention
  - Having said that, Haskell could be a pleasant and practical language to program in.

- In class, we defined `plus` in terms of `succ`.
  - Can you define `mult` standing for multiplication, in terms of `plus`
    - Install the Haskell interpreter on your device and check if your program works. Alternately, use the [repl.it](https://repl.it/languages/haskell) environment online.
  - Can you prove that `plus n m == plus m n` using the definitions of `plus`? If not, what other assumptions do you need?
    - Can you prove a similar thing for `mult`?
    - Can you prove that `plus a (plus b c) = plus (plus a b) c`?
    