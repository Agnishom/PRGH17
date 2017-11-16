{-
  Zubin's printer when asked to print n copies of something prints n^2 copies of it. However, Zubin, being smart, can always print any number of copies he want by using his printer at most 4 times. For example, if he did want 28 copies of something, he does so by asking the printer to print 4 copies, 3 copies and then 2 copies. [Lagrange's Four Square Theorem](https://en.wikipedia.org/wiki/Lagrange's_four-square_theorem) guarantees that Zubin can always do this.

  Your task is to write a function fourSquare such that the following holds:

  * `fourSquare n` returns a tuple `(a, b, c, d)` of non-negative integers.
  * a^2 + b^2 + c^2 + d^2 == n
  * a >= b >= c >= d
  * If more than one such tuple exists, return the lexicographically largest one
-}

fourSquare :: Int -> (Int, Int, Int, Int)
fourSquare = undefined

type Point = (Float, Float)
type Segment = (Point, Point)

{-
  Complete the function intersection below. `intersection xs` should return a list of points such that:
    * There are no repetitions in the list
    * The list is sorted with the usual order.
    * The list contains all those and only all those points in which the given line segments intersect.
-}

intersection :: [Segment] -> [Point]
intersection = undefined

--`maxConsecutive xs k` finds the maximum possible sum of k consecutive elements in xs.


-- Please fill up the following blanks

maxConsecutive :: [Int] -> Int -> Maybe Int
maxConsecutive xs k
  | k > length xs   = ....
  | otherwise       = let b = .... in
                          case b of
                              Nothing -> Just (sum $ take k xs)
                              Just b' -> Just (max (....) b')
-- Possible Solution:

maxConsecutive :: [Int] -> Int -> Maybe Int
maxConsecutive xs k
  | k > length xs   = Nothing
  | otherwise       = let b = (maxConsecutive (tail xs) k) in
                          case b of
                              Nothing -> Just (sum $ take k xs)
                              Just b' -> Just (max (sum $ take k xs) b')


{-
  A substring s[i..j] of a string s[1..n] is a *word* if
  * whenever either of s[(i-1)] or s[(j+1)] exists, s[(i-1)] == ' '  and s[(j+1)] == ' '
  * No character s[x] in s[i..j] satisfy s[x] == ' '

  We intend to find the words in a given string. A proper implementation of this can be found as `words` in Prelude
-}

-- Find an input for which the following does not yield the correct answer

wrongWords :: String -> [String]
wrongWords xs = go (' ':xs) "" []
  where
    go "" buffer wordlist           = tail $ reverse ((reverse buffer):wordlist)
    go (' ':xs) buffer wordlist     = go xs "" ((reverse buffer):wordlist)
    go (x:xs) buffer wordlist       = go xs (x:buffer) wordlist

-- Answer: "ABC DEF  GHI"
