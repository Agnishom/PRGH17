* (Moderately easy) [Knights](https://en.wikipedia.org/wiki/Knight_(chess)) are chess pieces whose moves are commmonly characterized _two and a half squares_, that is two say that they move two squares in one direction and one square in an orthogonal direction. A knight is sitting in the square (`(x,y)`) of an \(8 \times 8\) chess board (labelled with coordinates from `[(x,y) | x <- [0..7], y <- [0..7]]`): define `knightMove :: (Int, Int) -> Int -> [(Int, Int)]` such that `knightMove (x, y) n` is the list of squares where the knight could be, after a total of `n` moves, starting from `(x,y)`. Make sure that your list has no duplicates, and is lexicographically sorted.
  ```haskell
  knightMove (0,0) 1 == [(1,2),(2,1)]
  knightMove (0,0) 3 == [(0,1),(0,3),(0,5),(1,0),(1,2),(1,4),(1,6),(2,1),(2,3),(2,5),(3,0),(3,2),(3,4),(3,6),(4,1),(4,3),(4,5),(5,0),(5,2),(5,4),(6,1),(6,3)]
  knightMove (2,2) 2 == [(0,2),(0,6),(1,1),(1,3),(1,5),(2,0),(2,2),(2,4),(2,6),(3,1),(3,3),(3,5),(4,2),(4,6),(5,1),(5,3),(5,5),(6,0),(6,2),(6,4)]
  ```

* (Slightly less easy) A group of `n` children labelled `[1..n]` are playing a [counting out game](https://en.wikipedia.org/wiki/Counting-out_game), with parameter `k` to select a leader among themselves. The game proceeds as follows:
  * The children form a circle.
  * Child 1 is marked as an invalid candidate.
  * Ignoring the invalid children, the child who is k places to the right of child 1 is again marked invalid.
  * The process is repeated until all but one child is marked invalid.
  * The last remaining child is selected as the leader
  * Here are the steps of the process run with 5 children and `k=2`:
    *  1 2 3 4 5
    * ~~1~~ 2 3 4 5
    * ~~1~~ 2 ~~3~~ 4 5
    * ~~1~~ 2 ~~3~~ 4 ~~5~~
    * ~~1~~ 2 ~~3~~ ~~4~~ ~~5~~
  * Define `selectleader :: Int -> Int -> Int` such that `selectleader n k` is the label of the leader selected when the process starts with `n` children and parameter `k=2`
    ```haskell
    selectleader 100 5 == 43
    selectleader 5 2   == 2
    selectleader 10 5  == 9
    ```
