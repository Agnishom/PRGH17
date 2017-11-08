--the random number generator
import System.Random

-- the main action
main :: IO ()
main = do
  putStrLn "I am guessing a value between 1 to 100"
  guess <- (randomIO :: IO Int)
  playWith (guess `mod` 100)

playWith :: Int -> IO ()
playWith n = go
  where
  go = do
    putStr "Enter your guess: "
    guess <- readLn :: IO Int
    if n == guess then
      putStrLn "You win!"
    else
      if n < guess then
        do
          putStrLn "Too large!"
          go
      else
        do
          putStrLn "Too small!"
          go
