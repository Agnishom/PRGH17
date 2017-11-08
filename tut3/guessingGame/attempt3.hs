import System.Random

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
          putStrLn "Too large!" >> go -- removed do and simplified code
      else
          putStrLn "Too small!" >> go
