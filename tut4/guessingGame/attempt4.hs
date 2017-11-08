import System.Random

main :: IO ()
main = do
  putStrLn "I am guessing a value between 1 to 100"
  guess <- (randomIO :: IO Int)
  playWith (guess `mod` 100)
  putStrLn "Play again? (Y/N)" -- playing multiple times
  response <- getLine
  if response == "Y" then
    main
  else
    return ()

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
          putStrLn "Too large!" >> go
      else
          putStrLn "Too small!" >> go
