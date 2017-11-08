import System.Random
import Text.Read -- for readMaybe

main :: IO ()
main = do
  putStrLn "I am guessing a value between 1 to 100"
  guess <- (randomIO :: IO Int)
  playWith ((guess `mod` 100) + 1)
  putStrLn "Play again? (Y/N)"
  response <- getLine
  if response == "Y" then
    main
  else
    return ()

playWith :: Int -> IO ()
playWith n = go 5
  where
  go 0 = putStrLn "You loose!"
  go m = do
    putStr "Enter your guess: "
    guess <- readUntilValid -- safe reading, does not halt program on invalid input
    if n == guess then
      putStrLn "You win!"
    else
      if n < guess then
          putStrLn "Too large!" >> go (m-1)
      else
          putStrLn "Too small!" >> go (m-1)

readUntilValid :: IO Int
readUntilValid = do
  s <- getLine
  case (readMaybe s :: Maybe Int) of
    Nothing -> putStr "Please enter a valid guess:" >> readUntilValid
    Just x -> return x
