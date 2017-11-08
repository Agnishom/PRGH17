-- the game play, given the random guess

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
