import System.Environment
import System.IO
import System.Process

main :: IO ()
main = do
   args <- getArgs
   file1 <- readFile (args !! 0)
   file2 <- readFile "testData.log"
   outputFile <- openFile "testTmp.hs" WriteMode
   hPutStrLn outputFile file1
   hPutStrLn outputFile file2
   hClose outputFile
   result <- readProcess "runhaskell" ["testTmp.hs"] ""
   putStr result
