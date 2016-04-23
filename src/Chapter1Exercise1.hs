-- Chapter1Exercise1.hs
module Chapter1Exercise1 where
import System.Environment

-- Exercise 1:
-- Change the program so it reads *two* arguments from the command line, and
-- prints out a message using both of them

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0 ++ " and hello, " ++ args !! 1)
