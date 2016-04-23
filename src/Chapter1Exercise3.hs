-- Chapter1Exercise3.hs
module Chapter1Exercise3 where
import System.Environment

-- Exercise 3:
-- getLine is an IO action that reads a line from the console and returns it as
-- a string. Change the program so it prompts for a name, reads the name, and
-- prints that instead of the command line value

main :: IO ()
main = do
  putStr "Hi, what's your name? "
  name <- getLine
  putStrLn $ "Hi, " ++ name ++ "! Nice to meet you!"
