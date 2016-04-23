-- Chapter1Exercise2.hs
module Chapter1Exercise2 where
import System.Environment

-- Exercise 1:
-- Change the program so it performs a simple arithmetic operation on the two
-- arguments and prints out the result. You can use read to convert a string to
-- a number, and show to convert a number back into a string. Play around with
-- different operations.

main :: IO ()
main = do
  -- Doing this prefix notation style, like Lisp!
  -- Could use readMaybe, some let bindings, and other stuff too, but I'm trying
  -- not to overengineer this *too* much. :)
  args <- getArgs
  (putStrLn . show) $ case args !! 0 of
    "+"   -> ((read (args !! 1)) :: Double) + ((read (args !! 2)) :: Double)
    "-"   -> ((read (args !! 1)) :: Double) - ((read (args !! 2)) :: Double)
    "*"   -> ((read (args !! 1)) :: Double) * ((read (args !! 2)) :: Double)
    "/"   -> ((read (args !! 1)) :: Double) / ((read (args !! 2)) :: Double)
    _     -> 0 -- default error...
