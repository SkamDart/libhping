import Network.Ping

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs 
  case args of [host] -> ping host >>= print
               _      -> putStrLn "usage <host>"
