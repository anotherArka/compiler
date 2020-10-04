import Data.List
import Junkyard
import Lambda_happy
import Lambda_system
import Lambda_compiler

trial :: String -> IO String
trial msg = do
  putStrLn msg
  new_msg <- getLine
  if (new_msg == "quit") then return "bye"
  else (trial new_msg)

main :: IO ()
main = repl ("Hello",[])
