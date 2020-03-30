module Interpreter where

data Command_Type = 
    Define String Term | 
    Print_Term Term | 
    Print_Var String | 
    Evaluate Nat Term

talk = do
    foo <- putStrLn "Hello, what's do you want me to do?"
    name <- getLine
    if (name == "run")
        then putStrLn ("No way I am doing that :( ")
        else do
            putStrLn ("Ok, let me try")
            talk    
