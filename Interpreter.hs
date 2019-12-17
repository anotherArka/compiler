
talk = do
    foo <- putStrLn "Hello, what's do you want me to do?"
    name <- getLine
    if (name == "run")
        then putStrLn ("No way I am doing that :( ")
        else do
            putStrLn ("Ok, let me try")
            talk    
