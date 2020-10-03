module MonadE where

data E a = Ok a | Failed String
  deriving (Show, Eq)

thenE :: E a -> (a -> E b) -> E b
thenE m k = case m of
  Ok a -> k a
  Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = case m of
  Ok a -> Ok a
  Failed e -> k e
  
instance Functor E where  
    fmap f (Failed msg) = Failed msg 
    fmap f (Ok x)       = Ok (f x)    
    
instance Applicative E where  
    pure = Ok  
    (Failed msg) <*> _ = Failed msg  
    (Ok f) <*> x = fmap f x   
  
instance Monad E where
    (Ok val)     >>= f = f val
    (Failed msg) >>= f = Failed msg
    return x = Ok x
    fail msg = Failed msg  
