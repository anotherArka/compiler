import Data.Void

data Contradict = 
    Contra (Contradict -> Void)
    
fun :: Contradict -> (Contradict -> Void)
fun (Contra what) = what

oops :: Contradict
oops = Contra (fun oops)

yikes :: Void
yikes = (fun oops) oops
