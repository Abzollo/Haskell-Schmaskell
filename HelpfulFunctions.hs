chooseRem 0 xs = [([],xs)]
chooseRem _ [] = []
chooseRem k (x:xs) = map (putInFst x) (chooseRem (k-1) xs) ++ map (putInSnd x) (chooseRem k xs)
    where putInFst x (xs,ys) = (x:xs,   ys)
          putInSnd y (xs,ys) = (  xs, y:ys)