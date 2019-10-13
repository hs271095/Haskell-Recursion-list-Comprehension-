rev :: [a]->[a]
rev [] = []
rev (x:xs)=rev xs ++[x]

rev1 :: [a] -> [a]
rev1  = foldr (\ x xs -> xs ++ [x]) []

something :: [a]->[a]->[a]
something xs ys =concat[xs,ys]

some1 ::[a]->[a]->[a]
some1 [] []=[]
some1 xs ys=