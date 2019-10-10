type Interval = (Integer,Integer)
factors :: Int -> [Int]
factors n =[x | x<-[1..n],n `mod` x==0]

prime :: Int -> Bool
prime n = factors n ==[1,n]
-- to find any kth prime number just give n>k+1--
kthprime :: Int->Int-> Int
kthprime n k = [x|x<-[2..n],prime x]!!(k-1)

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

leq :: Integer -> Float -> Bool
m `leq` x = fromInteger m <= x

lt :: Float -> Integer -> Bool
x `lt` n = x < fromInteger n

lower :: Float -> Integer
lower x = until (`leq` x) (*2) (-1)

upper :: Float -> Integer
upper x = until (x `lt`) (*2) 1

shrink :: Float -> Interval -> Interval
shrink x (m,n) = if p `leq` x then (p,n) else (m,p)
                 where p = choose (m,n)

choose :: Interval -> Integer
choose (m,n) = (m+n) `div` 2

bound :: Float -> Interval
bound x = (lower x, upper x)

floor2 :: Float -> Integer
floor2 x = fst (until unit (shrink x) (bound x))
            where unit (m,n) = (m+1 == n)