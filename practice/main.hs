import Data.List (sort)

half :: Fractional a => a -> a
half x = x / 2

xor :: Bool -> Bool -> Bool
xor a b = not (a && b) && (a || b)

cbrt :: Floating a => a -> a
cbrt a = a ** (1/3)

s :: Floating a => a -> a -> a -> a
s a b c = (a+b+c)/2

heron :: Floating a => a -> a -> a -> a
heron x y z= sqrt((s x y z) * (s x y z - x) * (s x y z - y) * (s x y z - z))

isTriangular :: (Ord a, Num a) => a -> a -> a -> Bool
isTriangular a b c = (a+b > c) && (a+c > b) && (b+c > a)

isPythagorean :: (Num a, Eq a, Ord a) => a -> a -> a -> Bool
isPythagorean x y z = (sorted !! 0)^2 + (sorted !! 1)^2 == (sorted !! 2)^2
    where 
        sorted = sort [x,y,z]

five :: (Eq a, Num a) => a -> [Char]
five x 
    | x == 5 = "five"
    | otherwise = "not five"

min3 :: Ord a => a -> a -> a -> a
min3 a b c = sort [a,b,c] !! 0

testPh :: (Ord a, Floating a) => a -> [Char]
testPh ah = if x < 7 then "acid" else (if x == 7 then "neutral" else "basic")
    where x = -logBase 10 ah

testBMI :: (Ord a, Fractional a) => a -> a -> [Char]
testBMI x y
    | bmi < 18.5 = "underweight"
    | bmi >= 18.5 && bmi < 25 = "healthy weight"
    | bmi >= 25 && bmi < 30 = "overweight"
    | bmi >= 30 = "obese"
    where bmi = x / (y^2)

f :: (Ord a, Num a, Integral b) => a -> b
f 0 = 0
f x = if x > 0 then 1 else -1

factorial :: (Ord p, Num p) => p -> p
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial(x-1)

mPower :: (Fractional a, Integral t) => a -> t -> a
mPower a 0 = 1
mPower a t = a * (mPower a (t-1))

fib :: (Num a, Ord a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib x = if x > 1 then fib(x-1) + fib(x-2) else error "invalid order"

ackermann :: (Num a, Ord a, Num t, Ord t) => a -> t -> t
ackermann m n = 
    if m == 0
        then n + 1
        else if m > 0 && n == 0
            then ackermann (m-1) 1
            else ackermann (m-1) (ackermann m (n-1))

pascal :: (Num a, Ord a, Num p) => a -> a -> p
pascal k n = 
    if k == 1 || k == n
        then 1
        else if 1 < k && k < n
            then (pascal (k-1) (n-1)) + (pascal k (n-1))
            else error "invalid parameters"

myFst :: (a,b) -> a
myFst (a,_) = a

mySnd :: (a, b) -> b
mySnd (_,b) = b

mySwap :: (b, a) -> (a, b)
mySwap (a,b) = (b,a)

distance2 :: Floating a => (a, a) -> (a, a) -> a
distance2 (ax, ay) (bx, by) = sqrt ((ax-bx)**2 + (ay-by)**2)

distanceInf :: (Num a, Ord a) => (a, a) -> (a, a) -> a
distanceInf (ax, ay) (bx, by) = maximum [abs(ax-bx), abs(ay-by)]

myHead :: [a] -> a
myHead (a:_) = a

collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz n =
    if mod n 2 == 0
        then fmap (+1) (collatz (div n 2))
        else fmap (+1) (collatz (3 * n + 1))



convert :: Char -> Char
convert x
    | x == 'G' = 'C'
    | x == 'C' = 'G'
    | x == 'T' = 'A'
    | x == 'A' = 'U'
    | otherwise = '0'