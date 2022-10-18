------------------ejercicio 1-------------------------
rotaLista :: [a] -> [a]
rotaLista xs = tail xs ++ [head xs]
------------------ejercicio 2-------------------------

rango :: (Foldable t, Ord a) => t a -> [a]
rango xs = [minimum xs, maximum xs]

------------------ejercicio 3-------------------------
menores :: (Integral a)=> [a] -> [a]
menores(x:xs) = filter (<x) xs

------------------ejercicio 4-------------------------

mayores :: (Integral a)=> [a] -> [a]
mayores(x:xs) = filter (>x) xs

------------------ejercicio 5-------------------------
summinors :: (Integral a)=> [a] -> [a]
summinors (x:y:xs)= filter (<x+y) xs

------------------ejercicio 6-------------------------

sumaD :: (Integral a)=> [(a, a)] -> [a]
sumaD n = [a+b | (a,b) <- n]

------------------ejercicio 7-------------------------
sumaT :: (Integral a)=> [(a, a, a)] -> [a]
sumaT n = [a*b*c | (a,b,c) <- n]

------------------ejercicio 8-------------------------
revTuplas :: (Integral a)=> [(a, a)] -> [(a, a)]
revTuplas n = [(b,a)|(a,b) <- n]

------------------ejercicio 9-------------------------
suma :: Num a => [a] -> [a] -> [a]
suma [] []  = []
suma (x:xs) (y:ys) = x+y : suma xs ys

sumaVec1 :: Num a => [[a]] -> [a]
sumaVec1 []          = []
sumaVec1 [xs]        = xs
sumaVec1 (xs:ys:zss) = suma xs (sumaVec1 (ys:zss))

------------------ejercicio 10-------------------------
divisores :: Integral a => a -> [a]
divisores n = filter (\x -> mod n x == 0) [1 .. n]

------------------ejercicio 11-------------------------
divisible::Int->Int->Bool
divisible x y = mod x y ==0

divisibles::Int->[Int]
divisibles x = [y | y <-[1..x],divisible x y]

esPrimo::Int->Bool
esPrimo n = length (divisibles n) <=2

primos::Int->[Int]
primos n = [x | x <-[2..n],esPrimo x]

------------------ejercicio 12-------------------------
nPrimosi :: [Integer]
nPrimosi = criba [2..]
         where criba (p:ps) = p : criba [n | n<-ps, mod n p /= 0]