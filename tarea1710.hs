{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

------------------ejercicio 1-------------------------
--Definir la función firstToEnd tal que (firstToEnd xs) es una lista donde el primer
----elemento de xs, pasa a ser el último elemento de la nueva lista

rotaLista :: [a] -> [a]
rotaLista xs = tail xs ++ [head xs]

------------------ejercicio 2-------------------------
--- Definir la función minAndMax tal que (minAndMax xs) es una lista con únicamente 
-- los 2 elementos (elemento mínimo de xs y elemento máximo de xs), donde xs es una lista.

rango :: (Foldable t, Ord a) => t a -> [a]
rango xs = [minimum xs, maximum xs]

------------------ejercicio 3-------------------------
--Definir la función minorsFirstElement tal que (minorsFirstElement xs) es una lista con
--los elementos menores al primer elemento de xs, donde xs es una lista.(El primer
--elemento se ignora)

menores :: (Integral a)=> [a] -> [a]
menores [] = []
menores(x:xs) = filter (<x) xs

------------------ejercicio 4-------------------------
--Definir la función greaterOrEqualFirstElement tal que (greaterOrEqualFirstElement
-- xs) es una lista con los elementos mayores o iguales al primer elemento de xs,
-- donde xs es una lista.(El primer elemento se ignora)

mayores :: (Integral a)=> [a] -> [a]
mayores [] = []
mayores(x:xs) = filter (>x) xs

------------------ejercicio 5-------------------------
--Definir la función minorsToSumFirstAndSecondElem tal que
--(minorsToSumFirstAndSecondElem xs) es una lista con los elementos menores a la
--suma del primer y segundo elemento de xs (sin tomar en cuenta los primeros 2
--elementos), donde xs es una lista.

summinors :: (Integral a)=> [a] -> [a]
summinors [] = []
summinors (x:y:xs)= filter (<x+y) xs

------------------ejercicio 6-------------------------
--Definir la función listSumDuplaToList tal que (listSumDuplaToList xs) es una lista en
--la que cada elemento es la suma de los elementos de cada dupla, donde xs es una
--lista de duplas.

sumaD :: (Integral a)=> [(a, a)] -> [a]
sumaD []=[]
sumaD n = [a+b | (a,b) <- n]

------------------ejercicio 7-------------------------
--Definir la función listMultTripletaToList tal que (listMultTripletaToList xs) es una lista
--en la que cada elemento es la multiplicación de los elementos de cada tripleta,
--donde xs es una lista de tripletas.

sumaT :: (Integral a)=> [(a, a, a)] -> [a]
sumaT n = [a*b*c | (a,b,c) <- n]

------------------ejercicio 8-------------------------
--Definir la función changeFstToSnd tal que (changeFstToSnd xs) es una lista en
--donde los elementos de una dupla cambian de posición, donde xs es una lista de
--duplas.

revTuplas :: (Integral a)=> [(a, a)] -> [(a, a)]
revTuplas n = [(b,a)|(a,b) <- n]

------------------ejercicio 9-------------------------
--Definir la función sumVectors tal que (sumVectors xs) es un vector resultante de la
--suma de los diferentes vectores de xs, donde xs es una lista de duplas.
--Material de apoyo: https://www.educaplus.org/movi/1_4sumavector.html

suma :: Num a => [a] -> [a] -> [a]
suma [] []  = []
suma (x:xs) (y:ys) = x+y : suma xs ys

sumaVec1 :: Num a => [[a]] -> [a]
sumaVec1 []          = []
sumaVec1 [xs]        = xs
sumaVec1 (xs:ys:zss) = suma xs (sumaVec1 (ys:zss))

------------------ejercicio 10-------------------------
--Definir la función dividers tal que (dividers n) es una lista de los divisores de n, donde
--n es un número.

divisores :: Integral a => a -> [a]
divisores n = filter (\x -> mod n x == 0) [1 .. n]

------------------ejercicio 11-------------------------
--Definir la función primeNumbers tal que (primeNumbers n) es una lista con los
--números primos existentes de 1 a n, donde n es un número.
--Recuerda: un número primo tiene únicamente 2 divisores 1 y el mismo número.

divisible::Int->Int->Bool
divisible x y = mod x y ==0

divisibles::Int->[Int]
divisibles x = [y | y <-[1..x],divisible x y]

esPrimo::Int->Bool
esPrimo n = length (divisibles n) <=2

primos::Int->[Int]
primos n = [x | x <-[2..n],esPrimo x]

------------------ejercicio 12-------------------------
--Definir la función infinitePrimeNumbers tal que (infinitePrimeNumbers) es una lista
--infinita de los números primos.

nPrimosi :: [Integer]
nPrimosi = criba [2..]
         where criba (p:ps) = p : criba [n | n<-ps, mod n p /= 0]