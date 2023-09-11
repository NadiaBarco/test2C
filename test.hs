f:: Integer -> Integer
f 1 =8
f 4 = 131
f 16 = 16

f11 :: Integer -> Integer
f11 x | x== 1 =8
     | x == 4 = 131
     | x==16 = 16
     | otherwise=0

g:: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1

h :: Integer -> Integer
h x = f11 (g x)

h2:: Integer -> Integer
h2 x = g (f11 x)


absoluto:: Integer -> Integer
absoluto n | n<0 = -n
           |otherwise = n

maximoAbsoluto:: Integer -> Integer -> Integer
maximoAbsoluto n m | absoluto n >= absoluto m =absoluto n
                   | otherwise = absoluto m

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 n m l | n >= m && n>= l = n
              | m > n && m>= l = m
              |otherwise= l


algunoEs0 :: Integer -> Integer -> Bool
algunoEs0 _ 0 =True
algunoEs0 0 _ =False

ambosSon0 :: Integer -> Integer -> Bool
ambosSon0 0 0 = True

mismoInterval0 :: Float -> Float -> Bool
mismoInterval0 n m = n <= 3 && m<=3 || n > 3 && m >3 || n >7 && m > 7

sonIguales :: Integer -> Integer ->Bool
sonIguales n m = n == m
sunaDistintos :: Integer -> Integer -> Integer -> Integer
sunaDistintos n m l | sonIguales n m && sonIguales n l  = 0
                    | sonIguales n m  || sonIguales m l= n + l
                    | otherwise = n +m +l

esMultiploDe :: Integer-> Integer ->Bool
esMultiploDe n m = n `mod` m == 0

digitoUnidades:: Integer -> Integer
digitoUnidades n = if n < 10 then n else n `mod` 10

digitoDecenas :: Integer ->Integer
digitoDecenas n = n `div` 10 `mod` 10


--
estanRelacionados::Integer -> Integer -> Bool
estanRelacionados n m |n * n + n * m * k == 0 && k /= 0 =True
                      | otherwise = False
                      where k = (-n) `div` m


--
prodInt:: (Float,Float) -> (Float,Float) -> Float
prodInt n m = fst n * fst m + snd n* snd m

todoMenor:: (Float,Float) ->(Float,Float) -> Bool
todoMenor n m = fst n < fst m && snd n < snd m

distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos n m = sqrt ((fst m- fst n)**2+ (snd m - snd n)**2)

sumaTerna:: (Float,Float,Float) -> Float
sumaTerna (x,y,z) = x+y+z

{-
sumarSoloMultiplos :: (Integer,Integer,Integer)-> Integer -> Integer
sumarSoloMultiplos t n | not (esMultiploDe (fst t) n) && not (esMultiploDe (snd t) n) && not (esMultiploDe (thr t) n)=0
                             | esMultiploDe (fst t)  n && esMultiploDe (snd t) n && not (esMultiploDe (thr t) n)= fst t + snd t
                             | esMultiploDe (snd t)  n && esMultiploDe (thr t) n && not (esMultiploDe (fst t) n)= snd t + thr t
                             | otherwise =  fst t + snd t + thr t
                             where
                                   fst (x,_,_) = x
                                   snd (_,y,_) = y
                                   thr (_,_,z) = z
                                   
posPrimerPar :: (Integer,Integer,Integer) -> Integer
posPrimerPar (x,y,z) | x `mod` 2 == 0 = 0
                     | y `mod` 2 == 0 = 1
                     | z `mod` 2 == 0 = 2
                     |otherwise =4

posPrimerPar1 :: (Integer,Integer,Integer) -> Integer
posPrimerPar1 (x,y,z) | even x  = 1
                      | even y = 2
                      | even z = 3
                      | otherwise = 4
-}
crearPar:: a-> b -> (a,b)
crearPar x y = (x,y)

invertir :: (a,b) -> (b,a)
invertir (x,y)=(y,x)

--
fMenores :: Integer -> Integer
fMenores n = if n <= 7 then n*n else 2*n -1

gMenores :: Integer-> Integer
gMenores n = if even n then n `div`2 else 3*n+1

todosMenores :: (Integer, Integer, Integer) ->Bool
todosMenores (x,y,z) = fMenores x > gMenores x && fMenores y > gMenores y && fMenores z > gMenores z

--
bisiesto :: Integer -> Bool
bisiesto año = not (año `mod` 4 /=0 || año `mod` 100 ==0 && año `mod` 400 /=0)
--


distanciaManhattan :: (Float, Float, Float) ->(Float, Float, Float) ->Float
distanciaManhattan (x,y,z) (x2,y2,z2) = abs (x-x2) + abs (y-y2) + abs (z-z2)

---

comparar:: Integer -> Integer -> Integer
comparar n m |  mod n 10 + mod (div n  10) 10 > mod m 10 + mod (div m  10) 10 = - 1
             | mod n 10 + mod (div n  10) 10 < mod m 10 + mod (div m  10) 10 = 1
             | otherwise = 0


------------GUIA 4-----------

fibonacci:: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

parteEnrtera :: Float -> Integer
parteEnrtera = round


esDivisible:: Integer -> Integer -> Bool
esDivisible n m | m == n = False
                | otherwise = esDivisible n (m-n)

--
sumaImpares:: Integer -> Integer
sumaImpares n = sumaImparesAux n 1 0

sumaImparesAux :: Integer -> Integer -> Integer -> Integer
sumaImparesAux n m c|  c == n = 0
                    |odd m = m + sumaImparesAux n (m+1) (c+1)
                    |otherwise = sumaImparesAux n (m+1) c
------------REHACER----------LISTO!!
medioFact:: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n*medioFact (n-2)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n*factorial (n-1)

--
--sumaDigitos :: Integer -> Integer 
--sumaDigitos n | n< 10 = n
--              |otherwise= mod n 10 + sumaDigitos (mod (div n 10) 10) 
--funciona, pero en dos pasos ya llego a mi caso base, se deberia sacar el mod 

sumaDigitos :: Integer -> Integer
sumaDigitos n | n< 10 = n
              |otherwise= mod n 10 + sumaDigitos (div n 10)

todosDigitosIguales:: Integer -> Bool
todosDigitosIguales n = todosDigitosIgualesRec n == n

--Este auxiliar verifica si el resto de la division(modulo) es igual al resto del numero, voy dividiendo mi n recursivamente y la gracia es que verifique resto por resto
-- Que devuelva n, no tiene razon xd  
todosDigitosIgualesRec:: Integer -> Integer
todosDigitosIgualesRec n | mod n  10 == n = n
                         | mod n 10 == todosDigitosIgualesRec (div n 10) = n
                         | otherwise = 0

--
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i| n < 10 = n
                | i< n && i == cantidadDeDigitos n = mod n 10
                | otherwise = iesimoDigito (div n 10) i


cantidadDeDigitos :: Integer -> Integer
cantidadDeDigitos n | n < 10 = 1
                    | otherwise = 1 + cantidadDeDigitos (div n 10)

esCapicua:: Integer -> Bool
esCapicua n = n == invertirNumero n (cantidadDeDigitos n)

invertirNumero :: Integer->Integer -> Integer
invertirNumero n d | n < 10 = n
                   | otherwise = mod n 10*(10^(d-1 )) + invertirNumero ( div n 10) (d -1)

--

f1Rec :: Integer -> Integer ->Integer
f1Rec 0 _ = 1
f1Rec n i | i == n = 2^i
          |otherwise= 2^i + f1Rec n (i+1)
--Auxiliar--
fUno :: Integer -> Integer
fUno n = f1Rec n 0
--

f2 :: Integer -> Integer -> Integer
f2 n q = f2Aux n q 1

f2Aux:: Integer -> Integer ->Integer -> Integer
f2Aux _ 0 _ = 0
f2Aux n q i | i == n = q^i
            | otherwise = q^i + f2Aux n q (i+1)

f3:: Integer -> Integer -> Integer
f3 n q = f2Aux (2*n) q 1

f4 :: Integer -> Integer -> Integer
f4 n q = f2Aux (2*n) q n

--
eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n =  eAproxIndice n 0

eAproxIndice::Integer -> Integer -> Float
eAproxIndice 0 _ = 1.0
eAproxIndice n i | n ==i = 1 / fromIntegral (factorial i)
                 | otherwise = 1/fromIntegral (factorial i) + eAproxIndice n (i+1)
--
fe = eAprox 10

--
raizDe2Aprox:: Integer -> Float
raizDe2Aprox 1 = 1
raizDe2Aprox n = sucecionAproxRaizDe2 n -1

sucecionAproxRaizDe2:: Integer -> Float
sucecionAproxRaizDe2 1  = 2
sucecionAproxRaizDe2 n = 2 + 1 / sucecionAproxRaizDe2 (n-1)


---------------- REVISAR TODO-----------------------------
--
--sumatoriaDoble :: Integer -> Integer -> Integer
--sumatoriaDoble n m =sumatoriaDobleIndiceI n 1 m 1

--umatoriaDobleIndiceI :: Integer -> Integer -> Integer -> Integer -> Integer
--sumatoriaDobleIndiceI 1 i 1 j = 1
--sumatoriaDobleIndiceI n i m j | n

{--sumaPotencias13:: Integer -> Integer -> Integer
sumaPotencias13 1 _ = sumaPontenciasFijoI 1 m
sumaPotencias13 n m = sumaPontenciasFijoI n m + sumaPotencias13 (n-1) m 

sumaPontenciasFijoI i 1 = i
sumaPontenciasFijoI i m = i^m + sumaPontenciasFijoI i (m-1)--}


--Sumando uno de mas ver--
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias 1 n m = 1
sumaPotencias q n m = q^(n+m) + sumaPotencias (q-1) n m 

--umaRacionales:: Integer -> Integer -> Float
--sumaRacionales n m = fromIntegral(sumaGauss n )* fromIntegral(serieArmonica m 1 )

{--sumaGauss :: Integer -> Integer
sumaGauss 1 = 1
sumaGauss n = (n*(n+1))/ 2
--}
serieArmonica :: Integer -> Integer -> Integer
serieArmonica 1 i = 1 
serieArmonica n i | n == i = 0
                  | otherwise = serieArmonica n (i+1)


menorDivisor:: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer ->Integer
menorDivisorDesde n i | mod n i == 0 = i 
                      | otherwise = menorDivisorDesde n (i+1) 

--menorDivisorHsta n 1 = 1
--menorDivisorHsta n d | mod n d  == 0 =1 + cantidadDeDivisoresHasta n (d-1)
--                     | otherwise = cantidadDeDivisoresHasta n (d-1)

esPrimo:: Integer -> Bool
esPrimo n = menorDivisor n  == n 
--
sonCoprimos:: Integer-> Integer -> Bool
sonCoprimos n m | n == m = False
                | mod n m == 0 || mod m n  == 0 =False
                |otherwise =True
--------------------------------------------
----Idea ejercicio 19 ---
esSumaInicialesPrimos :: Integer -> Bool
esSumaInicialesPrimos n = sumaHastaElNPrimo n 2 0 == n

sumaHastaElNPrimo:: Integer-> Integer -> Integer -> Integer
sumaHastaElNPrimo 2 _ _ = 2
sumaHastaElNPrimo n p c | n == c && not (esPrimo p) =0 
                        | n == c && esPrimo p = p + sumaHastaElNPrimo n (p+1) c
                        | otherwise= sumaHastaElNPrimo (n-1) (p+1) (c+1)

--------------------
siguientePrimo :: Integer -> Integer 
siguientePrimo n | esPrimo n = n
                 | otherwise = siguientePrimo (n+1)


enesimoPrimo:: Integer -> Integer
enesimoPrimo n = enesimoPrimoCont n 2 0

enesimoPrimoCont:: Integer -> Integer -> Integer -> Integer
enesimoPrimoCont 1 p c = 2
enesimoPrimoCont n p c | n == c && esPrimo(p) = p
                       | otherwise = enesimoPrimoCont n (p+1) (c+1)
------

esFibonacci :: Integer -> Bool
esFibonacci 1 = True
esFibonacci n = perteneceAFibo n 1

perteneceAFibo :: Integer -> Integer -> Bool
perteneceAFibo n m | n == fibonacci(m) = True
                    | n < fibonacci (m) = False
                   | otherwise= perteneceAFibo n (m+1)


mayorDigitoPar:: Integer -> Integer
mayorDigitoPar 1 = 1
mayorDigitoPar n = mayorDivisor n 2 0

mayorDivisor::Integer -> Integer -> Integer -> Integer
mayorDivisor 1 _ _= 1
mayorDivisor n d c | mod n d == 0 && n == c = d
                   | mod n d /=0 = mayorDivisor n (d+1) (c+1)
                   | otherwise = mayorDivisor n (d+1) (c)



----------------------GUIA 5 RECURSION SOBRE LISTAS------------------

longitud :: [t] -> Integer
longitud [] = 0
longitud l = 1 + longitud (tail l)

ultimo :: [t] -> t
ultimo [x] = x
ultimo x = ultimo (tail x)



--hayRepetidos :: (Eq t) -> [t] -> Bool
--hayRepetidos (x:xs) = 

{--
cantidadDeApariciones :: t -> [t] -> Integer
cantidadDeApariciones _ [] = 0
cantidadDeApariciones e (x:xs) | pertenece e (x:xs) = 1
                               | 
--}

reverso :: [t] -> [t]
reverso [x] = [x]
reverso (x:xs) = reverso (xs) ++ [x]

---Ejercicio 2 --
pertenece::(Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs
                   
todosIguales:: (Eq t) => [t] -> Bool
todosIguales [x] = True
todosIguales (x:xs) | x /= head xs = False
                    | otherwise= todosIguales xs 

todosDistintos:: (Eq t) => [t] -> Bool
todosDistintos [x] = True
todosDistintos (x:xs)| x == head xs = False
                     | otherwise = todosDistintos xs
-------------

