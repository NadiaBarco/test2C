--Ejercicio 1.a
f:: Integer -> Integer
f 1 =8
f 4 = 131
f 16 = 16

f11 :: Integer -> Integer
f11 x | x== 1 =8
     | x == 4 = 131
     | x==16 = 16
     | otherwise=0

--Ejercicio 1.b
g:: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1

--Ejercicio 1.c
h :: Integer -> Integer
h x = f11 (g x)

h2:: Integer -> Integer
h2 x = g (f11 x)

--Ejercicio 2.a
absoluto:: Integer -> Integer
absoluto n | n<0 = -n
           |otherwise = n

--Ejercicio 2.b
maximoAbsoluto:: Integer -> Integer -> Integer
maximoAbsoluto n m | absoluto n >= absoluto m =absoluto n
                   | otherwise = absoluto m

--Ejercicio 2.c
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 n m l | n >= m && n>= l = n
              | m > n && m>= l = m
              |otherwise= l

--Ejercicio 2.d
algunoEs0 :: Integer -> Integer -> Bool
algunoEs0 _ 0 =True
algunoEs0 0 _ =False

--Ejercicio 2.e
ambosSon0 :: Integer -> Integer -> Bool
ambosSon0 0 0 = True

--Ejercicio 2.f
mismoInterval0 :: Float -> Float -> Bool
mismoInterval0 n m = n <= 3 && m<=3 || n > 3 && m >3 || n >7 && m > 7

--Ejercicio 2.g

sonIguales :: Integer -> Integer ->Bool
sonIguales n m = n == m

sunaDistintos :: Integer -> Integer -> Integer -> Integer
sunaDistintos n m l | sonIguales n m && sonIguales n l  = 0
                    | sonIguales n m  || sonIguales m l= n + l
                    | otherwise = n +m +l

--Ejercicio 2.h
esMultiploDe :: Integer-> Integer ->Bool
esMultiploDe n m = n `mod` m == 0

--Ejercicio 2.i
digitoUnidades:: Integer -> Integer
digitoUnidades n = if n < 10 then n else n `mod` 10

--Ejercicio 2.j
digitoDecenas :: Integer ->Integer
digitoDecenas n = n `div` 10 `mod` 10


--Ejercicio 3
estanRelacionados::Integer -> Integer -> Bool
estanRelacionados n m |n * n + n * m * k == 0 && k /= 0 =True
                      | otherwise = False
                      where k = (-n) `div` m


----Ejercicio 4.a
prodInt:: (Float,Float) -> (Float,Float) -> Float
prodInt n m = fst n * fst m + snd n* snd m

--Ejercicio 4.b
todoMenor:: (Float,Float) ->(Float,Float) -> Bool
todoMenor n m = fst n < fst m && snd n < snd m

--Ejercicio 4.c
distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos n m = sqrt ((fst m- fst n)**2+ (snd m - snd n)**2)

--Ejercicio 4.d
sumaTerna:: (Float,Float,Float) -> Float
sumaTerna (x,y,z) = x+y+z

----Ejercicio 4.e
sumarSoloMultiplos :: (Integer,Integer,Integer)-> Integer -> Integer
sumarSoloMultiplos t n | not (esMultiploDe (fst t) n) && not (esMultiploDe (snd t) n) && not (esMultiploDe (thr t) n)=0
                             | esMultiploDe (fst t)  n && esMultiploDe (snd t) n && not (esMultiploDe (thr t) n)= fst t + snd t
                             | esMultiploDe (snd t)  n && esMultiploDe (thr t) n && not (esMultiploDe (fst t) n)= snd t + thr t
                             | otherwise = fst t+ snd t + thr t
                             where
                                   fst (x,_,_) = x
                                   snd (_,y,_) = y
                                   thr (_,_,z) = z
--Ejercicio 4.f                                   
posPrimerPar :: (Integer,Integer,Integer) -> Integer
posPrimerPar (x,y,z) | even x = 0
                     | even y = 1
                     | even z = 2
                     |otherwise =4
{-
--Usando una funcion predefinida, innecesario (?, no se pueden usar funciones pre definidas que no esten en la teorica
posPrimerPar1 :: (Integer,Integer,Integer) -> Integer
posPrimerPar1 (x,y,z) | even x  = 1
                      | even y = 2
                      | even z = 3
                      | otherwise = 4
-}
--Ejercicio 4.g
crearPar:: a-> b -> (a,b)
crearPar x y = (x,y)

--Ejercicio 4.h
invertir :: (a,b) -> (b,a)
invertir (x,y)=(y,x)

--Ejercicio 5
fMenores :: Integer -> Integer
fMenores n = if n <= 7 then n*n else 2*n -1

gMenores :: Integer-> Integer
gMenores n = if even n then n `div`2 else 3*n+1

todosMenores :: (Integer, Integer, Integer) ->Bool
todosMenores (x,y,z) = fMenores x > gMenores x && fMenores y > gMenores y && fMenores z > gMenores z

--Ejercicio 6
bisiesto :: Integer -> Bool
bisiesto año = not (año `mod` 4 /=0 || año `mod` 100 ==0 && año `mod` 400 /=0)


--Ejercicio 7
distanciaManhattan :: (Float, Float, Float) ->(Float, Float, Float) ->Float
distanciaManhattan (x,y,z) (x2,y2,z2) = abs (x-x2) + abs (y-y2) + abs (z-z2)

--Ejercicio 8
comparar:: Integer -> Integer -> Integer
comparar n m |  mod n 10 + mod (div n  10) 10 > mod m 10 + mod (div m  10) 10 = - 1
             | mod n 10 + mod (div n  10) 10 < mod m 10 + mod (div m  10) 10 = 1
             | otherwise = 0


------------GUIA 4 RECURSION SOBRE NUMEROS ENTEROS-----------

--Ejercicio 1
fibonacci:: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

--Ejercicio 2
-- Se supone que se utilice recursion, REHACER
parteEnrtera :: Float -> Integer
parteEnrtera = round

--Ejercicio 3
esDivisible:: Integer -> Integer -> Bool
esDivisible n m | m == n = False
                | otherwise = esDivisible n (m-n)

--Ejercicio 4
sumaImpares:: Integer -> Integer
sumaImpares n = sumaImparesAux n 1 0

sumaImparesAux :: Integer -> Integer -> Integer -> Integer
sumaImparesAux n m c|  c == n = 0
                    |odd m = m + sumaImparesAux n (m+1) (c+1)
                    |otherwise = sumaImparesAux n (m+1) c
------------REHACER----------LISTO!!
--Ejercicio 5
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

--Ejercicio 6
sumaDigitos :: Integer -> Integer
sumaDigitos n | n< 10 = n
              |otherwise= mod n 10 + sumaDigitos (div n 10)

--Ejercicio 7
todosDigitosIguales:: Integer -> Bool
todosDigitosIguales n = todosDigitosIgualesRec n == n

--Este auxiliar verifica si el resto de la division(modulo) es igual al resto del numero, voy dividiendo mi n recursivamente y la gracia es que verifique resto por resto
-- Que devuelva n, no tiene razon xd  
todosDigitosIgualesRec:: Integer -> Integer
todosDigitosIgualesRec n | mod n  10 == n = n
                         | mod n 10 == todosDigitosIgualesRec (div n 10) = n
                         | otherwise = 0

--Ejercicio 8
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i| n < 10 = n
                | i< n && i == cantidadDeDigitos n = mod n 10
                | otherwise = iesimoDigito (div n 10) i

cantidadDeDigitos :: Integer -> Integer
cantidadDeDigitos n | n < 10 = 1
                    | otherwise = 1 + cantidadDeDigitos (div n 10)

--Ejercicio 9
esCapicua:: Integer -> Bool
esCapicua n = n == invertirNumero n (cantidadDeDigitos n)

invertirNumero :: Integer->Integer -> Integer
invertirNumero n d | n < 10 = n
                   | otherwise = mod n 10*10^(d-1 ) + invertirNumero ( div n 10) (d -1)

--Ejercicio 10.a
--Auxiliar para el indice
f1Rec :: Integer -> Integer ->Integer
f1Rec 0 _ = 1
f1Rec n i | i == n = 2^i
          |otherwise= 2^i + f1Rec n (i+1)

fUno :: Integer -> Integer
fUno n = f1Rec n 0
--
--Ejercicio 10.b
f2 :: Integer -> Integer -> Integer
f2 n q = f2Aux n q 1

f2Aux:: Integer -> Integer ->Integer -> Integer
f2Aux _ 0 _ = 0
f2Aux n q i | i == n = q^i
            | otherwise = q^i + f2Aux n (q-1) (i+1)

--Ejercicio 10.c
f3:: Integer -> Integer -> Integer
f3 n q = f2Aux (2*n) q 1

--Ejercicio 10.d
f4 :: Integer -> Integer -> Integer
f4 n q = f2Aux (2*n) q n

--Ejercicio 11.a
eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n =  eAproxIndice n 0

eAproxIndice::Integer -> Integer -> Float
eAproxIndice 0 _ = 1.0
eAproxIndice n i | n ==i = 1 / fromIntegral (factorial i)
                 | otherwise = 1/fromIntegral (factorial i) + eAproxIndice n (i+1)

----Ejercicio 11.b
fe = eAprox 10

--Ejercicio 12
raizDe2Aprox:: Integer -> Float
raizDe2Aprox 1 = 1
raizDe2Aprox n = sucecionAproxRaizDe2 n -1

sucecionAproxRaizDe2:: Integer -> Float
sucecionAproxRaizDe2 1  = 2
sucecionAproxRaizDe2 n = 2 + 1 / sucecionAproxRaizDe2 (n-1)


---------------- REVISAR TODO-----------------------------
--
sumatoriaDoble :: Integer -> Integer -> Integer
sumatoriaDoble 1 m = 1^m
sumatoriaDoble n m = f2Aux m n 1

indiceI:: Integer -> Integer -> Integer
indiceI n 0 = 0
indiceI n q | n==q = n
            | otherwise = n + indiceI n (q+1)
--sumatoriaDoble :: Integer -> Integer -> Integer
--sumatoriaDoble n m =sumatoriaDobleIndiceI n 1 m 1

--sumatoriaDobleIndiceI :: Integer -> Integer -> Integer -> Integer -> Integer
--sumatoriaDobleIndiceI 1 i 1 j = 1
--sumatoriaDobleIndiceI n i m j | n

{--sumaPotencias13:: Integer -> Integer -> Integer
sumaPotencias13 1 _ = sumaPontenciasFijoI 1 _
sumaPotencias13 n m = sumaPontenciasFijoI n m + sumaPotencias13 (n-1) m 

sumaPontenciasFijoI :: Integer -> Integer -> Integer
sumaPontenciasFijoI i 1 = i
sumaPontenciasFijoI i m = i^m + sumaPontenciasFijoI i (m-1)


--Sumando uno de mas ver--
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias 1 n m = 1
sumaPotencias q n m = q^(n+m) + sumaPotencias (q-1) n m --}

--sumaRacionales:: Integer -> Integer -> Float
--sumaRacionales n m = fromIntegral(sumaGauss n )* fromIntegral(serieArmonica m 1 )

{--sumaGauss :: Integer -> Integer
sumaGauss 1 = 1
sumaGauss n = (n*(n+1))/ 2
--}
serieArmonica :: Integer -> Integer -> Integer
serieArmonica 1 i = 1
serieArmonica n i | n == i = 0
                  | otherwise = serieArmonica n (i+1)

--Ejercicio 16a
menorDivisor:: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer ->Integer
menorDivisorDesde n i | mod n i == 0 = i
                      | otherwise = menorDivisorDesde n (i+1)

--menorDivisorHsta n 1 = 1
--menorDivisorHsta n d | mod n d  == 0 =1 + cantidadDeDivisoresHasta n (d-1)
--                     | otherwise = cantidadDeDivisoresHasta n (d-1)
--Ejercicio 16b
esPrimo:: Integer -> Bool
esPrimo n = menorDivisor n  == n
--
--Ejercicio 16c
sonCoprimos:: Integer-> Integer -> Bool
sonCoprimos n m | n == m = False
                | mod n (menorDivisorDesde m 1) /= 0 || mod m (menorDivisorDesde n 1)  /= 0 = True
                |otherwise =False
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
enesimoPrimoCont n p c | n == c && esPrimo p = p
                       | otherwise = enesimoPrimoCont n (p+1) (c+1)
------
--Ejercicio 17
esFibonacci :: Integer -> Bool
esFibonacci 1 = True
esFibonacci n = perteneceAFibo n 1

perteneceAFibo :: Integer -> Integer -> Bool
perteneceAFibo n m | n == fibonacci m = True
                    | n < fibonacci m = False
                   | otherwise= perteneceAFibo n (m+1)

--Ejercicio 18
mayorDigitoPar:: Integer -> Integer
mayorDigitoPar 1 = 1
mayorDigitoPar n = mayorDivisor n 2 0

mayorDivisor::Integer -> Integer -> Integer -> Integer
mayorDivisor 1 _ _= 1
mayorDivisor n d c | mod n d == 0 && n == c = d
                   | mod n d /=0 = mayorDivisor n (d+1) (c+1)
                   | otherwise = mayorDivisor n (d+1) c



----------------------GUIA 5 RECURSION SOBRE LISTAS------------------

{--
cantidadDeApariciones :: t -> [t] -> Integer
cantidadDeApariciones _ [] = 0
cantidadDeApariciones e (x:xs) | pertenece e (x:xs) = 1
                               | 
--}

--Ejercicio 1.1
longitud :: [t] -> Integer
longitud [] = 0
longitud l = 1 + longitud (tail l)

--Ejercicio 1.2
ultimo :: [t] -> t
ultimo [x] = x
ultimo x = ultimo (tail x)


--Ejercicio 1.3
principio:: [t] -> [t]
principio [x] = []
principio (x:xs) = x:principio xs

--Ejercicio 1.4
reverso :: [t] -> [t]
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]

---Ejercicio 2.1 --
pertenece::(Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

--Ejercicio 2.2
todosIguales:: (Eq t) => [t] -> Bool
todosIguales [x] = True
todosIguales (x:xs) | x /= head xs = False
                    | otherwise= todosIguales xs

--Ejercicio 2.3
todosDistintos:: (Eq t) => [t] -> Bool
todosDistintos [x] = True
todosDistintos (x:xs)| x == head xs = False
                     | otherwise = todosDistintos xs

--Ejercicio 2.4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [x] = False
hayRepetidos (x:y:xs) | x == y = True
                      | otherwise = hayRepetidos (x:xs)

--cantidadDeApariciones tiene un requerimiento, el elemento e debe pertenecer a la lista l
cantidadDeApariciones :: Eq t => t -> [t] -> Integer
cantidadDeApariciones _ [] = 0
cantidadDeApariciones e l | e == head l = 1 + cantidadDeApariciones e (tail l)
                          | otherwise = cantidadDeApariciones e (tail l)

--Ejercicio 2.5
quitar :: (Eq t) => t -> [t] -> [t]
quitar e [] = []
quitar e (x:xs) | e == x =  xs
                | otherwise = x:quitar e xs
--Ejercicio 2.6
quitarTodos:: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos e (x:xs) | e == x = quitarTodos e xs
                     | otherwise = x : quitarTodos e xs

--Ejercicio 2.7
eliminarRepetidos::(Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | cantidadDeApariciones x (x:xs) > 1 = x:eliminarRepetidos (quitarTodos x (x:xs))
                         | otherwise= x:eliminarRepetidos xs 

--Ejercicio 2.8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos l1 l2 = incluido l1 l2 && incluido l2 l1
--incluido verifica que si todos los elementos l1 estan en la lista l2 
incluido ::(Eq t) => [t] -> [t] -> Bool
incluido [] _ = True
incluido l1 l2 | pertenece (head (eliminarRepetidos l1)) l2 = incluido (tail l1) l2
               | otherwise = False

--Ejercicio 2.9
capicua:: (Eq t ) =>[t] -> Bool
capicua s = s== reverso s 

--Ejercicio 3.1
sumatoria::[Integer] ->Integer
sumatoria [] = 0
sumatoria s = head s + sumatoria (tail s)

--Ejercicio 3.2
productoria :: [Integer] -> Integer 
productoria [] = 1
productoria s = head s *productoria (tail s)

--Ejercicio 3.3
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:xs) | x > maximo xs  = x
               | otherwise = maximo xs
--Ejercicio 3.4
sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [] =[]
sumarN n (x:xs) = n + x : sumarN n xs 

--Ejercicio 3.5
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs) 

--Ejercicio 3.6
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo l = sumarN (ultimo l) l

--Ejercicio 3.7
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | mod x 2 ==0 = x:pares xs
             | otherwise = pares xs
--Ejercicio 3.8
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | esMultiploDe x n = x :multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

--Ejercicio 3.9
ordenar :: [Integer] -> [Integer]
ordenar [x] = [x]
ordenar s =  reverso(ordenarDecreciente s)

--Auxiliar medio al pedo :/ , no se me ocurrio otra forma de solucionar esto
--ordenar s =reverso(maximo s : ordenar (quitar (maximo s) s ))
ordenarDecreciente :: [Integer] -> [Integer]
ordenarDecreciente [x] = [x]
ordenarDecreciente s = maximo s : ordenarDecreciente (quitar (maximo s) s )

--Ejercicio 4.1
sacarBlancosRepetidos::[Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs) | x == ' ' && y == ' ' = sacarBlancosRepetidos (x:xs)
                              | x/= ' ' && y== ' '= x:sacarBlancosRepetidos (y:xs)
                              | otherwise = x:y:sacarBlancosRepetidos xs

--Ejercicio 4.2
contarPalabras::[Char] -> Integer
contarPalabras [] =0
contarPalabras [' '] =0
contarPalabras s= 1+cantidadDeApariciones (' ') (sinBlancosEnLosBordes (sacarBlancosRepetidos s)) 

--sinBlancosEnLosBordes no hace recursion por lo que no hay caso base, tambien NO ELIMINA TODOS LOS BLANCOS SOLO EL PRIMERO Y EL ULTIMO, SIN IMPORTAR REPETICIONES  
sinBlancosEnLosBordes :: [Char] -> [Char]
sinBlancosEnLosBordes s | head s == ' ' &&  ultimo s == ' ' =  quitar (head s) (quitarUltimo s)
                        | head s == ' ' &&  ultimo s /= ' ' = quitar (head s) s
                        | head s /= ' ' &&  ultimo s == ' ' = quitarUltimo s
                        |otherwise = s

--Requiere: |quitarUltimo|>0
quitarUltimo :: [t] -> [t]
quitarUltimo [x]=[]
quitarUltimo (x:xs) = x: quitarUltimo xs

--Ejercicios 
unionDeCaracteres:: [Char] -> [Char]
unionDeCaracteres [] =[]
unionDeCaracteres (x:xs) | x /= ' ' = x:unionDeCaracteres xs
                    | otherwise = unionDeCaracteres xs

palabra::[Char] -> [[Char]]
palabra s |head(sinBlancosEnLosBordes(sacarBlancosRepetidos s)) /= ' ' = 
