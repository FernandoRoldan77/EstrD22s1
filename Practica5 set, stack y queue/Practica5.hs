import Set
--import SetConRepetidos
import Queue
--import QueueV2
import Stack
-- Trabajo Práctico # 5 - Set, Stack y Queue

-- 1. Cálculo de costos
-- Especificar el costo operacional de las siguientes funciones:
head :: [a] -> a -- Constante
head (x:xs) = x

sumar :: Int -> Int -- Lineal
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

factorial :: Int -> Int -- Lineal
factorial 0 = 1
factorial n = n * factorial (n-1)


longitud :: [a] -> Int  -- Lineal
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

factoriales :: [Int] -> [Int] -- Cuadratica
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs  -- Lineal

pertenece :: Eq a => a -> [a] -> Bool  -- Lineal
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- sinRepetidos :: Eq a => [a] -> [a] --Cuadratico
-- sinRepetidos [] = []
-- sinRepetidos (x:xs) = if pertenece x xs        --Lineal
--                       then sinRepetidos xs
--                       else x : sinRepetidos xs


-- equivalente a (++)
append :: [a] -> [a] -> [a]      -- Constante
append [] ys = ys
append (x:xs) ys = x : append xs ys

concatenar :: [String] -> String  -- Constante
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

takeN :: Int -> [a] -> [a] --Lineal
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs --constante

dropN :: Int -> [a] -> [a] --Lineal
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs --constante

partir :: Int -> [a] -> ([a], [a]) -- cuadratica
partir n xs = (takeN n xs, dropN n xs) --constantes

minimo :: Ord a => [a] -> a -- lineal
minimo [x] = x
minimo (x:xs) = min x (minimo xs) --constante

sacar :: Eq a => a -> [a] -> [a] -- Lineal
sacar n [] = []
sacar n (x:xs) = if n == x          -- constante
                 then xs
                 else x : sacar n xs

ordenar :: Ord a => [a] -> [a] -- Cuadratica
ordenar [] = []
ordenar xs = let m = minimo xs  --lineal
             in m : ordenar (sacar m xs)


-- 1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
-- la cantidad de elementos en la estructura.

-- 2. Como usuario del tipo abstracto Set implementar las siguientes funciones:
set1 = losQuePertenecen [2,1] (addS 1 (addS 2 (addS 3 (emptyS))))

-- losQuePertenecen :: Eq a => [a] -> Set a -> [a]
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto.

losQuePertenecen :: Eq a => [a] -> Set a -> [a]  -- Lineal
losQuePertenecen  []     set   =   []
losQuePertenecen  (x:xs) set   =   if (belongs x set) -- constante
                                    then x : (losQuePertenecen xs set)
                                    else losQuePertenecen xs set

--sinRepetidos :: Eq a => [a] -> [a]
-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura
-- auxiliar.
sinRepetidos :: Eq a => [a] -> [a] -- lineal
sinRepetidos    xs  =  setToList (pasarListaAUnConjunto xs ) --constante

pasarListaAUnConjunto :: Eq a => [a] -> Set a -- lineal
pasarListaAUnConjunto  []     = emptyS
pasarListaAUnConjunto (x:xs)  = addS x (pasarListaAUnConjunto xs)

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- unirTodos :: Eq a => Tree (Set a) -> Set a
-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a -- cuadratica
unirTodos EmptyT              =   emptyS
unirTodos (NodeT a setI setD) =  unionS (unionS a (unirTodos setI))  (unionS a (unirTodos setD)) 


-- 3. Como usuario del tipo abstracto Queue implementar las siguientes funciones:

-- lengthQ :: Queue a -> Int
-- Cuenta la cantidad de elementos de la cola.
lengthQ :: Queue a -> Int --0(1) en  isEmptyQ y lenghtQ queue +1
lengthQ queue    =   if isEmptyQ queue
                      then 0
                      else 1 + lengthQ (dequeue queue)

-- queueToList :: Queue a -> [a]
-- Dada una cola devuelve la lista con los mismos elementos,
-- donde el orden de la lista es el de la cola.
-- Nota: chequear que los elementos queden en el orden correcto.

queueToList :: Queue a -> [a] -- O(n) 
queueToList queue   = error "No debe estar vacia"
queueToList queue   = [firstQ queue] ++ queueToList queue

-- unionQ :: Queue a -> Queue a -> Queue a
-- Inserta todos los elementos de la segunda cola en la primera.
unionQ :: Queue a -> Queue a -> Queue a -- 0(n^2)
unionQ  q1 q2   =   emptyQ
unionQ  q1 q2   =   unionQ  (queue (firstQ q2) q1)  q2  --queue O(n) first O(1)

--STACK

-- 1. Como usuario del tipo abstracto Stack implementar las siguientes funciones:
-- apilar :: [a] -> Stack a

apilar :: [a] -> Stack a  -- O(n) siendo n (x:xs)  
apilar []       =   emptySt
apilar (x:xs)   =   push x (apilar xs)


-- Dada una lista devuelve una pila sin alterar el orden de los elementos.

-- desapilar :: Stack a -> [a]
-- Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]  -- O(n) siendo  top stack n
desapilar stack =   if isEmptySt stack --O(1)
                     then []
                     else top stack : desapilar (pop stack) -- O(n)

-- insertarEnPos :: Int -> a -> Stack a -> Stack a
-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
-- posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a -- 0()
insertarEnPos 0 elemento stack = push elemento stack
insertarEnPos n elemento stack = push (top stack) (insertarEnPos (n-1) elemento (pop stack))                            

stack1 = push 30( push 20( push 10 emptySt)) --stack para pruebas


