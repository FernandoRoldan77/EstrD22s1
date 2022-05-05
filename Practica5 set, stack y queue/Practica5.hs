import Set
--import SetConRepetidos
import Queue
--import QueueV2
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
lengthQ :: Queue a -> Int --0(1)
lengthQ queue    =   if isEmptyQ queue
                     then 0
                     else lengthQ queue + 1

-- queueToList :: Queue a -> [a]
-- Dada una cola devuelve la lista con los mismos elementos,
-- donde el orden de la lista es el de la cola.
-- Nota: chequear que los elementos queden en el orden correcto.
queueToList :: Queue a -> [a] --
queueToList queue   = error "No debe estar vacia"
queueToList queue   = [firstQ queue] ++ queueToList queue

-- unionQ :: Queue a -> Queue a -> Queue a
-- Inserta todos los elementos de la segunda cola en la primera.
unionQ :: Queue a -> Queue a -> Queue a -- 0(n^2)
unionQ  q1 q2   =   emptyQ
unionQ  q1 q2   =   unionQ  (queue (firstQ q2) q1)  q2  --queue O(n) first O(1)

-- 4. Stack (pila)
-- Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa
-- que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
-- platos). Su interfaz es la siguiente:

-- emptyS :: Stack a
-- Crea una pila vacía.

-- isEmptyS :: Stack a -> Bool
-- Dada una pila indica si está vacía.

-- push :: a -> Stack a -> Stack a
-- Dados un elemento y una pila, agrega el elemento a la pila.

-- top :: Stack a -> a
-- Dada un pila devuelve el elemento del tope de la pila.
-- pop :: Stack a -> Stack a

-- Dada una pila devuelve la pila sin el primer elemento.
-- lenS :: Stack a -> Int

-- Dada una pila devuelve la pila sin el primer elemento.
-- Costo: constante.

-- 1. Como usuario del tipo abstracto Stack implementar las siguientes funciones:
-- apilar :: [a] -> Stack a
-- Dada una lista devuelve una pila sin alterar el orden de los elementos.

-- desapilar :: Stack a -> [a]
-- Dada una pila devuelve una lista sin alterar el orden de los elementos.

-- insertarEnPos :: Int -> a -> Stack a -> Stack a
-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
-- posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).

-- 2. Implementar el tipo abstracto Stack utilizando una lista.

-- 5. Queue con dos listas
-- Implemente la interfaz de Queue pero en lugar de una lista utilice dos listas. Esto permitirá
-- que todas las operaciones sean constantes (aunque alguna/s de forma amortizada).
-- La estructura funciona de la siguiente manera. Llamemos a una de las listas fs (front stack) y
-- a la otra bs (back stack). Quitaremos elementos a través de fs y agregaremos a través de bs, pero
-- todas las operaciones deben garantizar el siguiente invariante de representación: Si fs se encuentra
-- vacía, entonces la cola se encuentra vacía.
-- ¿Qué ventaja tiene esta representación de Queue con respecto a la que usa una sola lista?
