-- <!-- 
-- 1. Priority Queue (cola de prioridad)
-- Ejercicio 2

-- Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
-- menor a mayor utilizando una Heap como estructura auxiliar. ¾Cuál es su costo?


--MAP

-- Implementar como usuario del tipo abstracto Map las siguientes funciones:

-- 1. valuesM :: Eq k => Map k v -> [Maybe v]
-- Propósito: obtiene los valores asociados a cada clave del map.

-- 2. todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- Propósito: indica si en el map se encuentran todas las claves dadas.

-- 3. listToMap :: Eq k => [(k, v)] -> Map k v
-- Propósito: convierte una lista de pares clave valor en un map.

-- 4. mapToList :: Eq k => Map k v -> [(k, v)]
-- Propósito: convierte un map en una lista de pares clave valor.

-- 5. agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
-- la misma clave.

-- 6. incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
-- cada número asociado con dichas claves.

-- 7. mergeMaps:: Eq k => Map k v -> Map k v -> Map k v

-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
-- Indicar los ordenes de complejidad en peor caso de cada función implementada.

-- Ejercicio 4

-- Implemente las siguientes variantes del tipo Map, indicando los costos obtenidos para cada ope-
-- ración:
-- 1. Como una lista de pares-clave valor sin claves repetidas
-- 2. Como una lista de pares-clave valor con claves repetidas
-- 3. Como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está
-- asociada al valor en la misma posición, pero de la otra lista.


-- Ejercicio 5
-- Implemente estas otras funciones como usuario de Map:
-- indexar :: [a] -> Map Int a
-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
-- su posición en la lista.
-- ocurrencias :: String -> Map Char Int
-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
-- en el string, y los valores la cantidad de veces que aparecen en el mismo.
-- Indicar los ordenes de complejidad en peor caso de cada función de la interfaz y del usuario.


-- 1. Implementar el tipo abstracto MultiSet utilizando como representación un Map. Indicar los
-- ordenes de complejidad en peor caso de cada función de la interfaz.

-- 2. Reimplementar como usuario de MultiSet la función ocurrencias de ejercicios anteriores,
-- que dado un string cuenta la cantidad de ocurrencias de cada caracter en el string. En este
-- caso el resultado será un multiconjunto de caracteres.
