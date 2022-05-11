<!-- Ejercicio 1

La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.
Implementarla usando listas, e indicando el costo de cada operación.

emptyPQ :: PriorityQueue a
Propósito: devuelve una priority queue vacía.

isEmptyPQ :: PriorityQueue a -> Bool
Propósito: indica si la priority queue está vacía.

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
Propósito: inserta un elemento en la priority queue.

findMinPQ :: Ord a => PriorityQueue a -> a
Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
Precondición: parcial en caso de priority queue vacía.

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
Precondición: parcial en caso de priority queue vacía. -->
