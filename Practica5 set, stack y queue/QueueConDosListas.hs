module QueueConDosListas
    (QueueConDosListas, emptyQ, isEmptyQ, queue, firstQ, dequeue)
    where

 data QueueConDosListas a = Q [a] [a] {- INV REP: la primer lista se llama fs(front stack) y si se encuentra
         vacía, entonces la cola se encuentra vacía, la la segunda se llama bs(back stack)
    -}

-- 5. Queue con dos listas
-- Implemente la interfaz de Queue pero en lugar de una lista utilice dos listas. Esto permitirá
-- que todas las operaciones sean constantes (aunque alguna/s de forma amortizada).    
-- La estructura funciona de la siguiente manera. Llamemos a una de las listas fs (front stack) y
-- a la otra bs (back stack). Quitaremos elementos a través de fs y agregaremos a través de bs, pero
-- todas las operaciones deben garantizar el siguiente invariante de representación: Si fs se encuentra
-- vacía, entonces la cola se encuentra vacía.
-- ¿Qué ventaja tiene esta representación de Queue con respecto a la que usa una sola lista?

--La ventaja es que al tener 2 listas para agregar y sacar elementos, 
--se hace mas eficiente porque pasa a ser constante.



 emptyQ :: QueueConDosListas a -- Crea una cola vacía.
 isEmptyQ :: QueueConDosListas a -> Bool-- Dada una cola indica si la cola está vacía.
 queue :: a -> QueueConDosListas a -> QueueConDosListas a -- Dados un elemento y una cola, agrega ese elemento a la cola.
 firstQ :: QueueConDosListas a -> a -- Dada una cola devuelve el primer elemento de la cola.   
 dequeue :: QueueConDosListas a -> QueueConDosListas a -- Dada una cola la devuelve sin su primer elemento.



 emptyQ                 = (Q [] [] )           -- O(1)
 isEmptyQ (Q [] ys)     =  True
 isEmptyQ _             = False -- 0(1)
 queue a (Q [] ys)      =  Q [a] ys       -- 0(1)
 queue a (Q xs ys)      =  Q xs (a : ys)
 firstQ (Q xs  ys)      =  head xs           -- 0(1) 
 dequeue (Q (x:[]) ys)  = Q (reverse ys) []       -- 0(1)
 dequeue (Q xs ys)      = Q (tail xs) ys
    



