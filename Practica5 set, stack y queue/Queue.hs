module Queue
    (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue)
    where

 data Queue a = Q [a] 

 {- INV REP: (Q xs n) No hay ningun invariante de representacion
    -}

-- 3. Queue (cola)
-- Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa
-- que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
-- primero en salir (como la cola de un banco). Su interfaz es la siguiente:

 emptyQ :: Queue a -- Crea una cola vacía.
 isEmptyQ :: Queue a -> Bool-- Dada una cola indica si la cola está vacía.
 queue :: a -> Queue a -> Queue a -- Dados un elemento y una cola, agrega ese elemento a la cola.
 firstQ :: Queue a -> a -- Dada una cola devuelve el primer elemento de la cola.   --PARCIAL, la cola no puede ser vacia
 dequeue :: Queue a -> Queue a -- Dada una cola la devuelve sin su primer elemento.--PARCIAL, la cola no puede ser vacia


-- 1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el
-- final de la lista y desencolarse por delante.

 emptyQ            = (Q []  )           -- O(1)
 isEmptyQ (Q xs )  =  null xs            -- 0(1)
 queue a (Q xs )   =  Q (xs++[a])       -- 0(n)
 firstQ (Q xs )    =  head xs           -- 0(1) 
 dequeue (Q xs )   = Q (tail xs)        -- 0(1)
 

