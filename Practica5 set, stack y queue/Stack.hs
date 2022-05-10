module Stack 
    (Stack , emptySt , isEmptySt , push , top , pop , lenS )
     where

data Stack a    =   S [a] Int   deriving Show
{- INV REP, int es la longitud de [a] -}

-- 4. Stack (pila)
-- Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa
-- que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
-- platos). Su interfaz es la siguiente:

emptySt :: Stack a
-- Crea una pila vacía.

isEmptySt :: Stack a -> Bool
-- Dada una pila indica si está vacía.

push :: a -> Stack a -> Stack a
-- Dados un elemento y una pila, agrega el elemento a la pila.

top :: Stack a -> a
-- Dada un pila devuelve el elemento del tope de la pila. --Parcial, no puede estar vacia
pop :: Stack a -> Stack a
-- Dada una pila devuelve la pila sin el primer elemento. --Parcial, no puede estar vacia
lenS :: Stack a -> Int
-- Costo: constante.

emptySt              = S [] 0        -- O(1) en S [] 0
isEmptySt (S xs n)   = null xs       -- O(1) en null xs
push a (S xs n)     = S (a:xs) (n+1) -- O(1) en a:xs y n+1
top (S xs n)        = head xs       -- O(1) en head xs
pop (S xs n)        = S (tail xs) (n-1) -- O(1) en tail xs
lenS (S xs n)       =  n            -- O(1) en n
