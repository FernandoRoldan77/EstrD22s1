module Set
  (Set,emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
 where

data Set a = Set [a]  Int 
 {- INV.REP: *[a] no tiene repetidos
            *Int es la cantidad de elementos de [a]
    -}

-- 2. Set (conjunto)
-- Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

-- emptyS :: Set a
-- Crea un conjunto vacío.
emptyS :: Set a --Constante
emptyS = Set [] 0  

-- addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.

addS :: Eq a => a -> Set a -> Set a     --Lineal
addS x (Set listaS n)  =  Set (agregarSi x listaS) (aumentarSi x n listaS)
    
    
agregarSi :: Eq a => a -> [a]   -> [a] --constante
--Pro: Agregar un elemento a la lista solo si no es repetido
agregarSi x []      =   [x]
agregarSi x (y:ys)  =   if x == y
                          then y : ys
                          else x : agregarSi y ys

aumentarSi :: Eq a => a -> Int -> [a] -> Int --lineal  ^
--Pro: Incrementa la cantidad de elementos de la lista si se agrega uno nuevo
aumentarSi a n xs  = if elem a xs
                      then n 
                      else (n+1)
                           
-- belongs :: Eq a => a -> Set a -> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

belongs :: Eq a => a -> Set a -> Bool -- constante
belongs x (Set listaS n) =  elem x listaS

-- sizeS :: Eq a => Set a -> Int
-- Devuelve la cantidad de elementos distintos de un conjunto.

sizeS :: Eq a => Set a -> Int -- constante
sizeS (Set listS n)  = n

-- removeS :: Eq a => a -> Set a -> Set a
-- Borra un elemento del conjunto.
removeS :: Eq a => a -> Set a -> Set a  -- lineal
removeS x (Set listS n) = if elem x listS -- lineal
                            then (Set (removerA x listS) (n-1)) -- lineal
                            else (Set listS n)

removerA :: Eq a => a -> [a] -> [a] --Lineal
removerA x (a:as)   =  if x == a
                        then as
                        else a : removerA x as 

sacar :: Eq a => a -> [a] -> [a] -- Lineal
sacar n [] = []
sacar n (x:xs) = if n == x          -- constante
                 then xs
                 else x : sacar n xs

-- unionS :: Eq a => Set a -> Set a -> Set a
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.

unionS :: Eq a => Set a -> Set a -> Set a -- cuadratica
unionS (Set listS1 _) (Set listS2 _)    =   let listaDeSet1Y2 = concatenar listS1 listS2
                                              in (Set listaDeSet1Y2 (length listaDeSet1Y2))

concatenar :: [a] -> [a] -> [a] -- lineal
concatenar [] ys           =   ys
concatenar (x:xs) ys  = x : concatenar xs ys -- lineal


-- setToList :: Eq a => Set a -> [a]
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a] -- constante
setToList (Set listS n) = listS


-- 1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
-- la cantidad de elementos en la estructura.

-- Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
-- de esta implementación, pero para mantener una interfaz común entre distintas posibles
-- implementaciones estamos obligados a escribir así los tipos.