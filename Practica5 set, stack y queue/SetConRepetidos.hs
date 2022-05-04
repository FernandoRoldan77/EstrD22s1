module SetConRepetidos
  (SetR,emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
 where

--  Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
-- otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
-- sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
-- por ejemplo). Contrastar la eficiencia obtenida en esta implementación con la anterior.

data SetR a = Set [a] Int   
 {- INV.REP: *[a] pueden ser repetidos
            *Int es la cantidad de todos los elementos de [a]
    -}

-- 2. Set (conjunto)
-- Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

-- emptyS :: Set a
-- Crea un conjunto vacío.
emptyS :: SetR a --Constante
emptyS = Set [] 0  

-- addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.

addS :: Eq a => a -> SetR a -> SetR a     --Lineal
addS x (Set listaS n)  =  Set (x:xs) (aumentarSi x n listaS)
    
    
aumentarSi :: Eq a => a -> Int -> [a] -> Int --lineal  ^
--Pro: Incrementa la cantidad de elementos de la lista si se agrega uno nuevo
aumentarSi a n xs  = if elem a xs
                      then n 
                      else (n+1)
                           
-- belongs :: Eq a => a -> Set a -> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

belongs :: Eq a => a -> SetR a -> Bool -- constante
belongs x (Set listaS n) =  elem x listaS

-- sizeS :: Eq a => Set a -> Int
-- Devuelve la cantidad de elementos distintos de un conjunto.

sizeS :: Eq a => SetR a -> Int -- constante
sizeS (Set listS n)  = n

-- removeS :: Eq a => a -> Set a -> Set a
-- Borra un elemento del conjunto.
removeS :: Eq a => a -> SetR a -> SetR a  -- lineal
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

unionS :: Eq a => SetR a -> SetR a -> SetR a -- cuadratica
unionS (Set listS1 _) (Set listS2 _)    =   let listaDeSet1Y2 = concatenar listS1 listS2
                                              in (Set listaDeSet1Y2 (length listaDeSet1Y2))

concatenar :: [a] -> [a] -> [a] -- lineal
concatenar [] ys           =   ys
concatenar (x:xs) ys  = sinRepetidos (x : concatenar xs ys) -- lineal


-- setToList :: Eq a => Set a -> [a]
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => SetR a -> [a] -- Cuadratico
setToList (Set listS n) = sinRepetidos listS -- cuadratico



sinRepetidos :: Eq a => [a] -> [a] --Cuadratico auxiliar de la primera parte de la pract 5
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs        --Lineal
                      then sinRepetidos xs
                      else x : sinRepetidos xs

