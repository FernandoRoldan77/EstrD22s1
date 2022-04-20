
-- 1. Tipos recursivos simples

-- 1.1. Celdas con bolitas
-- Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:

data Color = Azul | 
             Rojo deriving Show
data Celda = Bolita Color Celda | 
             CeldaVacia deriving Show

-- En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
-- de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
-- ser la siguiente:
sinBolitas    = CeldaVacia
bolitaEjemplo = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
bolita1       = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
bolita2       = Bolita Rojo CeldaVacia
bolita3       = Bolita Azul (Bolita Azul (Bolita Azul (Bolita Azul CeldaVacia)))

-- Implementar las siguientes funciones sobre celdas:

-- nroBolitas :: Color -> Celda -> Int
-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.

nroBolitas :: Color -> Celda -> Int
nroBolitas colorAContar CeldaVacia        =   0
nroBolitas colorAContar (Bolita color celda)   =   unoSi(esMismoColor colorAContar color) + 
                                                   nroBolitas colorAContar celda


esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _  _      = False

unoSi:: Bool -> Int
unoSi True =  1
unoSi False = 0

-- poner :: Color -> Celda -> Celda
-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner color CeldaVacia               = (Bolita color CeldaVacia)
poner colorAgregado (Bolita c celda) = (Bolita c (Bolita colorAgregado celda))


--sacar :: Color -> Celda -> Celda
-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
-- Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar colorASacar CeldaVacia        = CeldaVacia
sacar colorASacar (Bolita c celda)  = if (esMismoColor colorASacar c)
                                      then celda
                                      else Bolita c (sacar colorASacar celda)


-- ponerN :: Int -> Color -> Celda -> Celda
-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.

ponerN :: Int -> Color -> Celda -> Celda
ponerN n colorAPoner CeldaVacia           = agregarNBolitasEn n colorAPoner
ponerN n colorAPoner (Bolita color celda) = Bolita color (ponerN n colorAPoner celda)

agregarNBolitasEn :: Int -> Color -> Celda
agregarNBolitasEn 0 _     = CeldaVacia
agregarNBolitasEn n color = Bolita color (agregarNBolitasEn(n-1) color)

-- 1.2. Camino hacia el tesoro
-- Tenemos los siguientes tipos de datos

data Objeto = Cacharro | 
              Tesoro  deriving Show
data Camino = Fin | 
              Cofre [Objeto] Camino | 
              Nada Camino deriving Show

caminoConDeTodo = 
  Cofre [Cacharro, Cacharro] (
  Nada (
  Cofre [Cacharro,Tesoro] 
  Fin))

caminoConCacharro = 
  Cofre [Cacharro] (
  Nada (
  Cofre [Cacharro, Cacharro] 
  Fin))

caminoConTesoros = 
  Nada (
  Nada (
  Nada (
  Cofre [Tesoro] 
  Fin)))

caminoCon1Tesoro = 
  Cofre [Tesoro] (
  Nada 
  Fin)

-- Definir las siguientes funciones:

-- hayTesoro :: Camino -> Bool
-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro   Fin                   = False
hayTesoro (Cofre objetos camino)  = (tieneTesoro objetos)  || hayTesoro  camino
hayTesoro(Nada camino)            = hayTesoro camino


tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (t:ts) = (esTesoro t ) || (tieneTesoro ts)

esTesoro :: Objeto -> Bool                        
esTesoro Tesoro   = True
esTesoro  _       = False

-- pasosHastaTesoro :: Camino -> Int
-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
-- Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- Precondición: tiene que haber al menos un tesoro.


pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin                    = 0
pasosHastaTesoro (Cofre objetos camino) =   if (tieneTesoro objetos)
                                            then pasosHastaTesoro Fin
                                            else pasosHastaTesoro camino + 1

pasosHastaTesoro (Nada camino)          =   pasosHastaTesoro camino  + 1 


-- hayTesoroEn :: Int -> Camino -> Bool
-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.

hayTesoroEn :: Int -> Camino  -> Bool
hayTesoroEn   n  Fin         = False
hayTesoroEn   0 camino       = seEncontroTesoro camino
hayTesoroEn   n camino       = hayTesoroEn (n-1) (elCaminoQueSigue camino)

elCaminoQueSigue :: Camino -> Camino
elCaminoQueSigue Fin              = Fin       
elCaminoQueSigue (Cofre _ camino) = camino
elCaminoQueSigue (Nada camino)    = camino

seEncontroTesoro :: Camino -> Bool
seEncontroTesoro Fin                 = False
seEncontroTesoro (Cofre objetos _ )  = tieneTesoro objetos
seEncontroTesoro (Nada camino)       = False


------ intento de solucion  ----
-- alMenosNTesoros :: Int -> Camino -> Bool
-- Indica si hay al menos “n” tesoros en el camino.
-- alMenosNTesoros :: Int -> Camino -> Bool
-- alMenosNTesoros 0 Fin                     = False
-- alMenosNTesoros n (Cofre objetos camino)  = alMenosNTesoros (contarSiHayTesoro n camino) camino
-- alMenosNTesoros n (Nada camino)           = alMenosNTesoros (contarSiHayTesoro (n-1) camino ) camino


-- contarSiHayTesoro :: Int -> Camino -> Int
-- contarSiHayTesoro _  Fin                    = 0
-- contarSiHayTesoro n (Cofre objetos camino)  = if (seEncontroTesoro camino)
--                                               then (contarSiHayTesoro  n + 1) camino
--                                               else (contarSiHayTesoro n   camino)

-- contarSiHayTesoro n (Nada camino)           = contarSiHayTesoro n camino

 -- Indica si hay al menos “n” tesoros en el camino.

-- (desafío) cantTesorosEntre :: Int -> Int -> Camino -> Int
-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
-- el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
-- incolorAContaruidos tanto 3 como 5 en el resultado.

-- 2. Tipos arbóreos

-- 2.1. Árboles binarios
-- Dada esta definición para árboles binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

tree1 :: Tree Int
tree1 = 
    NodeT 10 
      (NodeT 10
            (NodeT 10 EmptyT EmptyT) 
            (NodeT 10 EmptyT EmptyT))
      (NodeT 20 
               (NodeT 20 EmptyT EmptyT)
               (NodeT 20 EmptyT EmptyT))


treeConRamaLarga :: Tree Int
treeConRamaLarga = 
    NodeT 1 
      (NodeT 2
            (NodeT 4 EmptyT EmptyT) 
            (NodeT 5 EmptyT EmptyT))
      (NodeT 3 
               (NodeT 6 EmptyT EmptyT)
               (NodeT 7 EmptyT EmptyT))

-- data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

-- defina las siguientes funciones utilizando recursión estructural según corresponda:

-- 1. sumarT :: Tree Int -> Int
-- Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT EmptyT           = 0
sumarT (NodeT n t1 t2)   =  n + (sumarT t1) + (sumarT t2)

-- 2. sizeT :: Tree a -> Int
-- Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
-- en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT a t1 t2) = 1 + (sizeT t1) + (sizeT t2)  

-- 3. mapDobleT :: Tree Int -> Tree Int
-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2)  = NodeT (n*2) (mapDobleT t1) (mapDobleT t2 )

-- 4. perteneceT :: Eq a => a -> Tree a -> Bool
-- Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
-- árbol.
perteneceT  :: Eq a => a -> Tree a -> Bool
perteneceT e   EmptyT            = False
perteneceT e  (NodeT a t1 t2)   =  (sonMismosElementos e a) || (perteneceT e t1) || (perteneceT e t2) 

sonMismosElementos :: Eq a => a -> a -> Bool
sonMismosElementos a e   = a == e

-- 5. aparicionesT :: Eq a => a -> Tree a -> Int
-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
-- iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT          = 0
aparicionesT a (NodeT n ti td) = ( sumarSiSonIguales a n) + (aparicionesT a ti) + (aparicionesT a td) 

sumarSiSonIguales :: Eq a => a -> a -> Int
sumarSiSonIguales a e   =   unoSi (sonMismosElementos a e) 
                             
-- 6. leaves :: Tree a -> [a]
-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves :: Tree a -> [a]
leaves  EmptyT            = []
leaves (NodeT a ti td)    = [a] ++ (leaves ti) ++ (leaves  td)

-- 7. heightT :: Tree a -> Int
-- Dado un árbol devuelve su altura.
-- Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
-- de niveles del árbol1. La altura para EmptyT es 0, y para una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT            =  0
heightT (NodeT a ti td)   =  1 + (heightT ti) + (heightT td)


-- 8. mirrorT :: Tree a -> Tree a
-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
-- en cada nodo del árbol.

-- 9. toList :: Tree a -> [a]
-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
-- y luego los elementos del hijo derecho.

-- 10. levelN :: Int -> Tree a -> [a]
-- Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
-- nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
-- distancia de la raiz a uno de sus hijos es 1.
-- Nota: El primer nivel de un árbol (su raíz) es 0.

-- 11. listPerLevel :: Tree a -> [[a]]
-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
-- dicho árbol.

-- 12. ramaMasLarga :: Tree a -> [a]
-- Devuelve los elementos de la rama más larga del árbol

-- 13. todosLosCaminos :: Tree a -> [[a]]
-- Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.



-- 2.2. Expresiones Aritméticas
-- El tipo algebraico ExpA modela expresiones aritméticas de la siguiente manera:
data ExpA = Valor Int      | 
            Sum ExpA ExpA  | 
            Prod ExpA ExpA | 
            Neg ExpA  deriving Show

--Implementar las siguientes funciones utilizando el esquema de recursión estructural sobre Exp:
-- 1. eval :: ExpA -> -> Int
-- Dada una expresión aritmética devuelve el resultado evaluarla.

-- 2. simplificar :: ExpA -> ExpA
-- Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
-- notación matemática convencional):

-- a) 0 + x = x + 0 = x
-- b) 0 * x = x * 0 = 0
-- c) 1 * x = x * 1 = x
-- d) - (- x) = x

-- 1También existen otras definiciones posibles. Por ejemplo, puede definirse como la distancia del camino desde la
-- raíz a su hoja más lejana. Por distancia entendemos la cantidad de nodos que hay en dicho camino. En este caso
-- las hojas tendrían altura 0, porque la distancia del camino a sí mismos lo es. Se suele utilizar más en árboles que
-- no poseen un constructor vacío.
