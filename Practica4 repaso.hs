
-- 1. Pizzas
-- Tenemos los siguientes tipos de datos:
data Pizza = Prepizza |
             Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa |
                   Queso |
                   Jamon |
                   Aceitunas Int deriving Show

pizza0 = Prepizza
pizza1 = Capa Jamon Prepizza
pizza2 = Capa Jamon (Capa  (Aceitunas 77) Prepizza )
pizza3 = Capa Salsa (Capa Salsa (Capa Queso Prepizza))
pizza4 = Capa Jamon pizza3


-- Definir las siguientes funciones:
-- cantidadDeCapas :: Pizza -> Int
-- Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza        = 0
cantidadDeCapas (Capa ing p)    = 1 + cantidadDeCapas p

-- armarPizza :: [Ingrediente] -> Pizza
-- Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza      []    =  Prepizza
armarPizza  (i:is)    =  (Capa i (armarPizza is) )

-- sacarJamon :: Pizza -> Pizza
-- Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza       = Prepizza
sacarJamon (Capa ing p)   =	if(esJamon ing)
						    then p
						    else (Capa ing (sacarJamon p))   


esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

-- tieneSoloSalsaYQueso :: Pizza -> Bool
-- Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza       =   True
tieneSoloSalsaYQueso (Capa ing p)   =   soloSalsaYQueso ing && tieneSoloSalsaYQueso p

soloSalsaYQueso :: Ingrediente -> Bool
soloSalsaYQueso Salsa   = True
soloSalsaYQueso Queso   = True
soloSalsaYQueso _       = False

-- duplicarAceitunas :: Pizza -> Pizza
-- Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza      =   Prepizza
duplicarAceitunas (Capa ing p)  =   (Capa (duplicarSiSonAceitunas ing) (duplicarAceitunas p))

duplicarSiSonAceitunas :: Ingrediente -> Ingrediente
duplicarSiSonAceitunas (Aceitunas n)    = (Aceitunas  (n * 2))
duplicarSiSonAceitunas      ing         = ing


cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (pizza:pizzas) = ( cantidadDeCapas pizza , pizza ) : cantCapasPorPizza pizzas 

-- 2. Mapa de tesoros (con bifurcaciones)
-- Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
-- cada cofre tiene un objeto, que puede ser chatarra o un tesoro.

data Dir    = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre  = Cofre [Objeto] deriving Show

data Mapa   = Fin Cofre | 
              Bifurcacion Cofre Mapa Mapa deriving Show

tesoro   = Tesoro
chatarra = Chatarra
objetosConUnoDeCada  = [tesoro,chatarra]
objetosConTesoros    = [tesoro,tesoro]
objetosConChatarra   = [chatarra,chatarra]
cofreVariado         = (Cofre objetosConUnoDeCada)
cofreSoloTesoros     = (Cofre objetosConTesoros)
cofreConChatarra     = (Cofre objetosConChatarra)
cofreVacio           = (Cofre [])
mapa1                =  Fin cofreVacio
mapa2                = (Bifurcacion cofreVariado mapa1 mapa1)
mapa3                = (Bifurcacion cofreSoloTesoros mapa1 mapa2)
mapa4                = (Bifurcacion cofreVacio mapa2 mapa3)
mapa5                = (Bifurcacion cofreConChatarra mapa4 mapa4)

mapa6                = (Bifurcacion cofreConChatarra mapa3 mapa3)


-- Definir las siguientes operaciones:
-- 1. hayTesoro :: Mapa -> Bool
-- Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro  (Fin  cofre )                    =   False
hayTesoro (Bifurcacion cofre mapa1 mapa2)   =   tieneTesoro cofre || hayTesoro mapa1 || hayTesoro mapa2

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre objetos)     =   hayAlgunTesoro objetos

hayAlgunTesoro :: [Objeto] -> Bool
hayAlgunTesoro []           = False
hayAlgunTesoro (o:os)       = if esTesoro o
                              then True
                              else hayAlgunTesoro os

esTesoro :: Objeto -> Bool
esTesoro Tesoro     =   True
esTesoro _          =   False


-- 2. hayTesoroEn :: [Dir] -> Mapa -> Bool
-- Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
-- lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn direcciones (Fin cofre)                = error "Debe existir la direccion"
hayTesoroEn []          mapa                       =  hayTesoro mapa   
hayTesoroEn (d:ds) (Bifurcacion cofre mapa1 mapa2) =  if (hayTesoroEnLaIzquierda d)
                                                      then hayTesoroEn ds mapa1
                                                      else hayTesoroEn ds mapa2
hayTesoroEnLaIzquierda :: Dir -> Bool
hayTesoroEnLaIzquierda Izq  = True
hayTesoroEnLaIzquierda _    = False


--Indica el camino al tesoro. Precondición: existe un tesoro y es único.

caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofre)                   = error "debe existir un tesoro y es unico"
caminoAlTesoro (Bifurcacion cofre map1 map2) = if (tieneTesoro cofre)
                                                then []
                                                else elegirElMapaQueConElCaminoAlTesoro map1  map2

elegirElMapaQueConElCaminoAlTesoro::  Mapa -> Mapa -> [Dir]
elegirElMapaQueConElCaminoAlTesoro mapa1 mapa2     = if(hayTesoro mapa1)
                                                     then Izq : caminoAlTesoro mapa1
                                                     else Der : caminoAlTesoro mapa2

-- 4. caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin cofre)                      = []
caminoDeLaRamaMasLarga (Bifurcacion cofre mapa1 mapa2)  = elegirCaminoMasLargo (caminoDeLaRamaMasLarga mapa1) (caminoDeLaRamaMasLarga mapa2)

elegirCaminoMasLargo :: [Dir] -> [Dir] -> [Dir]
elegirCaminoMasLargo  direcciones1 direcciones2 =   if (length direcciones1 > length direcciones2)
                                                    then Izq : direcciones1
                                                    else Der : direcciones2

singularSi x True  = [x]
singularSi _ False = []

-- consACada :: a -> [[a]] -> [[a]]
-- consACada x []       = []
-- consACada x (xs:xss) = (x:xs) : consACada x xss

-- 5. tesorosPorNivel :: Mapa -> [[Objeto]]
-- Devuelve los tesoros separados por nivel en el árbol.

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin cofre)     = []  
tesorosPorNivel (Bifurcacion cofre mapa1 mapa2) =  (buscarTesorosEn cofre) :  juntarTesorosPorNivel (tesorosPorNivel mapa1 ) 
                                                                                                    (tesorosPorNivel mapa2)  

buscarTesorosEn :: Cofre -> [Objeto]
buscarTesorosEn (Cofre objetos)  = devolverTesoros objetos

devolverTesoros:: [Objeto] -> [Objeto]
devolverTesoros []        = []
devolverTesoros (o:os)    = if(esTesoro o)
                            then o: devolverTesoros os
                            else devolverTesoros os

juntarTesorosPorNivel:: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
juntarTesorosPorNivel []  obs2              = obs2
juntarTesorosPorNivel obs1  []              = obs1
juntarTesorosPorNivel (o1:obs1) (o2:obs2)   = (o1 ++ o2) : juntarTesorosPorNivel obs1 obs2


-- 6. todosLosCaminos :: Mapa -> [[Dir]]
-- Devuelve todos lo caminos en el mapa.
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _ )                    = [[]]
todosLosCaminos (Bifurcacion _ mapa1 mapa2) =   consACada Izq (todosLosCaminos mapa1) ++
                                                consACada Der (todosLosCaminos mapa2)

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss


-- 3. Nave Espacial
-- modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
-- dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
-- es la siguiente:

data Componente = LanzaTorpedos | 
                  Motor Int     | 
                  Almacen [Barril] deriving Show
data Barril = Comida  | 
              Oxigeno | 
              Torpedo | 
              Combustible deriving Show

data Sector = S SectorId [Componente] [Tripulante] deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | 
              NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show

componente1 = (Almacen [Comida,Oxigeno,Combustible])
componente2 = (Motor 10)
componente3 = (Almacen [Comida, Oxigeno, Combustible, Torpedo])

sector1  = (S "Midgar"[componente3]["Cloud, Barret"])
sector2 = (S "Kalm"[componente2]["Tifa, Red"])
sectorT1 = (EmptyT)
sectorT2 = (NodeT sector1 sectorT1 sectorT1)
sectorT3 = (NodeT sector2 sectorT1 sectorT1)
sectorT4 = (NodeT sector2 sectorT3 sectorT3)

nave1    = N(sectorT2)
nave2    = N(sectorT4)

-- Implementar las siguientes funciones utilizando recursión estructural:

-- 1. sectores :: Nave -> [SectorId]
-- Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores (N sectorT)    =  devolverSectores ( sectorT)

devolverSectores :: Tree Sector -> [SectorId]
devolverSectores EmptyT              =  [] 
devolverSectores (NodeT s ti td)     = idSector s  : (devolverSectores ti) ++ 
                                                     (devolverSectores td)
idSector:: Sector  -> String
idSector (S id _ _) = id


-- 2. poderDePropulsion :: Nave -> Int
-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
-- el poder de propulsión es el número que acompaña al constructor de motores.

poderDePropulsion :: Nave -> Int
poderDePropulsion (N sectorT)   =   calcularPropulsion sectorT

calcularPropulsion :: Tree Sector -> Int
calcularPropulsion EmptyT  = 0
calcularPropulsion (NodeT sector ti td) = propulsionDeSector sector + (calcularPropulsion ti) + (calcularPropulsion td)

propulsionDeSector :: Sector -> Int
propulsionDeSector (S _ componentes _)  = contarPropulsion componentes

contarPropulsion :: [Componente] -> Int
contarPropulsion []     =   0
contarPropulsion (c:cs) = sumarPoderSiEsMotor c + contarPropulsion cs

sumarPoderSiEsMotor :: Componente  -> Int
sumarPoderSiEsMotor (Motor poder)  = poder
sumarPoderSiEsMotor _          = 0


-- 3. barriles :: Nave -> [Barril]
-- Propósito: Devuelve todos los barriles de la nave.
barriles:: Nave -> [Barril]
barriles (N sectorT)        = devolverBarriles sectorT

devolverBarriles :: Tree Sector -> [Barril]
devolverBarriles EmptyT               =   []
devolverBarriles (NodeT sector ti td) = buscarBarriles sector ++ 
                                        (devolverBarriles ti) ++ 
                                        (devolverBarriles td)
buscarBarriles:: Sector -> [Barril]
buscarBarriles (S _ componentes _)  = filtrarPorBarril componentes

filtrarPorBarril :: [Componente]    -> [Barril]
filtrarPorBarril []        = []
filtrarPorBarril (c:cs)    = agregarSiEsBarril c ++ filtrarPorBarril cs

agregarSiEsBarril :: Componente -> [Barril]
agregarSiEsBarril (Almacen barriles) = barriles
agregarSiEsBarril _                  = []

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs  sectorId (N sectorT)  =   N (agregarComponentesA cs sectorId sectorT)

agregarComponentesA :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarComponentesA    cs sectorId  EmptyT               = EmptyT
agregarComponentesA    cs sectorId (NodeT sector ti td)  = NodeT (agregarComponenteASectorSi cs sectorId sector) (agregarComponentesA cs sectorId ti) 
                                                                                                        (agregarComponentesA cs sectorId td)

agregarComponenteASectorSi :: [Componente] -> SectorId -> Sector -> Sector
agregarComponenteASectorSi cs sectorId (S id componentes tripulantes ) = if (esMismoSector id sectorId)
                                                                         then (S id (cs ++ componentes) tripulantes)
                                                                         else (S id componentes tripulantes)

-- 5. asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA tripulante sectoresId (N sectorT) = N (agregarTripulanteA tripulante sectoresId sectorT)

agregarTripulanteA :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
agregarTripulanteA  t sectoresId EmptyT               = EmptyT
agregarTripulanteA  t sectoresId (NodeT sector ti td) = NodeT(agregarTripulantesASectores t sectoresId sector) (agregarTripulanteA t sectoresId ti) 
                                                                                                               (agregarTripulanteA t sectoresId td)

agregarTripulantesASectores :: Tripulante -> [SectorId] -> Sector -> Sector
agregarTripulantesASectores t sectoresId (S id componentes tripulantes)   =  (S id componentes (agregarTripulanteAUnSector t sectoresId id))

agregarTripulanteAUnSector :: Tripulante -> [SectorId] -> SectorId  -> [Tripulante]
agregarTripulanteAUnSector  t [] sectorId        = []
agregarTripulanteAUnSector  t (s:ss) sectorId    = if (esMismoSector s sectorId)
                                                   then t :(agregarTripulanteAUnSector t ss sectorId)
                                                   else (agregarTripulanteAUnSector t ss sectorId)

esMismoSector :: SectorId -> SectorId -> Bool
esMismoSector s1 s2     =  s1 == s2


-- 6. sectoresAsignados :: Tripulante -> Nave -> [SectorId]
-- Propósito: Devuelve los sectores en donde aparece un tripulante dado.

-- 7. tripulantes :: Nave -> [Tripulante]
-- Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]
tripulantes (N sectorT)     = tripulantesSinRepetidos( buscarTripulantes sectorT)

buscarTripulantes:: Tree Sector -> [Tripulante]
buscarTripulantes EmptyT               =   []
buscarTripulantes (NodeT sector ti td) =  tripulantesDelSector sector ++ 
                                          (buscarTripulantes ti) ++ 
                                          (buscarTripulantes td)

tripulantesDelSector :: Sector -> [Tripulante]
tripulantesDelSector (S _ _ tripulantes)    =  tripulantesSinRepetidos tripulantes


tripulantesSinRepetidos :: [Tripulante] -> [Tripulante]
tripulantesSinRepetidos []     = []
tripulantesSinRepetidos (t:ts) = agregarSiHaceFalta t (tripulantesSinRepetidos ts)

agregarSiHaceFalta :: Tripulante -> [Tripulante] -> [Tripulante]
agregarSiHaceFalta t ts = if (pertenece t ts)
                             then ts
                             else t:ts

pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = (a == x) || pertenece a xs


-- 4. Manada de lobos
-- Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto
-- de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
-- Los diferentes casos de lobos que forman la jerarquía son los siguientes:
-- Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
-- Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres de
-- bosques, ríos, etc.), y poseen 2 lobos a cargo.
-- Las crías poseen sólo un nombre y no poseen lobos a cargo.
-- La estructura es la siguiente:

-- type Presa = String -- nombre de presa
-- type Territorio = String -- nombre de territorio
-- type Nombre = String -- nombre de lobo
-- data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
-- | Explorador Nombre [Territorio] Lobo Lobo
-- | Cría Nombre

-- data Manada = M Lobo
-- 1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
-- crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
-- que corresponda en cada caso:

-- 2. buenaCaza :: Manada -> Bool
-- Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad
-- de crías.

-- 3. elAlfa :: Manada -> (Nombre, Int)
-- Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
-- con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
-- cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
-- cero presas.

-- 4. losQueExploraron :: Territorio -> Manada -> [Nombre]
-- Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
-- pasaron por dicho territorio.

-- 5. exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
-- Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio
-- y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
-- dicho territorio. Los territorios no deben repetirse.

-- 6. superioresDelCazador :: Nombre -> Manada -> [Nombre]
-- Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los
-- cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
-- Precondición: hay un cazador con dicho nombre y es único.
