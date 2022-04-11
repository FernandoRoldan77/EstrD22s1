
-- 1. Recursión sobre listas
-- Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique
-- lo contrario:

-- 1. sumatoria :: [Int] -> Int
-- Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
sumatoria []     =   0
sumatoria (n:ns) =  n + sumatoria ns

-- 2. longitud :: [a] -> Int
-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
-- de elementos que posee.
longitud :: [a] -> Int
longitud []         = 0
longitud (n:ns)     = 1 + longitud ns

-- 3. sucesores :: [Int] -> [Int]
-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) = n+1 : sucesores ns

-- 4. conjuncion :: [Bool] -> Bool
-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool
conjuncion []   = True
conjuncion (b:bs) = b && conjuncion bs


-- 5. disyuncion :: [Bool] -> Bool
-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion []       =   False
disyuncion (b:bs)   =   b || disyuncion bs


-- 6. aplanar :: [[a]] -> [a]
-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar []     =   []
aplanar (a:as) =    a ++ aplanar as

-- 7. pertenece :: Eq a => a -> [a] -> Bool
-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
-- a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = (a == x) || pertenece a xs


-- 8. apariciones :: Eq a => a -> [a] -> Int
-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones a  []   = 0
apariciones a (x:xs)    = if(a == x)
                          then apariciones a xs + 1
                          else apariciones a xs


-- 9. losMenoresA :: Int -> [Int] -> [Int]
-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []        = []
losMenoresA n (x:xs)    =   if(x < n )
                            then x : losMenoresA n xs
                            else losMenoresA n xs
                        
-- 10. lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
-- de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []        = []
lasDeLongitudMayorA n (xs:xss)  = if longitud xs > n
                                 then xs : lasDeLongitudMayorA n xss
                                 else lasDeLongitudMayorA n xss

-- 11. agregarAlFinal :: [a] -> a -> [a]
-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
-- lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e         =   [e]
agregarAlFinal (x:xs) e     = x : agregarAlFinal xs e


-- 12. concatenar :: [a] -> [a] -> [a]
-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
-- elementos de la segunda a continuación. Definida en Haskell como ++.
concatenar :: [a] -> [a] -> [a]
concatenar [] ys           =   ys
concatenar (x:xs) ys  = x : concatenar xs ys

-- 13. reversa :: [a] -> [a]
-- Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
-- en Haskell como reverse.
reversa :: [a] -> [a]
reversa []      =   []
reversa (x:xs)  =   reversa xs ++ [x]

-- 14. zipMaximos :: [Int] -> [Int] -> [Int]
-- Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
-- máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
-- las listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos  ns1  []         = ns1
zipMaximos  []  ns2         = ns2
zipMaximos  (x:xs)  (y:ys)  = if(x >= y)
                                then x : zipMaximos xs ys
                                else y : zipMaximos xs ys


-- 15. elMinimo :: Ord a => [a] -> a
-- Dada una lista devuelve el mínimo
--PreCondicion: La lista no debe estar vacia. 
elMinimo :: Ord a => [a] -> a

elMinimo [x]    = x
elMinimo (x:xs) =  minimo x (elMinimo xs)

minimo :: Ord a => a -> a -> a
minimo x y = if x < y
                then x
                else y


-- 2. Recursión sobre números
-- Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
-- lo contrario:

-- 1. factorial :: Int -> Int
-- Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
-- llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int
factorial 0   =   1
factorial n    = n * factorial (n-1)

-- 2. cuentaRegresiva :: Int -> [Int]
-- Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
-- n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0   = []
cuentaRegresiva n   = n : cuentaRegresiva (n-1)

-- 3. repetir :: Int -> a -> [a]
-- Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0   e    =   []
repetir n   e    =  e : repetir(n-1) e

-- 4. losPrimeros :: Int -> [a] -> [a]
-- Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
-- Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 xs     =   []
losPrimeros n []     =   []
losPrimeros n (x:xs) =  x :losPrimeros(n-1) xs

-- 5. sinLosPrimeros :: Int -> [a] -> [a]
-- Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
-- recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros  0 xs     = xs
sinLosPrimeros  n []     = []
sinLosPrimeros  n (x:xs) = sinLosPrimeros (n-1) xs

-- 3. Registros

-- 1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
-- siguientes funciones:

data Persona = P String  Int deriving Show -- nombre y edad

fer    = P "Fernando" 35
billie = P "Billie" 3
edda   = P "Edda"   12

-- mayoresA :: Int -> [Persona] -> [Persona]
-- Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA e  []      =   []
mayoresA e  (p:ps)  = if edad p > e
                        then p : mayoresA e ps
                        else mayoresA e ps

-- Devuelve la edad de una persona
edad :: Persona -> Int
edad (P _ e) = e

-- promedioEdad :: [Persona] -> Int
-- Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición:
-- la lista al menos posee una persona.
promedioEdad :: [Persona] -> Int
promedioEdad []   = error" La lista debe tener al menos una persona"
promedioEdad ps   =  promedio ps

todasLasEdades :: [Persona] -> Int
todasLasEdades []       =   0
todasLasEdades (p:ps)   = edad p + todasLasEdades ps

promedio :: [Persona] -> Int
promedio    []  =   0
promedio    ps  =   div (todasLasEdades ps)  (longitud ps)

-- elMasViejo :: [Persona] -> Persona
-- Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
-- lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo []          = error "Debe haber al menos una persona"
elMasViejo (p:[])      = p
elMasViejo (p:p1:ps)   = if(edad p > edad p1)
                        then elMasViejo (p:ps)
                        else elMasViejo (p1:ps)


-- 2. Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la siguiente
-- manera:
-- data TipoDePokemon = Agua | Fuego | Planta
-- data Pokemon = ConsPokemon TipoDePokemon Int
-- data Entrenador = ConsEntrenador String [Pokemon]
-- Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
data TipoDePokemon = Agua | Fuego | Planta deriving (Show,Eq)
data Pokemon = Pk TipoDePokemon Int deriving Show
data Entrenador = E String [Pokemon] deriving Show

charizard = Pk Fuego 25
flareon = Pk Fuego 100

lapras = Pk Agua 50
vaporeon = Pk Agua 200

gloom = Pk Planta 75
trecko = Pk Planta 300

--Entrenadores para probar
red     = E "Red"  [charizard,lapras,gloom, vaporeon]
gary    = E "Gary"[trecko,vaporeon]
misty   = E "Misty" [lapras,vaporeon]
satoshi = E "Satoshi" [flareon]
blaine  = E "Blaine" [flareon, charizard, flareon, flareon, flareon]
maestro = E "Maestro"[charizard, gloom, lapras]
-- Definir en base a esa representación las siguientes funciones:

-- cantipookemon :: Entrenador -> Int
-- Devuelve la cantidad de Pokémon que posee el entrenador.
cantipookemon :: Entrenador -> Int
cantipookemon (E _ [])       = 0     
cantipookemon (E _ (p:ps))   = contarPokemones (p:ps)

contarPokemones :: [Pokemon] -> Int
contarPokemones []      =   0
contarPokemones (p:ps)  =   longitud (p:ps)

-- cantipookemonDe :: TipoDePokemon -> Entrenador -> Int
-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (E _ [])       =   0
cantidadDePokemonDe tipo (E _ ps)   =  contarPokemonesSegunTipo tipo ps

contarPokemonesSegunTipo :: TipoDePokemon -> [Pokemon]  ->    Int
contarPokemonesSegunTipo tipo []      =   0
contarPokemonesSegunTipo tipo (p:ps)  =   contarSiEsDeTipo tipo p + 
                                          contarPokemonesSegunTipo tipo ps
--funciones practica 1
contarSiEsDeTipo :: TipoDePokemon -> Pokemon -> Int
contarSiEsDeTipo tipoAContar (Pk t1 _) = unoSi(tipoAContar == t1)

unoSi:: Bool -> Int
unoSi True =  1
unoSi False = 0


-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
-- a los Pokemon del segundo entrenador.
losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int 
losQueLeGanan tipo  (E n1 [])  (E n2 [])  =   0
losQueLeGanan tipo  (E n1 (pk1:pks1)) (E n2 pks) = sumaSiTipoLeGanaA (tipoPokemon pk1) pks


sumaSiTipoLeGanaA :: TipoDePokemon -> [Pokemon]  -> Int --suma 1 si el tipo es superior a algun tipo de pokemon de la lista
sumaSiTipoLeGanaA tipo []          = 0
sumaSiTipoLeGanaA tipo (p:pks)     =  if(elementoSuperiorA tipo (tipoPokemon p))
                                      then (sumaSiTipoLeGanaA tipo pks) + 1
                                      else sumaSiTipoLeGanaA tipo pks
    --auxiliar practica 1
elementoSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool
elementoSuperiorA Agua  Fuego    = True
elementoSuperiorA Fuego Planta   = True
elementoSuperiorA Planta Agua    = True
elementoSuperiorA _   _          = False

--auxiliar
tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (Pk t _)    =   t
esTipoDePokemon :: Pokemon -> TipoDePokemon -> Bool
esTipoDePokemon (Pk t1 _) tipo  =   esMismoTipo tipo t1

-- esMaestroPokemon :: Entrenador -> Bool
-- Dado un entrenador, devuelve True si posee al menos un PokÃ©mon de cada tipo posible.

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (E _ (pk:pks)) = sonTodosLosTipos pks 

sonTodosLosTipos :: [Pokemon]  -> Bool
sonTodosLosTipos  []       = True
sonTodosLosTipos  (pk:pks) = alMenosUnPokemonDeTipo [pk] (tipoPokemon pk)  && 
                             sonTodosLosTipos  pks

alMenosUnPokemonDeTipo :: [Pokemon] -> TipoDePokemon -> Bool
alMenosUnPokemonDeTipo [] tipo       = False
alMenosUnPokemonDeTipo (pk:pks) tipo = esTipoDePokemon pk tipo || 
                                       alMenosUnPokemonDeTipo pks tipo

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Fuego Fuego   = True
esMismoTipo Agua Agua     = True
esMismoTipo Planta Planta = True
esMismoTipo _   _         = False
 

-- 3. El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
-- de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
-- una lista de personas con diferente rol. La definición es la siguiente:

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol]    deriving Show

-- Definir las siguientes funciones sobre el tipo Empresa:

-- proyectos :: Empresa -> [Proyecto]
-- Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.

-- losDevSenior :: Empresa -> [Proyecto] -> Int
-- Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
-- además a los proyectos dados por parámetro.

-- cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
-- Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.

-- asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
-- cantidad de personas involucradas.
