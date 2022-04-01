-- --1. Números enteros

-- 1. Defina las siguientes funciones:

-- a) sucesor :: Int -> Int
-- Dado un número devuelve su sucesor
sucesor :: Int -> Int
sucesor n = n + 1

-- b) sumar :: Int -> Int -> Int
-- Dados dos números devuelve su suma utilizando la operación +.
sumar :: Int -> Int -> Int
sumar n m = n + m

-- c) divisionYResto :: Int -> Int -> (Int, Int)
-- Dado dos números, devuelve un par donde la primera componente es la división del
-- primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
-- para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
-- provista por Haskell.
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

-- d) maxDelPar :: (Int,Int) -> Int
-- Dado un par de números devuelve el mayor de estos.
maxDelPar :: (Int,Int) -> Int
maxDelPar (n,m) = if n > m
                  then n 
                  else m  
--O se puede hacer así tambien.                  
maxDelPar' :: (Int, Int) -> Int
maxDelPar' (n,m) = max n m                  
                  
-- 2. De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión
-- a todas las funciones del punto anterior.

-- Ejemplo: maxDelPar (divisionYResto (sumar 5 5) (sucesor 0)) 
--Ejemplo 1 maxDelPar (divisionYResto (maxDelPar(2,20)) (sucesor (sumar 1 0)))
--Ejemplo 2 maxDelPar (divisionYResto (sumar 20 (sucesor 19)) 4 ) 
--Ejemplo 3 maxDelPar( divisionYResto (sucesor (sumar 78 1)) 8)   
--Ejemplo 4 maxDelPar( divisionYResto (sucesor (sumar 78 (sucesor 0))) (sumar 4 4))  


-- 2. Tipos enumerativos

-- 1. Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
-- las siguientes funciones:

data Dir = Norte | Sur | Este | Oeste deriving Show

-- a) opuesto :: Dir -> Dir
-- Dada una dirección devuelve su opuesta.
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Este = Oeste


-- b) iguales :: Dir -> Dir -> Bool
-- Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

-- c) siguiente :: Dir -> Dir
-- Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
-- la siguiente dirección a Oeste. ¿Posee una precondición esta función? ¿Es una función
-- total o parcial? ¿Por qué?
siguiente :: Dir -> Dir

siguiente Sur = Este
siguiente Este = Norte
siguiente Norte = Este
siguiente Oeste = error "No existe siguiente de Oeste"

-- 2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
-- Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
-- es domingo. Luego implementar las siguientes funciones:
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes| Sabado | Domingo deriving Show


-- a) primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
-- Devuelve un par donde la primera componente es el primer día de la semana, y la
-- segunda componente es el último día de la semana.
primeroYUltimoDia :: (DiaDeSemana,DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

-- b) empiezaConM :: DiaDeSemana -> Bool
-- Dado un dia de la semana indica si comienza con la letra M.
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

-- c) vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
-- Dado dos dias de semana, indica si el primero viene después que el segundo.
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Martes     = True
vieneDespues Martes Miercoles = True
vieneDespues Miercoles Jueves = True
vieneDespues Jueves Viernes   = True
vieneDespues Viernes Sabado   = True
vieneDespues Sabado Domingo   = True
vieneDespues _ _              = False


-- d) estaEnElMedio :: DiaDeSemana -> Bool
-- Dado un dia de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = True
estaEnElMedio Domingo = True
estaEnElMedio _       = False

-- 3. Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina
-- las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
-- ya definidas en Haskell):

-- a) negar :: Bool -> Bool
-- Dado un booleano, si es True devuelve False, y si es False devuelve True.
-- En Haskell ya está definida como not.
negar :: Bool -> Bool
negar True  = False
negar False = True


-- b) implica :: Bool -> Bool -> Bool
-- Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
-- devuelve True.
-- Nota: no viene implementada en Haskell.
implica :: Bool -> Bool -> Bool
implica True False = False
implica _   _      = True

-- c) and :: Bool -> Bool -> Bool
-- Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
-- En Haskell ya está definida como \&\&.
and :: Bool -> Bool -> Bool
and True True   = True
and _   _       = False

-- d) or :: Bool -> Bool -> Bool
-- Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
-- En Haskell ya está definida como ||.
or :: Bool -> Bool -> Bool
or True _      = True
or False False = False

-- 3. Registros

-- 1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
-- siguientes funciones:

-- nombre :: Persona -> String
-- Devuelve el nombre de una persona

-- edad :: Persona -> Int
-- Devuelve la edad de una persona

-- crecer :: Persona -> Persona
-- Aumenta en uno la edad de la persona.

-- cambioDeNombre :: String -> Persona -> Persona
-- Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
-- nuevo nombre.

-- esMayorQueLaOtra :: Persona -> Persona -> Bool
-- Dadas dos personas indica si la primera es mayor que la segunda.

-- laQueEsMayor :: Persona -> Persona -> Persona
-- Dadas dos personas devuelve a la persona que sea mayor.

-- 2. Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
-- porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego definir las
-- siguientes funciones:

-- superaA :: Pokemon -> Pokemon -> Bool
-- Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
-- supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.

-- cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.

-- juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
-- Dado un par de entrenadores, devuelve a sus Pokémon en una lista.


-- 4. Funciones polimórficas

-- 1. Defina las siguientes funciones polimórficas:

-- a) loMismo :: a -> a
-- Dado un elemento de algún tipo devuelve ese mismo elemento.

-- b) siempreSiete :: a -> Int
-- Dado un elemento de algún tipo devuelve el número 7.

-- c) swap :: (a,b) -> (b, a)
-- Dadas una tupla, invierte sus componentes.

-- ¿Por qué existen dos variables de tipo diferentes?

-- 2. Responda la siguiente pregunta: ¿Por qué estas funciones son polimórficas?
-- 5. Pattern matching sobre listas
-- 1. Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no
-- utilizar las funciones que ya vienen con Haskell):

-- 2. estaVacia :: [a] -> Bool
-- Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
-- Definida en Haskell como null.

-- 3. elPrimero :: [a] -> a
-- Dada una lista devuelve su primer elemento.
-- Definida en Haskell como head.
-- Nota: tener en cuenta que el constructor de listas es :

-- 4. sinElPrimero :: [a] -> [a]
-- Dada una lista devuelve esa lista menos el primer elemento.
-- Definida en Haskell como tail.
-- Nota: tener en cuenta que el constructor de listas es :

-- 5. splitHead :: [a] -> (a, [a])
-- Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
-- lista, y la segunda componente es esa lista pero sin el primero.
-- Nota: tener en cuenta que el constructor de listas es :
