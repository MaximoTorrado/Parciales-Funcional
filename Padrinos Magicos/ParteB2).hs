data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
} -- deriving Show

type Habilidad = String
type Deseo = Chico -> Chico

-- A) 1) a) aprenderHabilidades habilidades unChico : agrega una lista de habilidades nuevas a las que ya tiene el chico.
mapNombre :: (String -> String) -> Chico -> Chico
mapNombre funcionModificacion unChico = unChico { nombre = funcionModificacion . nombre $ unChico }

mapEdad :: (Int -> Int) -> Chico -> Chico
mapEdad funcionModificacion unChico = unChico { edad = funcionModificacion . edad $ unChico }

mapHabilidades :: ([Habilidad] -> [Habilidad]) -> Chico -> Chico
mapHabilidades funcionModificacion unChico = unChico { habilidades = funcionModificacion . habilidades $ unChico }

mapDeseos :: ([Deseo] -> [Deseo]) -> Chico -> Chico
mapDeseos funcionModificacion unChico = unChico { deseos = funcionModificacion . deseos $ unChico }

aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades unasHabilidades unChico = mapHabilidades (++ unasHabilidades) unChico             

{-A) 1) b) serGrosoEnNeedForSpeed unChico: dado un chico, le agrega las habilidades de jugar a todas las versiones pasadas y futuras del Need For Speed, que son: “jugar need for speed
1”, “jugar need for speed 2”, etc.   -}
serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed unChico = mapHabilidades (++ versionesPasadasNeedForSpeed) unChico

versionesPasadasNeedForSpeed :: [String]
versionesPasadasNeedForSpeed = map (\n -> "jugar need for speed" ++ show n) [1 ..]

-- A) 1) c) serMayor unChico: Hace que el chico tenga 18 años.
serMayor :: Deseo
serMayor unChico = mapEdad (const 18) unChico

-- A) 2) a) wanda: dado un chico, wanda le cumple el primer deseo y lo hace madurar (crecer un año de edad).
wanda :: Deseo 
wanda unChico = (serMayor . head (deseos unChico)) unChico

-- A) 2) b) cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de edad. Como es olvidadizo, no le concede ningún deseo.
cosmo :: Deseo
cosmo unChico = id unChico

cosmoSiNoOlvida :: Deseo
cosmoSiNoOlvida unChico = mapEdad (`div` 2) unChico

-- A) 2) c) muffinMagico: dado un chico le concede todos sus deseos.
muffinMagico :: Deseo
muffinMagico unChico = foldr ($) unChico (deseos unChico)
 
-- B) 1) a)  tieneHabilidad unaHabilidad unChico: Dado un chico y una habilidad, dice si la posee.
tieneHabilidad :: Habilidad -> Chico -> Bool
tieneHabilidad unaHabilidad unChico = (laPosee unaHabilidad . habilidades) unChico

laPosee :: Habilidad -> [Habilidad] -> Bool
laPosee unaHabilidad unasHabilidades = elem unaHabilidad unasHabilidades


-- B) 1) b) esSuperMaduro: Dado un chico dice si es mayor de edad (es decir, tiene más de 18 años) y además sabe manejar.
esSuperMaduro :: Chico -> Bool
esSuperMaduro unChico = edad unChico > 18 && sabeManejar unChico

sabeManejar :: Chico -> Bool
sabeManejar unChico = elem "Manejar" (habilidades unChico)

{- B) 2) Las chicas tienen un nombre, y una condición para elegir al chico con el que van ir al baile. Ejemplos: para Trixie la única condición es que el chico no sea Timmy, ya que 
nunca saldría con él : trixie = Chica “Trixie Tang” noEsTimmy
                       vicky = Chica “Vicky” (tieneHabilidad “ser un supermodelo noruego”)

Se pide definir el data Chica -}
-- Respuesta:

{- Sabemos que el data de una Chica tiene un nombre (String) y una condicion, que abstrayendo la logica debe basicamente recibir un chico y fijarse si es o no es apto para ella, esto
tambien nos damos cuenta si miramos las funciones que usan en el cuerpo de cada una, trixie usa "noEsTimmy" si bien no esta definida pareciera que toma un chico y devolveria True en
caso que no sea timmy, luego trixie usa "tieneHabilidad "ser un supermodelo noruego"" que si vemos el tipado de "tieneHabilidad" tambien recibe un chico mas una habilidad y devuelve 
un booleano (aplicado parcialmente), entonces      -}

type Condicion = Chico -> Bool
data Chica = Chica {
    nombreChica :: String,
    condicion :: Condicion 
} 

-- EXTRA!! Un posible planteo para "noEsTimmy"
noEsTimmy :: Chico -> Bool
noEsTimmy unChico = nombre unChico /= "Timmy"

{- 2) B) a) quienConquistaA unaChica losPretendientes: Dada una chica y una lista de pretendientes, devuelve al que se queda con la chica, es decir, el primero que cumpla con la 
condición que ella quiere. Si no hay ninguno que la cumpla, devuelve el último pretendiente (una chica nunca se queda sola). (Sólo en este punto se puede usar recursividad) -}
-- Respuesta: 

quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica losPretendientes = 




