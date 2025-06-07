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

{- B) 1) En busqueda de pareja 1. Se acerca el baile de fin de año y se quiere saber cuáles van a ser las parejas. Para esto las chicas tienen condiciones para elegir al chico con el
que van a salir, algunas de ellas son:
a. tieneHabilidad unaHabilidad unChico: Dado un chico y una habilidad, dice si la posee.
b. esSuperMaduro: Dado un chico dice si es mayor de edad (es decir, tiene más de 18 años) y además sabe manejar. -}
 
-- B) 1) a)  tieneHabilidad unaHabilidad unChico: Dado un chico y una habilidad, dice si la posee.
-- Respuesta: 
tieneHabilidad :: Habilidad -> Chico -> Bool
tieneHabilidad unaHabilidad unChico = (laPosee unaHabilidad . habilidades) unChico

-- elem es capaz de recibir un string y una lista de string, para mirar elemento por elemento de la lista de string para comparar si son iguales 
laPosee :: Habilidad -> [Habilidad] -> Bool
laPosee unaHabilidad unasHabilidades = elem unaHabilidad unasHabilidades


-- B) 1) b) esSuperMaduro: Dado un chico dice si es mayor de edad (es decir, tiene más de 18 años) y además sabe manejar.
-- Respuesta:

esSuperMaduro :: Chico -> Bool
esSuperMaduro unChico = edad unChico > 18 && sabeManejar unChico

sabeManejar :: Chico -> Bool
sabeManejar unChico = elem "Manejar" (habilidades unChico)









