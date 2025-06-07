data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
} -- deriving Show

type Habilidad = String
type Deseo = Chico -> Chico

-- 1) a) aprenderHabilidades habilidades unChico : agrega una lista de habilidades nuevas a las que ya tiene el chico.
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

{- 1) b) serGrosoEnNeedForSpeed unChico: dado un chico, le agrega las habilidades de jugar a todas las versiones pasadas y futuras del Need For Speed, que son: “jugar need for speed
1”, “jugar need for speed 2”, etc.   -}
serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed unChico = mapHabilidades (++ versionesPasadasNeedForSpeed) unChico

versionesPasadasNeedForSpeed :: [String]
versionesPasadasNeedForSpeed = map (\n -> "jugar need for speed" ++ show n) [1 ..]

-- 1) c) serMayor unChico: Hace que el chico tenga 18 años.
serMayor :: Deseo
serMayor unChico = mapEdad (const 18) unChico

{- Los padrinos son seres mágicos capaces de cumplir los deseos de sus ahijados. Desarrollar los siguientes padrinos:
a. wanda: dado un chico, wanda le cumple el primer deseo y lo hace madurar (crecer un año de edad).
b. cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de edad. Como es olvidadizo, no le concede ningún deseo.
c. muffinMagico: dado un chico le concede todos sus deseos.
Nota importante: no debe haber lógica repetida entre wanda, cosmo y serMayor -}
-- Respuesta:

{- De la lista de deseos del chico que le paso me quedo unicamente con el primer elemento, que es una funcion que espera un chico, que luego recibe de la composicion y hace su logica
luego se aplicar ese deseo lo recibe "serMayor" -}
wanda :: Deseo 
wanda unChico = (serMayor . head (deseos unChico)) unChico

cosmo :: Deseo
cosmo unChico = id unChico

cosmoSiNoOlvida :: Deseo
cosmoSiNoOlvida unChico = mapEdad (`div` 2) unChico

-- Hacemos que se aplique una lista de deseos (una lista de funciones) a un parametro, y devuelva un parametro del mismo tipo pero con los cambios simulados
muffinMagico :: Deseo
muffinMagico unChico = foldr ($) unChico (deseos unChico)










