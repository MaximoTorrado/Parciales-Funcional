{- Fairly odd parents : “Timmy is an average kid that no one understands...” En la ciudad de Dimmsdale, los chicos pueden contar con  padrinos mágicos que cumplen sus deseos. Y Jorgen Von Strángulo, encargado de supervisar 
toda la actividad mágica del lugar, nos encargó un sistema funcional para poder entender a los ahijados. De los chicos se conoce su nombre, edad, sus habilidades y sus deseos.
Por ejemplo: timmy = Chico “Timmy” 10 [“mirar television”, “jugar en la pc”] [serMayor]
A. Concediendo deseos
1. Desarrollar los siguiente deseos y declarar el data Chico -}
-- Respuesta: 

data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseos]
} -- deriving Show

type Habilidad = String
type Deseos = Chico -> Chico

-- 1) a) aprenderHabilidades habilidades unChico : agrega una lista de habilidades nuevas a las que ya tiene el chico.
-- Respuesta: 

-- Como los deseos significan simular algun cambio en los campos de un Chico, entonces creamos los "mapX"
mapNombre :: (String -> String) -> Chico -> Chico
mapNombre funcionModificacion unChico = unChico { nombre = funcionModificacion . nombre $ unChico }

mapEdad :: (Int -> Int) -> Chico -> Chico
mapEdad funcionModificacion unChico = unChico { edad = funcionModificacion . edad $ unChico }

mapHabilidades :: ([Habilidad] -> [Habilidad]) -> Chico -> Chico
mapHabilidades funcionModificacion unChico = unChico { habilidades = funcionModificacion . habilidades $ unChico }

mapDeseos :: ([Deseos] -> [Deseos]) -> Chico -> Chico
mapDeseos funcionModificacion unChico = unChico { deseos = funcionModificacion . deseos $ unChico }

aprenderHabilidades :: [Habilidad] -> Deseos
aprenderHabilidades unasHabilidades unChico = mapHabilidades (++ unasHabilidades) unChico             

{- 1) b) serGrosoEnNeedForSpeed unChico: dado un chico, le agrega las habilidades de jugar a todas las versiones pasadas y futuras del Need For Speed, que son: “jugar need for speed 1”, “jugar need for speed 2”, etc.   -}
-- Respuesta:  

{- IMPORTANTE!!
Si bien pareciera que estamos "repitiendo logica" al llamar a "mapHabilidades" pero como en ambas estamos pasandole una funcion aplicada parcialmente "++ String", mas que eso no podemos andar delegando porque estamos 
exprimiendo bien bien el "mapHabilidades" -}

serGrosoEnNeedForSpeed :: Deseos
serGrosoEnNeedForSpeed unChico = mapHabilidades (++ versionesPasadasNeedForSpeed) unChico

versionesPasadasNeedForSpeed :: [String]
versionesPasadasNeedForSpeed = map (\n -> "jugar need for speed" ++ show n) [1 ..]

-- 1) c) serMayor unChico: Hace que el chico tenga 18 años.
-- Respuesta

serMayor :: Deseos
serMayor unChico = mapEdad (const 18) unChico






















