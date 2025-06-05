{--
Parte A
1. Modelar Personaje, estos tienen: nombre, una cantidad de poder 
y una lista de derrotas (léase, las veces que derrotó a alguien). 
Cada derrota tiene el nombre de su oponente y el año en el que ocurrió. --}

data Personaje = UnPersonaje {
    nombre :: String,
    poder :: Int,
    derrotas :: [Derrota]  
} deriving (Show)

data Derrota = UnaDerrota {
    oponente :: String,
    anio :: Int
} deriving (Show)

{--2. Modelar entrenamiento, el cual lo realizan un grupo de personajes y 
multiplica el poder de cada uno de ellos por la cantidad de personajes que están entrenando al mismo tiempo.--}

type Entrenamiento = [Personaje] -> [Personaje]

entrenamiento :: Entrenamiento
entrenamiento personajes = map (mapPoder (* length personajes)) personajes
--entrenamiento personajes = map (mapPoder ((*).length $ personajes)) personajes
--entrenamiento personajes = map (entrenar (length personajes)) personajes

entrenar :: Int -> Personaje -> Personaje
entrenar multiplicadorDePoder personaje = mapPoder (*multiplicadorDePoder) personaje 

mapPoder :: (Int -> Int) -> Personaje -> Personaje
mapPoder modificacion unPersonaje = unPersonaje { poder = modificacion . poder $ unPersonaje }

{--
3. Modelar rivalesDignos, que dado un grupo de personajes nos dice 
quienes son rivales para Thanos. Son dignos aquellos personajes que, 
luego de haber entrenado,  tienen un poder mayor a 500 y además alguna de sus derrotas se llame "Hijo de Thanos". --}

rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos personajes = filter esDigno.entrenamiento $ personajes
--filter esDigno (entrenamiento personajes)

esDigno :: Personaje -> Bool
esDigno personaje = poder personaje > 500 && any (esDerrota "Hijo de Thanos") (derrotas personaje)
--esDigno personaje = (>500).poder $ personaje && any (esDerrota "Hijo de Thanos").derrotas $ personaje 

esDerrota :: String -> Derrota -> Bool
esDerrota nombreDerrota derrota = (== nombreDerrota).oponente $ derrota

esDigno :: Personaje -> Bool
esDigno personaje = esPoderoso personaje && derrotoA "HijoDeThanos".derrotas $ personaje 

esPoderoso :: Personaje -> Bool 
esPoderoso personaje = (>500).poder $ personaje 
--esPoderoso personaje = 500 < (poder personaje) 

derrotoA :: String -> [Derrota] -> Bool
derrotoA nombreDerrota derrotas = elem "hijodethanos".oponente $ derrotas


{--4. Modelar guerraCivil, la cual dado un año y dos conjuntos de 
personajes hace que cada personaje pelee con su contraparte de la otra 
lista y nos dice quienes son los ganadores. Cuando dos personajes pelean, 
gana el que posee mayor poder y se le agregará la derrota del perdedor a su 
lista de derrotas con el año en el que ocurrió. Por ejemplo...
guerraCivil 2018 [scarletWitch, capitanAmerica, blackWidow] [vision, ironMan, spiderman] 
...hará que Scarlet Witch pelee contra Vision, el Capitán América contra Iron Man y Black Widow contra Spiderman.
--}

guerraCivil :: Int -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil anio ladoA ladoB = zipWith (pelear anio) ladoA ladoB

pelear :: Int -> Personaje -> Personaje -> Personaje 
pelear anio p1 p2
  | poder p1 > poder p2 = agregarDerrota anio p1 p2
  | otherwise = agregarDerrota anio p2 p1

agregarDerrota :: Int -> Personaje -> Personaje -> Personaje
agregarDerrota anio ganador perdedor = mapDerrotas (UnaDerrota (nombre perdedor) anio :) ganador

mapDerrotas :: ([Derrota] -> [Derrota]) -> Personaje -> Personaje
mapDerrotas unaFuncion unPersonaje = unPersonaje { derrotas = unaFuncion . derrotas $ unPersonaje }
--mapDerrotas unaFuncion unPersonaje = unPersonaje { derrotas = unaFuncion (derrotas unPersonaje)}


{-Ahora aparecen los equipamientos: estos son objetos de poder que dado un personaje modifican sus habilidades de manera extraordinaria.
 Asimismo, sabemos que los personajes tienen varios equipamientos.

1. Escribir el tipo de un Equipamiento y modificar el modelo de Personaje para que acepte equipamientos.

2. Tenemos equipamientos generales como por ejemplo escudo y trajeMecanizado. Modelar los siguientes equipamientos.
escudo: si tiene menos de 5 derrotas le suma 50 de poder, pero si tiene 5 o más le resta 100 de poder. 
trajeMecanizado: devuelve el personaje anteponiendo "Iron" al nombre del personaje y le agrega una
 versión dada al final del mismo. Por ejemplo:
Si el personaje se llama "Groot" y la versión del traje es 2 , su nombre quedaría "Iron Groot V2"
-}

data Personaje {nombre :: string,
poder:: Int,
derrotas ::[Derrota],
equipamientos :: [Equipamiento]}
type Equipamiento = Personaje -> Personaje 

escudo :: Equipamiento
escudo unPersonaje
  | (<5).length.derrotas $ unPersonaje = mapCantidadPoder (+50) unPersonaje
  | otherwise = mapCantidadPoder (subtract 100) unPersonaje

escudo :: Equipamiento
escudo unPersonaje
  | GanoMenosDeCinco unPersonaje = mapCantidadPoder (+50) unPersonaje
  | otherwise = mapCantidadPoder (subtract 100) unPersonaje

GanoMenosDeCinco :: Personaje -> Bool
GanoMenosDeCinco unPersonaje =  (<5).length.derrotas $ unPersonaje

trajeMecanizado :: Int -> Equipamiento
trajeMecanizado version unPersonaje = mapNombre (\nombre -> "Iron " ++ nombre ++ " V" ++ show version) unPersonaje

trajeMecanizado :: Int -> Equipamiento
trajeMecanizado version unPersonaje = mapNombre (transformarEnIronVersionado version) unPersonaje

transformarEnIronVersionado :: Int -> String -> String
transformarEnIronVersionado version nombre = "Iron " ++ nombre ++ " V" ++ show version

mapNombre :: (String -> String) -> Personaje -> Personaje
mapNombre f unPersonaje = unPersonaje {nombre = f . nombre $ unPersonaje}

{-3. También tenemos equipamientos exclusivos que solo lo pueden usar determinados
 personajes, por ejemplo: stormBreaker solo lo puede usar Thor, guantelete del Infinito sólo lo 
 puede usar Thanos, gemaDelAlma sólo lo puede usar Thanos. Modelar:

stormBreaker: Le agrega "dios del trueno" al final del nombre y limpia su historial de derrotas ya que un dios es bondadoso.

gemaDelAlma: Añade a la lista de derrotas a todos los extras, y cada uno con un año 
diferente comenzando con el actual. Considerar que hay incontables extras. Por ejemplo: 

[("extra numero 1", 2018), ("extra numero 2", 2019), …]
-}

equipamientoExclusivo :: String -> Personaje -> Equipamiento -> Equipamiento
equipamientoExclusivo nombrePersonaje unPersonaje equipamiento
  | nombre unPersonaje == nombrePersonaje = equipamiento
  | otherwise = id 

stormBreaker :: Equipamiento
stormBreaker unPersonaje = equipamientoExclusivo "Thor" unPersonaje (mapNombre (++ " dios del trueno").mapDerrotas (const [])) $ unPersonaje

gemaDelAlma :: Equipamiento
gemaDelAlma unPersonaje = equipamientoExclusivo "Thanos" unPersonaje (mapDerrotas (++ derrotasExtras)) $ unPersonaje

derrotasExtras :: [Derrota]
derrotasExtras = zip extras [2018 ..]

extras :: [String]
extras = map agregarExtra [1..]

agregarExtra :: Int -> String
agregarExtra valor = ("extra numero "++).show $ valor
--agregarExtra valor = ("extra numero "++) (show valor)

{--
guanteleteInfinito: Aplica todos los equipamientos que sean gemas del infinito al personaje. 
Usar la función sin definirla esGemaDelInfinito la cual recibe un equipamiento y nos dice si la misma es o no una gema del infinito. 

>> esGemaDelInfinito escudo
False
>> esGemaDelInfinito gemaDelAlma
True
--}
guanteleteInfinito :: [Equipamiento] -> Personaje -> Personaje
guanteleteInfinito equipamientos personaje =
  foldr ($) personaje (filter esGemaDelInfinito equipamientos)

guanteleteInfinito :: [Equipamiento] -> Personaje -> Personaje
guanteleteInfinito equipamientos personaje =
  foldl aplicarGemaDelInfinito personaje (filter esGemaDelInfinito equipamientos)

aplicarGemaDelInfinito :: Personaje -> Equipamiento -> Personaje
aplicarGemaDelInfinito personaje gema = gema personaje 



guanteleteInfinito :: Equipamiento
guanteleteInfinito personaje = equipamientoExclusivo "Thanos" personaje usarGemasDelInfinito

usarGemasDelInfinito :: Personaje -> Personaje
usarGemasDelInfinito personaje = foldr ($) personaje (filter esGemaDelInfinito.equipamientos $ personaje)

{-
  Parte C
  a. Se cuelga porque definí derrotoAMuchos con un length, y el length de una lista infinita no termina de evaluar.
     Se puede definir derrotoAMuchos de forma de que sea lazy y pueda terminar de evaluar, como:
     derrotoAMuchos = not . null . drop 5 . derrotas
  b. Si no es fuerte luego de entrenar con el resto del equipo, termina bien.
     Si es fuerte y el hijo de thanos se encuentra entre sus derrotados, termina bien.
     En caso contrario, no termina.
  c. Si. Porque haskell trabaja con evaluación perezosa, y no es necesario evaluar toda la lista para tomar los primeros 100 elementos.
-}