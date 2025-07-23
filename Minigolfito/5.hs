import Text.Show.Functions  -- esto nos permite mostrar las funciones

-- Modelo inicial

{- Lisa Simpson se propuso a desarrollar un programa que le permita ayudar a su hermano a vencer a su vecino Todd en un torneo de minigolf. Para hacerlo mas interesante, los padres de los
niños hicieron una apuesta: el padre del niño que no gane debera cortar el cesped del otro usando un vestido de su esposa
De los participantes nos interesara el nombre del jugador, el de su padre y sus habilidades (fuerza y precision)  -}
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles

between n m x = elem x [n .. m]

-- maximoSegun f = foldl1 (mayorSegun f)

-- mayorSegun f a b
--  | f a > f b = a
--  | otherwise = b

-- Alternativa con modelado de funciones (reciba una habilidad y dada dicha habilidad genera el tipo adecuado del tiro)
putter :: Palo
putter habilidad = UnTiro {
    velocidad = 10, 
    precision = precisionJugador habilidad * 2, 
    altura = 0
    }

-- putter (habilidad bart)
-- UnTiro {velocidad = 10, precision = 120, altura = 0}   

madera :: Palo
madera habilidad = UnTiro {
    velocidad = 100,
    precision = precisionJugador habilidad `div` 2,
    altura = 5
    }
hierro :: Int -> Palo       
hierro n habilidad = UnTiro {
    velocidad = fuerzaJugador habilidad * n,
    precision = precisionJugador habilidad `div` n,
    altura = (n - 3) `max` 0 
}

-- b. Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego
type Palo = Habilidad -> Tiro

palos :: [Palo]
palos = [putter, madera {- hierro 1,..., hierro 10, tenemos q zafar esto generalizando-} ] ++ map hierro [1..10]

{- 2. Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0. -}

-- Lo que hace golpe es a partir de un jugador aplicar la funcion palo que labura con la habilidad de un jugador
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

golpe' :: Palo -> Jugador -> Tiro
golpe' palo = (palo . habilidad)

-- 3.
-- Generalizacion de los obstaculos, esta funcion cumplia dos roles 1) el de crear los obstaculos mediante las condiciones de cada uno y los efectos luego de aplicarlos
-- y 2) ver como quedaba el tiro luego de aplicar superar el obstaculo, era un 2 en 1, ahora al usar un REFACTOR la 1) tarea la tomara el constructor del data nuevo
-- y la 2) en otra funcion donde del obstaculo que creamos se guarda su condicion y su efecto los campos del data, a los cuales llamaremos y evaluaremos

-- obstaculoSuperableSi :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
-- obstaculoSuperableSi condicion efecto tiroOriginal 
--  | condicion tiroOriginal = efecto tiroOriginal
--  | otherwise = tiroDetenido

-- type Obstaculo = Tiro -> Tiro

tiroDetenido :: Tiro 
tiroDetenido = UnTiro 0 0 0

-- aca tenemos que hacer un REFACTOR: Cambiar el diseño de una solucion sin cambiar la funcionalidad de la misma, donde replanteamos lo que es un obstaculo donde 
-- tendra la condicion y el efecto luego de superar, ahora la primera funcion de la funcion general anterior de crear un obstaculo la pasamos al constructor de este data
data Obstaculo = UnObstaculo {
    puedeSuperar :: (Tiro -> Bool),
    efectoLuegoDeSuperar :: (Tiro -> Tiro)
} 

-- luego la segunda operacion de la funcion general anterior de ver si supera o no (lo que hacia en las guardas) lo hacemos en esta nueva
intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal 
  | (puedeSuperar obstaculo) tiroOriginal = (efectoLuegoDeSuperar obstaculo) tiroOriginal 
  | otherwise = tiroDetenido

-- a. Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a 
-- ser 100 y la altura 0.
-- entonces usando el REFACTOR anterior donde antes usabamos "obstaculoSuperableSi" para crear los obstaculos ahora usamos el constructor del data Obstaculo
tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita 

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

vaAlRasDelSuelo tiro = ((==0) . altura) tiro

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = UnTiro {
    velocidad = velocidad tiro * 2,
    precision = 100,
    altura = 0
    }

-- b. Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura 
-- equivalente a la altura original dividida por el largo de la laguna

-- Lo mismo aca ahora creamos este obstaculo usando el constructor del nuevo data obstaculo
laguna :: Int -> Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo} 

-- c. Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.
-- Lo mismo aca y de paso para mostrar como seria sin point free 
hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo 

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && vaAlRasDelSuelo tiro

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido

-- intentarSuperarObstaculo tunelConRampita (UnTiro {velocidad = 80, precision = 95, altura = 0})
-- UnTiro {velocidad = 160, precision = 100, altura = 0}
-- intentarSuperarObstaculo tunelConRampita (UnTiro {velocidad = 80, precision = 55, altura = 0})
-- UnTiro {velocidad = 0, precision = 0, altura = 0}


-- 4a. Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.  
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)

-- 4b. Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.
-- Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos de un hoyo, el resultado sería 2 ya que la velocidad al salir del segundo túnel es de 
-- 40, por ende no supera el hoyo.
-- BONUS: resolver este problema sin recursividad, teniendo en cuenta que existe una función takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.

cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosConsecutivosSupera tiro [] = 0  -- caso base
cuantosObstaculosConsecutivosSupera tiro (obstaculo : obstaculos) -- aca abrimos la lista en su logica (head (elemento individual) : tail (resto de elementos))
  | puedeSuperar obstaculo tiro = 1 + cuantosObstaculosConsecutivosSupera (efectoLuegoDeSuperar obstaculo tiro) obstaculos 
  | otherwise = 0

-- 4c. Definir paloMasUtil que recibe una persona y una lista de obstaculos y determina cual es el palo que le permite superar mas obstaculos con un solo tiro
maximoSegun :: Ord b => (t -> b) -> [t] -> t 
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord x => (a -> x) -> a -> a -> a  
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos =  maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos . golpe jugador) palos 

-- Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo, se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño 
-- que no ganó”. Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.

-- le pone estos nombres a las funciones "fst" y "snd" como para ser mas expresivos, digamos
jugadorDeTorneo = fst
puntosGanados = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = (map (padre . jugadorDeTorneo) . filter (not . gano puntosDeTorneo)) puntosDeTorneo

-- el ultimo mapeo es ideal porque toma de la lista de tuplas solo los miembros del jugador, que de tipo "Jugador" y tienen en su campo a "padre" cargado con un string con el nombre del padre, entonces va creando
-- una lista con los strings de los padres de todos aquellos que no ganaron

-- dados todos los puntos del torneo (tupla de los jugadores y sus puntos) y una tupla de un jugador y sus puntos
gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador = (all ((< puntosGanados puntosDeUnJugador) . puntosGanados) . filter (/= puntosDeUnJugador)) puntosDeTorneo 

-- con el filter filtramos la lista de tuplas con todos aquellos que sean distinto a la tupla individual que tenemos, para tener todas las tuplas menos la individual luego con el all queremos ver si los puntos ganados
-- de la tupla individual se cumple que son mayores a todos los puntos de las demas tuplas de la lista, osea de los puntos de todos los demas jugadores menos este, si se cumple esto entonces es el ganador de la carrera

-- gano [(bart, 10), (todd, 7), (rafa, 8)]
-- <function>
-- gano [(bart, 10), (todd, 7), (rafa, 8)] (bart, 10)
-- True                                                   (tiene sentido, uno gano porque quitando dicha tupla tiene mas puntos que todos los demas)
-- gano [(bart, 10), (todd, 10), (rafa, 8)] (bart, 10) 
-- False                                                  (tiene sentido, nadie gano porque quitando dicha tupla ninguna tiene mas puntos que todos los demas)
-- pierdenLaApuesta [(bart, 10), (todd, 10), (rafa, 8)] 
-- ["Homero","Ned","Gorgory"]                             (tiene sentido, nadie gano "todos perdieron" porque ninguna tupla tiene mas puntos que todos los demas)
-- pierdenLaApuesta [(bart, 9), (todd, 10), (rafa, 8)]  
-- ["Homero","Gorgory"]