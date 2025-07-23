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

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

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

-- a.
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

-- b.
-- Lo mismo aca ahora creamos este obstaculo usando el constructor del nuevo data obstaculo
laguna :: Int -> Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo} 

-- c.
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


{- 4a. Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.  -}

-- que "palos" significa una lista de palos que habiamos creado en el punto 1, "a partir de un conjunto de palos quiero conseguir 
-- otro conjunto de palos segun cierto criterio" 

-- type Obstaculo ::  Tiro -> Tiro
-- golpe :: Jugador -> Palo -> Tiro
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

-- no tenemos como saber si le sirve o no el palo, ya que el obstaculo finalmente devuelve un tiro pero no un booleano, entonces tenemos
-- que hacer un REFACTOR a como modelamos el Obstaculo (rediseñar sin generar cambios en la funcionalidad)

-- puedeSuperar quiere recibir el obstaculo y el tiro que viene del golpe para ver si es capaz de superarlo o no, pudimos hacerlo ahora usando el REFACTOR con el
-- nuevo data del obstaculo entonces llamamos dicho campo y le pasamos lo que necesita
leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)

-- palosUtiles bart tunelConRampita
-- [<function>]
-- head (palosUtiles bart tunelConRampita) (habilidad bart)
-- UnTiro {velocidad = 10, precision = 120, altura = 0}



