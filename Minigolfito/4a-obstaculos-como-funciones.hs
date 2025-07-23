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

-- 1. Alternativa con modelado de funciones (reciba una habilidad y dada dicha habilidad genera el tipo adecuado del tiro)
putter :: Palo
putter habilidad = UnTiro {
    velocidad = 10, 
    precision = precisionJugador habilidad * 2, 
    altura = 0
    }

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

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

golpe' :: Palo -> Jugador -> Tiro
golpe' palo = (palo . habilidad)

-- 3.
-- las 2 funciones que recibe son aquellas que laburan en las guardas, osea en la definicion de la funcion, y el tiro en base al cual
-- se aplicaran las funciones
obstaculoSuperableSi :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
obstaculoSuperableSi condicion efecto tiroOriginal 
  | condicion tiroOriginal = efecto tiroOriginal
  | otherwise = tiroDetenido

tiroDetenido = UnTiro 0 0 0

type Obstaculo = Tiro -> Tiro

-- reescribimos las funciones en terminos de la funcion general 
--  a. Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la 
--  velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.

-- esta la pasamos point free pero podriamos no hacerlo pasandole el tiro, asi es mas "lindo" porque si miramos el tipado de la general
-- vemos que recibe 2 funciones que se la estamos pasando y despues va de Tiro -> Tiro que es el mismo tipado que esta, estan las 2 bien
tunelConRampita :: Obstaculo
tunelConRampita = obstaculoSuperableSi superaTunelConRampita efectoTunelConRampita 

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

vaAlRasDelSuelo tiro = ((==0) . altura) tiro

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = UnTiro {
    velocidad = velocidad tiro * 2,
    precision = 100,
    altura = 0
    }

--b. Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar
--   una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el
--   largo de la laguna.
laguna :: Int -> Obstaculo
laguna largo = obstaculoSuperableSi superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo} 

-- c. Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al 
-- superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.

-- De paso para mostrar como seria sin point free 
hoyo :: Obstaculo
hoyo tiro = obstaculoSuperableSi superaHoyo efectoHoyo tiro

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && vaAlRasDelSuelo tiro

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido

-- hoyo (UnTiro {velocidad = 25, altura = 0, precision = 50})    
-- UnTiro {velocidad = 0, precision = 0, altura = 0}


{- 4a. Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.  -}

-- que "palos" significa una lista de palos que habiamos creado en el punto 1, "a partir de un conjunto de palos quiero conseguir 
-- otro conjunto de palos segun cierto criterio" 

-- type Obstaculo ::  Tiro -> Tiro
-- golpe :: Jugador -> Palo -> Tiro

--palosUtiles :: Jugador -> Obstaculo -> [Palo]
-- palosUtiles jugador obstaculo = filter (leSirveParaSuperar) palos

-- encontramos un problema aca!! un obstaculo retorna un tiro pero no un booleano, entonces no podemos usarlo en el filter
-- leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
-- leSirveParaSuperar jugador obstaculo palo = obstaculo (golpe palo)