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

-- b.
type Palo = Habilidad -> Tiro

palos :: [Palo]
palos = [putter, madera {- hierro 1,..., hierro 10, tenemos q zafar esto generalizando-} ] ++ map hierro [1..10]

--2.
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

golpe' :: Palo -> Jugador -> Tiro
golpe' palo = (palo . habilidad)

{- 3. Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve
 afectado dicho tiro por el obstáculo. En principio necesitamos representar los siguientes obstáculos: 
    a. Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la 
    velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0. 
    b. Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar
    una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el
    largo de la laguna.
    c. Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al 
    superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.      
Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en cuenta que en caso de no superarlo, se detiene, 
quedando con todos sus componentes en 0.     -}

-- Primero abstraemos aquello que despues definiremos 
tunelConRampita :: Tiro -> Tiro
tunelConRampita tiroOriginal 
  | superaTunelConRampita tiroOriginal = efectoTunelConRampita tiroOriginal
  | otherwise = tiroDetenido 

tiroDetenido = UnTiro 0 0 0 

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita = undefined

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita = undefined

-- Hacemos lo mismo para laguna y ya notamos una repeticion de logica, entonces buscamos abstraer esa logica general, reescribir la 
-- funcion en base a esa logica general, encontrar lo que es igual, parametrizar lo que es distinto 
laguna :: Tiro -> Tiro
laguna tiroOriginal
  | superaLaguna tiroOriginal = efectoLaguna tiroOriginal
  | otherwise = tiroDetenido 

superaLaguna :: Tiro -> Bool
superaLaguna = undefined

efectoLaguna :: Tiro -> Tiro
efectoLaguna = undefined

