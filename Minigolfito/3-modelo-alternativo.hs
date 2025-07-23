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
    altura = (n - 3) `max` 0 -- dado el caso donde la resta "n - 3" sea negativa entonces el minimo es el 0
}

-- hierro 4 (habilidad bart)
-- UnTiro {velocidad = 100, precision = 15, altura = 1}

-- b. Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego
type Palo = Habilidad -> Tiro

palos :: [Palo]
palos = [putter, madera {- hierro 1,..., hierro 10, tenemos q zafar esto generalizando-} ] ++ map hierro [1..10]

-- 2.
-- Lo que hace golpe es a partir de un jugador aplicar la funcion palo que labura con la habilidad de un jugador
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

-- golpe bart putter
-- UnTiro {velocidad = 10, precision = 120, altura = 0}

golpe' :: Palo -> Jugador -> Tiro
golpe' palo = (palo . habilidad)

-- golpe' bart putter
-- UnTiro {velocidad = 10, precision = 120, altura = 0}


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

-- De un obstaculo nos interesaba dos cosas, si un tiro puede superarlo y como termina afectado el tiro, nahuel penso en
-- en crear una estructura "compuesta" o un tipo de dato "compuesto" que tiene las 2 ideas por separado, una con como se
-- supera y otra con como queda 

data Obstaculo = UnObstaculo {
    puedeSuperar :: Tiro -> Bool, 
    efectoLuegoDeSuperar :: Tiro -> Tiro
    } -- no le ponemos deriving (Eq, Show) porque este data dentro tiene funciones, que no se pueden comparar ni "mostrar"

tiroDetenido = UnTiro 0 0 0

-- masomenos basandonos con la funcion general de la otra alternativa
intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal 
  | (puedeSuperar obstaculo) tiroOriginal = (efectoLuegoDeSuperar obstaculo) tiroOriginal 
  | otherwise = tiroDetenido  

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

-- intentarSuperarObstaculo tunelConRampita (UnTiro {precision = 95, altura = 0, velocidad = 10})
-- UnTiro {velocidad = 20, precision = 100, altura = 0}
-- intentarSuperarObstaculo tunelConRampita (UnTiro {precision = 95, altura = 10, velocidad = 10})
-- UnTiro {velocidad = 0, precision = 0, altura = 0}

laguna :: Int ->  Obstaculo 
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)
 
superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro

efectoLaguna :: Int -> Tiro -> Tiro 
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo} 

-- intentarSuperarObstaculo (laguna 10) (UnTiro {velocidad = 85, altura = 9, precision = 10}) 
-- UnTiro {velocidad = 0, precision = 0, altura = 0}

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo 

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . altura) tiro && vaAlRasDelSuelo tiro && precision tiro > 95

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido

