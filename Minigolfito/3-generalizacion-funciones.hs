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

-- a. Alternativa con modelado de funciones (reciba una habilidad y dada dicha habilidad genera el tipo adecuado del tiro)
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

-- 2.

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

{-
tunelConRampita :: Tiro -> Tiro
tunelConRampita tiroOriginal 
  | superaTunelConRampita tiroOriginal = efectoTunelConRampita tiroOriginal
  | otherwise = tiroDetenido 

tiroDetenido = UnTiro 0 0 0 

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita = undefined

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita = undefined

laguna :: Tiro -> Tiro
laguna tiroOriginal
  | superaLaguna tiroOriginal = efectoLaguna tiroOriginal
  | otherwise = tiroDetenido 

superaLaguna :: Tiro -> Bool
superaLaguna = undefined

efectoLaguna :: Tiro -> Tiro
efectoLaguna = undefined

Notamos la repeticion de logica entonces abstraemos esa logica general, reescribimos la funcion en base a esa logica general, 
encontramos lo que es igual y parametrizamos lo que es distinto    -}

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

-- tunelConRampita (UnTiro {precision = 100, altura = 0, velocidad = 20})
-- UnTiro {velocidad = 40, precision = 100, altura = 0}
-- tunelConRampita (UnTiro {precision = 85, altura = 0, velocidad = 20}) 
-- UnTiro {velocidad = 0, precision = 0, altura = 0}


--b. Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar
--   una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el
--   largo de la laguna.
laguna :: Int -> Obstaculo
laguna largo = obstaculoSuperableSi superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro

-- 1) usamos otra forma de devolver una copia del data que recibio con dichos cambios, y 2) notamos que necesitamos el largo de la 
-- laguna, al no tener este dato lo agregamos en el tipado de "laguna" y lo pasamos aplicado parcialmente (igual que punto 1)
efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo} 

-- laguna 2 (UnTiro {precision = 85, altura = 0, velocidad = 20})
-- UnTiro {velocidad = 0, precision = 0, altura = 0}
-- laguna 2 (UnTiro {precision = 85, altura = 3, velocidad = 82})
-- UnTiro {velocidad = 82, precision = 85, altura = 1}

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



