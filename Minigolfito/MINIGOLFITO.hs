module Lib where

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

{- También necesitaremos modelar los palos de golf que pueden usarse y los obstáculos que deben enfrentar para ganar el juego. -}

-- Sabemos que cada palo genera un efecto diferente, por lo tanto elegir el palo correcto puede ser la diferencia entre ganar o perder el torneo.
-- a) Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro que se compone por velocidad, precisión y altura.
--  i) El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
--  ii) La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.
--  iii) Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de  n-3 (con mínimo 0). Modelarlos de la forma más genérica posible  -}

-- 1) Alternativa con modelado de funciones (reciba una habilidad y dada dicha habilidad genera el tipo adecuado del tiro)
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

-- madera (habilidad bart)
-- UnTiro {velocidad = 100, precision = 30, altura = 5}

hierro :: Int -> Palo       -- (Podriamos pensar que hierro no es un "palo" pero si aplicado parcialmente con un entero pasa a serlo)
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

-- con el map entonces estamos creando una lista [hierro 1, ..., hierro 10] ya que aplica la funcion hierro a los elementos de la lista

-- palos
-- [<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function>]
-- last palos (habilidad bart) 
-- UnTiro {velocidad = 250, precision = 6, altura = 7}   
-- (pide agarrar el ultimo elemento de los palos, que es una funcion, y la aplica con la habilidad que le pasamos)

-- 2) Alternativa con matcheo
-- el String que recibe seria "putter" "madera" o "hierro" como identificador y luego la habilidad de la misma, termina adecuando el tiro segun esa habilidad
palo :: String -> Int -> Habilidad -> Tiro
palo "putter" _ habilidad = UnTiro {
    velocidad = 10, 
    precision = precisionJugador habilidad * 2,
    altura = 0
    }
palo "madera" _ habilidad = UnTiro {
    velocidad = 100, 
    precision = precisionJugador habilidad `div` 2,
    altura = 5
    }
-- palo "hierro 1" habilidad = UnTiro {}
-- palo "hierro 10" habilidad = UnTiro {}  queremos zafarle a esto entonces necesitamos un parametro mas, podemos agregarlo al tipado y obviarlo en los anteriores que no lo necesitan
palo "hierro" n habilidad = UnTiro {
  velocidad = fuerzaJugador habilidad * n, 
  precision = precisionJugador habilidad `div` n,
  altura = (n-3) `max` 0
}

-- palo "putter" (habilidad bart)
-- UnTiro {velocidad = 10, precision = 120, altura = 0}
-- palo "madera" (habilidad bart)
-- UnTiro {velocidad = 100, precision = 30, altura = 5}

-- Con el cambio que hicimos para "hierro" ahora deberiamos para todas las llamadas a "palo" pasarle un numero 
-- palo "hierro" 4 (habilidad bart)
-- UnTiro {velocidad = 100, precision = 15, altura = 1}

{- b. Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego -}

-- esto NO se puede hacer al usar variables incognitas => palos = [palo "putter" _, palo "madera" _, ...]
-- y bueno literal no encontraron manera de armar la constante usando matcheo, Grok me dio esta 
palos' :: [Habilidad -> Tiro]
palos' = [palo "putter" 0, palo "madera" 0] ++ [palo "hierro" n | n <- [1..10]]

{- 2. Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0. -}
-- Lo que hace golpe es a partir de un jugador aplicar la funcion palo que labura con la habilidad de un jugador
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

-- golpe bart putter
-- UnTiro {velocidad = 10, precision = 120, altura = 0}

golpe' :: Palo -> Jugador -> Tiro
golpe' palo = (palo . habilidad)

-- golpe' bart putter
-- UnTiro {velocidad = 10, precision = 120, altura = 0}

golpe''' :: Jugador -> Palo -> Tiro
golpe''' jugador palo = (palo . habilidad) jugador

{- 3. Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve
 afectado dicho tiro por el obstáculo. En principio necesitamos representar los siguientes obstáculos:  -}

{- NOSOTROS ANTERIORMENTE PLANTEAMOS ESTO, DONDE DESPUES NOS DIMOS CUENTA DE LA REPETICION DE LOGICA OPTANDO POR UN "REFACTOR" EN UNA FUNCION GENERAL

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

Notamos la repeticion de logica entonces abstraemos esa logica general, reescribimos la funcion en base a esa logica general, encontramos lo que es igual y parametrizamos lo que es distinto    -}

-- 3- 1) ALTERNATIVA GENERALIZACION DE FUNCIONES
-- las 2 funciones que recibe son aquellas que laburan en las guardas, osea en la definicion de la funcion, y el tiro en base al cual se aplicaran las funciones
obstaculoSuperableSi :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
obstaculoSuperableSi condicion efecto tiroOriginal 
  | condicion tiroOriginal = efecto tiroOriginal
  | otherwise = tiroDetenido

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0

type Obstaculo = Tiro -> Tiro

-- reescribimos las funciones en terminos de la funcion general 

-- a. Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la  velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.

-- esta la pasamos point free pero podriamos no hacerlo pasandole el tiro, asi es mas "lindo" porque si miramos el tipado de la general vemos que recibe 2 funciones que se la estamos pasando y despues va de Tiro -> Tiro que es el mismo tipado que esta, estan las 2 bien
tunelConRampita :: Obstaculo
tunelConRampita = obstaculoSuperableSi superaTunelConRampita efectoTunelConRampita 

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

vaAlRasDelSuelo :: Tiro -> Bool
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

--b. Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.
laguna :: Int -> Obstaculo
laguna largo = obstaculoSuperableSi superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro

-- 1) usamos otra forma de devolver una copia del data que recibio con dichos cambios, y 2) notamos que necesitamos el largo de la laguna, al no tener este dato lo agregamos en el tipado de "laguna" y lo pasamos aplicado parcialmente (igual que punto 1)
efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal { altura = altura tiroOriginal `div` largo } 

-- laguna 2 (UnTiro {precision = 85, altura = 0, velocidad = 20})
-- UnTiro {velocidad = 0, precision = 0, altura = 0}
-- laguna 2 (UnTiro {precision = 85, altura = 3, velocidad = 82})
-- UnTiro {velocidad = 82, precision = 85, altura = 1}

-- c. Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.
-- De paso para mostrar como seria sin point free 
hoyo :: Obstaculo
hoyo tiro = obstaculoSuperableSi superaHoyo efectoHoyo tiro

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && vaAlRasDelSuelo tiro

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido

-- hoyo (UnTiro {velocidad = 25, altura = 0, precision = 50})    
-- UnTiro {velocidad = 0, precision = 0, altura = 0} 



