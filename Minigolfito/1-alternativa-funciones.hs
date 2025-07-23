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

{- También necesitaremos modelar los palos de golf que pueden usarse y los obstáculos que deben enfrentar para ganar el juego.


Sabemos que cada palo genera un efecto diferente, por lo tanto elegir el palo correcto puede ser la diferencia entre ganar o perder el torneo.
a) Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro que se compone por velocidad, precisión y altura.
  i) El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
  ii) La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.
  iii) Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de 
  n-3 (con mínimo 0). Modelarlos de la forma más genérica posible.  -}

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
                    