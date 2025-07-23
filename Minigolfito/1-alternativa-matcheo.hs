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
  n-3 (con mínimo 0). Modelarlos de la forma más genérica posible  -}

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
palos :: [Habilidad -> Tiro]
palos = [palo "putter" 0, palo "madera" 0] ++ [palo "hierro" n | n <- [1..10]]
