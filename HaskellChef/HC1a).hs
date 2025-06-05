{- Desde una importante cadena televisiva nos pidieron modelar un programa (cuak) para ver si sus originales ideas tienen sentido. Si bien el formato nos resulto un poco trillado nos
dijeron que la idea es totalmente nueva -}

-- Parte A

{- Nuestro programa tendra participantes que cuentan con nombre, trucos de cocina y un plato que es su especialidad. Los platos, a su vez, tienen una dificultad que va de 0 a 10 y
un conjunto de componentes que nos indican sus ingredientes con sus respectivos pesos en gramos

Algunos de los trucos más famosos son:

endulzar: dada una cantidad de gramos de azúcar, le agrega ese componente a un plato.   -}
-- Respuesta: 
type Truco = Plato -> Plato

data Participante = Participante {
    nombre :: String,
    trucos :: [Truco],
    especialidad :: Plato
} 

type Ingrediente = String
type Peso = Int
type Componente = (Ingrediente, Peso)

data Plato = Plato {
  dificultad :: Int,
  componentes :: [Componente]
} deriving Show

-- a) endulzar: dada una cantidad de gramos de azúcar, le agrega ese componente a un plato.
-- Respuesta: 



