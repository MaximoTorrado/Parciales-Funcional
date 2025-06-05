{- Desde una importante cadena televisiva nos pidieron modelar un programa (cuak) para ver si sus originales ideas tienen sentido. Si bien el formato nos resulto un poco trillado nos
dijeron que la idea es totalmente nueva -}

-- Parte A

{- Nuestro programa tendra participantes que cuentan con nombre, trucos de cocina y un plato que es su especialidad. Los platos, a su vez, tienen una dificultad que va de 0 a 10 y
un conjunto de componentes que nos indican sus ingredientes con sus respectivos pesos en gramos

Algunos de los trucos más famosos son:

endulzar: dada una cantidad de gramos de azúcar, le agrega ese componente a un plato.   -}
-- Respuesta: 

{- Primero podemos arrancar modelando nuestros participantes viendo que tienen cada uno, en este caso voy a intentar para las funciones de acceso usar nombres mas cortitos sin agregar
un "DeX" para que cuando las tengamos que invocar o cuando creemos los "mapX" no tengamos un nombre gigante  -}

{- De arranque notamos que un plato tiene mas cosas (dificultad y un conjunto de componentes, que al mismo tiempo un componente se representa con un (ingrediente, peso)) entonces
como no es que tiene 2 cosas nomas que podemos representar en una tupla, porque si no seria tupla con una tupla adentro refiriendonos al conjunto de componentes, entonces mejor usamos
un data para representar el plato, y dentro el campo componentes usariamos ahi si una tupla (entonces cuando queremos representar tipos de campos que tienen mas de una cosa y esa cosa
tiene mas de una cosa es mejor usar una tupla como rama, osea a lo ultimo, si fuera que tenemos una cosa que representa a dos cosas ahi si nos ahorrariamos del data y usamos tupla sola)    -}

{- Luego notamos mirando las funciones que representan los trucos, basicamente son funciones que reciben un plato y devuelven otro plato pero con el truco aplicado, entonces un truco
recibe un plato devuelve un plato, y trucos seria una lista de funciones que reciben un plato y devuelven un plato 
Ya notando esto cuando leemos "un plato que es su especialidad" vemos que el campo es "especialidad" y es de tipo plato, pensandolo como un data seria al entrar al campo especialidad
de un participante nos devuelve su plato que es su especialidad (Es mejor pensarlo asi para notar bien cuales son los campos de cada uno)  -}
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

