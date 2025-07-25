{- Buscar departamentos por los medios tradicionales es una tarea compleja, ya que requiere mucho tiempo de investigación buscando en los clasificados de los diarios y recorriendo inmobiliarias. Es por eso que hoy en día cada
 vez son más las personas que dejaron eso atrás dejando que internet se encargue de buscar las supuestas mejores alternativas para sus necesidades.
Por eso surge una nueva página para buscar departamentos que permita al usuario personalizar sus propias búsquedas y de paso eventualmente mandarle mails con las nuevas ofertas inmobiliarias que podrían ser de su interés a 
ver si agarra viaje.
Tenemos los departamentos modelados de la siguiente forma: -}

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
 ambientes :: Int,
 superficie :: Int,
 precio :: Int,
 barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
   mail :: Mail,
   busquedas :: [Busqueda]  -- aca no usamos el deriving porque no hay una forma "sencilla" de hacer mostrar [Busqueda] que es una lista de lista de funciones
}  --[Busqueda] -> [[Requisito]] -> [[Depto -> Bool, ... Depto -> Bool],..., [Depto -> Bool, ..., Depto -> Bool]] 

-- Datos de ejemplo
deptosDeEjemplo = [
 Depto 3 80 7500 "Palermo",
 Depto 1 45 3500 "Villa Urquiza",
 Depto 2 50 5000 "Palermo",
 Depto 1 45 5500 "Recoleta"]

-- Codigo base

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
 (ordenarSegun criterio . filter (not . criterio x)) xs ++
 [x] ++
 (ordenarSegun criterio . filter (criterio x)) xs

between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor =
 valor <= cotaSuperior && valor >= cotaInferior

-- Se pide desarrollar las siguientes funciones y consultas de modo que se aprovechen tanto como sea posible los conceptos de orden superior, aplicación parcial y composición.

-- 1.a Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar esa función sobre el primer valor es mayor o menor que el resultado de evaluarlo sobre el segundo 
-- valor respectivamente.

mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor f valor1 valor2 = f valor1 > f valor2

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor f valor1 valor2 = f valor1 < f valor2

-- lo que inferimos de ambas es que recibe una funcion y dos valores, los valores deben ser del mismo tipo, la funcion debe recibir ese tipo "a" y devolver un cualquiera "b", como los resultados de tipo "b" van a ser 
-- comparados por menor y por mayor entonces los englobamos en familia "Ord" y finalmente devuelve booleano en caso de ser o no ser mayor o menor 

-- 1.b Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.

ejemploDeOrdenarSegun :: [[Char]]
ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["1", "esteVaASerElTercero", "dos"]
-- ejemploDeOrdenarSegun 
-- ["esteVaASerElTercero","dos","1"]

-- ordenarSegun (mayor length) ["1","esteVaASerElTercero","dos"]
-- ["esteVaASerElTercero","dos","1"]

-- esto es algo que hice para entender como laburaba "ordenarSegun", y basicamente "mayor" tiene el tipado que necesita "ordenarSegun" si es aplicado parcialmente con una funcion (a -> b), hice esta como para no aplicar 
-- parcialmente y es lo mismo, pero bueno no es generica y lo que tiene de bueno "mayor" y "menor" es que la podemos aplicar parcialmente con cualquier funcion unaria booleana y labura bien con "ordenarSegun"

mayorLength :: String -> String -> Bool
mayorLength valor1 valor2 = length valor1 > length valor2

-- ordenarSegun (mayorLength) ["1","esteVaASerElTercero","dos"] 
-- ["esteVaASerElTercero","dos","1"]