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

-- sabemos que recibe 2 parametros: -> ->
-- vemos que opera el segundo parametro es una lista y devolvera una lista de algo, por el caso base de devolver la lista vacia y como en el caso recursivo concatena con "++" : -> [a] -> [a] 
-- con "criterio" vemos que recibe dos elementos que son la cabeza y la cola de la lista : (a -> a -> ??) -> [a] -> [a]
-- mirando luego que en el filter le pasamos el "criterio" aplicado parcialmente con "x", al mismo lo NIEGA con "not" entonces "criterio" debe devolver un booleano
ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
 (ordenarSegun criterio . filter (not . criterio x)) xs ++
 [x] ++
 (ordenarSegun criterio . filter (criterio x)) xs

-- luego de inferir vemos que recibe una funcion que recibe elementos de la lista que le mandamos, y la ordena segun si el primer parametro va primero que el segundo parametro al ser aplicado por la funcion

-- recibe 3 parametros : -> -> ->
-- la operacion principal es "&&" entonces debe devolver un booleano : -> -> -> Bool
-- como a "valor" lo terminamos COMPARANDO con "cotaSuperior" y con "cotaInferior" sabemos entonces 1) los 3 deben ser del mismo tipo 2) dicho tipo al haber comparaciones debe ser englobado en la familia "Ord"
between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor =
 valor <= cotaSuperior && valor >= cotaInferior

{- Se pide desarrollar las siguientes funciones y consultas de modo que se aprovechen tanto como sea posible los conceptos de orden superior, aplicación parcial y composición.

1.a Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar esa función sobre el primer valor es mayor o menor que el resultado de evaluarlo sobre el segundo valor
respectivamente.
1.b Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.

2. Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
2.a ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra en alguno de los barrios de la lista.
2.b cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función al ser aplicada con el departamento se encuentra entre los dos valores indicados.

3.a Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.
3.b Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y una lista de departamentos retorne todos aquellos que cumplen con la búsqueda ordenados en base al criterio recibido.
3.c Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, que cumplan con: 
 i. Encontrarse en Recoleta o Palermo
 ii. Ser de 1 o 2 ambientes
 iii. Precio menor a $6000 por mes

4. Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas retorne los mails de las personas que tienen alguna búsqueda que se cumpla para el departamento dado. -}