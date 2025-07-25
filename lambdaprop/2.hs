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
   busquedas :: [Busqueda] 
}  

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

-- 1.a Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar esa función sobre el primer valor es mayor o menor que el resultado de evaluarlo sobre el segundo 
-- valor respectivamente.
mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor f valor1 valor2 = f valor1 > f valor2

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor f valor1 valor2 = f valor1 < f valor2

-- 1.b Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.
ejemploDeOrdenarSegun :: [[Char]]
ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["1", "esteVaASerElTercero", "dos"]

-- 2. Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
-- 2.a ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra en alguno de los barrios de la lista.

ubicadoEn :: [Barrio] -> Depto -> Bool
ubicadoEn barrios departamento = flip elem barrios . barrio $ departamento

ubicadoEn' :: [Barrio] -> Depto -> Bool
ubicadoEn' barrios departamento = (`elem` barrios) . barrio $ departamento

-- un departamento tiene un atributo "barrio" entonces queremos conseguir dicho String para luego ver si esta dentro de la [Barrio] = [String], vimos que elem recibe primero elemento luego lista, entonces flipeamos

-- 2.b cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función al ser aplicada con el departamento se encuentra entre los dos valores indicados.
cumpleRango :: Ord a => (Depto -> a) -> a -> a -> Depto -> Bool
cumpleRango f cotaInferior cotaSuperior departamento = between cotaInferior cotaSuperior . f $ departamento

-- queremos aplicar la funcion con el departamento para que lo que retorne lo comparemos si esta entre medio de los otros 2 valores del mismo tipo, si queremos ver si esta "entre" usamos "between", si no la tuvieramos 
-- la partiriamos para definir despues




















