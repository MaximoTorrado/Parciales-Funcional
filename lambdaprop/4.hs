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

-- 2.b cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función al ser aplicada con el departamento se encuentra entre los dos valores indicados.
cumpleRango :: Ord a => (Depto -> a) -> a -> a -> Depto -> Bool
cumpleRango f cotaInferior cotaSuperior departamento = between cotaInferior cotaSuperior . f $ departamento

-- 3.a Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.
cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda departamento busqueda = all (\x -> x departamento) busqueda

-- 3.b Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y una lista de departamentos retorne todos aquellos que cumplen con la búsqueda ordenados en base al criterio recibido.
buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]
buscar busqueda criterio departamentos = ordenarSegun criterio . filter (flip cumpleBusqueda busqueda) $ departamentos

-- 3.c Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, que cumplan con: 
--   i.   Encontrarse en Recoleta o Palermo
--   ii.  Ser de 1 o 2 ambientes
--   iii. Precio menor a $6000 por mes
ejemploDeBuscar :: [Depto]
ejemploDeBuscar = buscar [ubicadoEn ["Recoleta", "Palermo"], cumpleRango ambientes 1 2, cumpleRango precio 0 6000] (mayor superficie) deptosDeEjemplo 
-- ejemploDeBuscar
-- [Depto {ambientes = 2, superficie = 50, precio = 5000, barrio = "Palermo"},Depto {ambientes = 1, superficie = 45, precio = 5500, barrio = "Recoleta"}]

-- 4. Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas retorne los mails de las personas que tienen alguna búsqueda que se cumpla para el departamento dado. 
mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas departamento personas = map mail . filter (any (cumpleBusqueda departamento) . busquedas) $ personas 

-- de [Persona] quiero filtrarla en aquellas que a partir del "departamento" que recibimos si cumple con "alguna" de todas las [Busqueda] de cada persona, entonces la funcion que recibe el filter primero obtiene 
-- [Busqueda] de la persona luego como queremos ver "alguna" se la pasamos a un "any" con "cumpleBusqueda" aplicada parcialmente con "departamento" asi aplica dicho departamento a todos los elementos de [Busqueda]
-- de cada persona, con la [Persona] filtrada obtengo sus nombres mapeando con "mail"