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

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [Depto 3 80 7500 "Palermo", Depto 1 45 3500 "Villa Urquiza", Depto 2 50 5000 "Palermo", Depto 1 45 5500 "Recoleta"]

mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor funcion parametro parametro2 = funcion parametro > funcion parametro2

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor funcion parametro parametro2 = funcion parametro < funcion parametro2

ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["1", "esteVaASerElTercero", "dos"]

ubicadoEn :: [Barrio] -> Depto -> Bool
ubicadoEn barrios departamento = elem (barrio departamento) barrios

cumpleRango :: Ord a => (Depto -> a) -> a -> a -> Depto -> Bool
cumpleRango funcion cotaInferior cotaSuperior departamento = (between cotaInferior cotaSuperior . funcion) departamento

cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda departamento busqueda = all ($ departamento) busqueda

{- 3) b) Definir la funcion "buscar" que a partir de una busqueda, un criterio de ordenamiento y una lista de departamentos retorne todos aquellos que cumplen con
la busqueda ordenados en base al criterio recibio  -}

--Respuesta

{-Mirando el enunciado hasta ahora podemos plantear lo siguiente

buscar :: Busqueda -> () -> [Depto] -> [Depto]

Ahora nuestro criterio de ordenamiento, no vamos a crear un nuevo criterio de ordenamiento cuando tenemos una funcion "ordenarSegun" que YA RECIBE una "funcion
criterio" que es de la siguiente manera (a -> a -> Bool) ..., entonces podemos adaptarlo a esta funcion con lo que tenemos (Todo esto planeando que en algun 
momento usemos esta funcion)    -}

buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto] 

{- Logica de la funcion: Primero que nada de una LISTA DE DEPARTAMENTOS queremos "filtrarla" entre aquellos que CUMPLAN con una BUSQUEDA (Que es cumplir con una serie
de requerimientos) y luego a esa lista FILTRADA, ORDENARLA SEGUN algun CRITERIO (Notar que este criterio no es necesidad nuestra crearlo si no generar el espacio para
que cuando se invoque esta funcion pueda recibir una funcion que responda a un criterio de ordenamiento) -}

{- 1) Entonces como tenemos que filtrar podriamos pensar en usar la funcion filter, sabemos que filter necesita una "funcion criterio" y una lista a la cual aplicar
dicho criterio, como la "funcion criterio" debe ser booleana podemos optar por usar "cumpleBusqueda" que se FIJA EN UN DEPARTAMENTO INDIVIDUAL, pero nos viene bien
porque podemos pasarle ahora esta lista de departamentos entonces ira aplicando ese criterio a todos los elementos de la lista de departamentos

2) Luego ya con la lista de departamentos filtrada con el criterio de una busqueda buscamos ahora ordenarla segun un ordenCriterio

3) Ahora pensando todo en composicion seria la funcion principal recibe "busqueda" (lista de requerimientos) "criterioOrdenamiento" (funcion que solo le damos 
espacio para que en algun momento la pasen por parametro) y "departamentos" (lista de departamentos)

Componemos filter aplicando parcialmente la "funcion criterio" busqueda (ya que esta NO sera la lista que se le aplicara el criterio) entonces usamos `` para 
"flipear" el orden de los parametros, y le pasariamos la lista de departamentos, esto devolveria una lista con departamentos que cumplan con la condicion

Esta lista la recibe "ordenarSegun" a la cual la aplicamos parcialmente con "criterioOrdenamiento" para que cuando se llame a esta funcion y se pase por parametro
dicho criterio agarre la lista que ya cumple con la busqueda y se ordene segun ese criterio, y listo    -}

buscar busqueda criterioOrdenamiento departamento = (ordenarSegun criterioOrdenamiento . filter (`cumpleBusqueda` busqueda)) departamento

