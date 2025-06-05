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

buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto] 
buscar busqueda criterioOrdenamiento departamento = (ordenarSegun criterioOrdenamiento . filter (`cumpleBusqueda` busqueda)) departamento

{- 3) c)  Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, que cumplan con:

-- Encontrarse en Recoleta o Palermo
-- Ser de 1 o 2 ambientes
-- Precio menor a $6000 por mes    -}

-- Respuesta

{- Basicamente el ejemplo sera un llamado a la funcion anterior (Esto podriamos hacerlo directamente de la consola, pero bueno es lo mismo, seria una invocacion
de la funcion pero constante) -}

{- "Ordenado por mayor superficie" de aca ya tenemos el indicio de cual sera el "criterioOrdenamiento", que podriamos traducirlo en la funcion "mayor" aplicado 
parcialmente con "superficie"

FUNDAMENTAL!! 
mayor :: Ord b => (a -> b) -> a -> a -> Bool  

Esta funcion y todas las que usan parametros "generales" ni bien se concreta que para el a se le pasa cierto tipo de parametro, y para el b cierto tipo de parametro
SE REEMPLAZAN, y por Aplicacion Parcial tienen lo necesario para saber operar, no necesitan esperar otros parametros
Nosotros si aplicamos parcialmente (mayor superficie) y reemplazamos los valores llegamos a lo siguiente

=> mayor superficie :: (Depto -> Depto -> Bool)

Que es EXACTAMENTE lo que necesita la funcion buscar buscar :: Busqueda -> ""(Depto -> Depto -> Bool)"" -> [Depto] -> [Depto]    

Entonces funciona de diez, es FUNDAMENTAL si tenemos dudas REEMPLAZAR cuando hablamos de FUNCIONES con PARAMETROS "GENERICOS" para ver a que se reducen  

ejemploDeBuscar = buscar [?] (mayor superficie) ?    

Entonces nos falta pasarle el parametro de tipo Busqueda (lista de requerimientos) y una lista de departamentos   -}

ejemploDeBuscar = buscar [ubicadoEn ["Recoleta", "Palermo"], cumpleRango ambientes 1 2, cumpleRango precio 0 6000] (mayor superficie) deptosDeEjemplo















