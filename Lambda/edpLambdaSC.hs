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

cumpleRango :: Ord a => (Depto -> a) -> a -> a -> Depto -> Bool
cumpleRango funcion cotaInferior cotaSuperior departamento = (between cotaInferior cotaSuperior . funcion) departamento

cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda departamento busqueda = all ($ departamento) busqueda
