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

-- 1) a)

mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor funcion parametro parametro2 = funcion parametro > funcion parametro2

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor funcion parametro parametro2 = funcion parametro < funcion parametro2

-- 1) b) 

ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["1", "esteVaASerElTercero", "dos"]

{- 2) a) Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra en alguno de los barrios de la lista. -}

-- Respuesta

-- Ya nos estan dando el tipado a partir de el enunciado
ubicadoEn :: [Barrio] -> Depto -> Bool

{- Hasta ahora entonces lo que nos esta pidiendo basicamente es, recibir una lista de Strings que representan una lista con los nombres de los barrios, luego recibe
un data de tipo Depto, el uno de sus campos o funcion de acceso es "barrio", entonces nosotros tenemos que verificar la funcion de acceso de ese data que nos pasaron
esta contenida en la lista de barrios  -}

{- Podriamos entonces usar la funcion elem -}

ubicadoEn barrios departamento = elem (barrio departamento) barrios

{- EXTRA!!
Si quisiera usar una notacion point free podria hacer la siguiente composicion

ubicadoEn barrios departamento = (`elem` barrios) . barrio departamento => ubicadoEn barrios = (`elem` barrios) . barrio

Donde claro la funcion de acceso "barrio" recibe el departamento, que eso se reduce a un String que luego recibe elem pero "flipeado" ya que en su definicion recibe
primero el elemento y luego la lista, entonces aplicado parcialmente con la lista de barrios, y entonces recibe elem el String y se fija si esta en la lista de 
Strings (Pareciera que asi es mas de guapo)  -}