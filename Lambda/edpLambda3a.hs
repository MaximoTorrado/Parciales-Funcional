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

-- 1)

mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor funcion parametro parametro2 = funcion parametro > funcion parametro2

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor funcion parametro parametro2 = funcion parametro < funcion parametro2

-- 1) b)

ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["1", "esteVaASerElTercero", "dos"]

-- 2) a)

ubicadoEn :: [Barrio] -> Depto -> Bool
ubicadoEn barrios departamento = elem (barrio departamento) barrios

-- 2) b)

cumpleRango :: Ord a => (Depto -> a) -> a -> a -> Depto -> Bool
cumpleRango funcion cotaInferior cotaSuperior departamento = (between cotaInferior cotaSuperior . funcion) depto

{- 3) a) Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.  -}

{- Basicamente me van a dar una sola busqueda (Que es una lista de requisitos, una lista de funciones que reciben un depto y devuelve un booleano) y un depto
entonces lo que pide es "si cumple que TODOS los REQUISITOS de una BUSQUEDA se VERIFICAN para un DEPTO DADO" osea, usaremos la funcion all para que en la lista de
funciones que significa una busqueda se evaluen todas en el departamento que le pasemos     -}

cumpleBusqueda :: Depto -> Busqueda -> Bool

{- IMPORTANTE!!
¿Cuando usar ($ a) funcionAplicar? Cuando queremos usar ese parametro para esa funcion especifica, y si hablamos de una lista de funciones que reciben todos como
primer parametro ese tipo a y queremos hacer que se apliquen ahi de manera secuencial (Anidado con una funcion que haga eso ademas como un "all" o "any") podemos 
hacerlo ya nos aseguramos que se use ese parametro para esa funcion

FUNDAMENTAL!! 
Ademas cuando pensamos en una funcion que recibe una lista de funciones que recien un parametro a y devuelven algo (Potencialmente un Bool), y un parametro de tipo 
a, la mejor manera de apuntar a que con ese parametro se efectue de manera secuencial en toda la lista de funciones seria con un all o un any mas 1) Expresion
lambda o 2) Usando ($ a) que me parece mucho mas linda porque es "hacemos que se haga estas funciones listo" (Esto porque ingresar a una lista de funciones no es 
del todo regalado) -}

cumpleBusqueda departamento busqueda = all ($ departamento) busqueda
-- cumpleBusqueda' departamento = all ($ departamento)

{- Con expresion lambda
 
-- cumpleBusqueda'' departamento busqueda = (\requisito -> ...) 

Siendo que busqueda es una lista de requisitos, un elemento de esa lista es UN requisito, ahora viendo el tipo de requisito vemos que da de Depto a Bool, entonces
quiero ver que ese requisito al aplicarse al "departamento" se cumple o no

-- cumpleBusqueda'' departamento busqueda = (\requisito -> requisito departamento)

Finalmente hasta ahora tengo como se aplicar individualmente para un departamento ,como quiero que se haga secuencialmente para toda una lista de requisitos entonces
uso all y le paso la lista donde estan todas las funciones que queremos que haga eso ("busqueda")

-- cumpleBusqueda'' departamento busqueda = all (\requisito -> requisito departamento) busqueda

A eso tambien podemos aplicar point free pero estariamos  -}

{- EXTRA!!
Una forma que nos da este profesor para intentar pensar las funciones para directamente hacerlas mas cancheras y con point free es pensar la salida como una funcion
gracias a la currificacion

cumpleBusqueda :: Depto -> (Busqueda -> Bool)

Pero como todo es opcional, directamente podriamos pensar la solucion que consideremos y luego reescribirla si queremos, o no -}
