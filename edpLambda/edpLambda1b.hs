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

-- b) Mostrar un ejemplo de como se usaria una de estas funciones para ordenar una lista de strings en base a su longitud usando "OrdenarSegun"

-- Respuesta

{- Basicamente nos esta pidiendo que hagamos un ejemplo de una funcion que del otro lado del igual use "ordenarSegun" y tambien alguna de las otras dos, como esta
marca el camino de como debe recibir los parametros  ( ordenarSegun :: (a -> a -> Bool) -> [a] -> [a] )

-- ejemploDeOrdenarSegun = ordenarSegun (__ __) ["1", "esteVaASerElTercero", "dos"]

Osea no tenemos que estar rompiendonos la cabeza en entender como funciona "ordenarSegun" solamente enfocarnos en pasarle bien los otros 2 parametros que necesita,
ya que hasta ahora cumplimos con pasarle bien una lista ( ordenarSegun :: (a -> a -> Bool) -> ✓ -> ✓ ) Como pidio que se use una funcion creada anteriormente
entonces cumplimos con eso

-- ejemploDeOrdenarSegun = ordenarSegun (mayor __) ["1", "esteVaASerElTercero", "dos"]

Finalmente como estamos queriendo ordenar segun "longitud" lo mas probable es pasarle como segundo parametro la funcion length (lo cual tiene SENTIDO porque la 
funcion "mayor" ademas necesita una funcion criterio que termina dando un resultado "b" que se puede ordenar, en este caso sera un entero con la cantidad de letras)

-- ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["1", "esteVaASerElTercero", "dos"]

Si intentamos entender toda la logica se vuelve medio pesadazo porque hay mucha delegacion de parametros, pero basicamente lo que hace es ordenar esa lista segun
mayor cantidad de letras por elemento de la lista, si usaramos "menor" seria lo mismo pero con la menor cantidad de letras  -}

{- IMPORTANTE!!
Lo fundamental de este punto no es ponerse a intentar entender toda la logica detras, porque estamos jugando con funciones ya creadas del enunciado, pero si ver 
que parametros en un orden logico pasarlos -}
                                    
ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["1", "esteVaASerElTercero", "dos"]