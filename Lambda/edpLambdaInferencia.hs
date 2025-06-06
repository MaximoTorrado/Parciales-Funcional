{- Buscar departamentos por los medios tradicionales es una tarea compleja, ya que requiere mucho tiempo de investigación buscando en los clasificados de los diarios
y recorriendo inmobiliarias. Es por eso que hoy en día cada vez son más las personas que dejaron eso atrás dejando que internet se encargue de buscar las supuestas
mejores alternativas para sus necesidades.

Por eso surge una nueva página para buscar departamentos que permita al usuario personalizar sus propias búsquedas y de paso eventualmente mandarle mails con las 
nuevas ofertas inmobiliarias que podrían ser de su interés a ver si agarra viaje.

Tenemos los departamentos modelados de la siguiente forma: -}

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool  --Otra manera de llamarle a esta funcion, aquella que recibe un Departamento y devuelve un booleano
type Busqueda = [Requisito]  -- Una lista de funciones que reciben un dpto y devuelven un booleano

data Depto = Depto {
    ambientes :: Int,
    superficie :: Int, 
    precio :: Int, 
    barrio :: Barrio
} deriving (Show, Eq)

{- Notar que aca no estamos usando deriving, es por un campo que tiene esta data porque no hay una forma "sencilla" de comparar  -}

{- EXTRA!! 
Y en consola para construir una persona seria algo asi
> Persona "fulano@gmail.com" [[(\ depto -> ambientes dpto > 2)]]    (La cual esta bien implementada pero no podremos mostrar, y creo que ni usamos)

para confirmar esto podriamos intentar que nos muestre el mail y habra respuesta
> mail (Persona "fulano@gmail.com" [[(\ depto -> ambientes dpto > 2)]])
"fulano@gmail.com"   -}

data Persona = Persona {
    mail :: Mail, 
    busquedas :: [Busqueda] --Dijimos que Busqueda es una lista de Requisitos, entonces una persona tiene una lista de listas de funciones (Requisito)
}

{- EXTRA!! Como ejercicio de inferencia queremos tipar estas funciones para saber de donde a donde van

ordenarSegun :: ? -> [??] -> [??]
ordenarSegun :: ? -> [a] -> [a]  (Esto lo noto porque "obvia" la primer parte que no sabemos todavia que es, luego recibe la lista vacia y devuelve la lista vacia)
ordenarSegun :: (a -> ? -> Bool) -> [a] -> [a]  (notamos que "criterio" siendo compuesta con not, osea devuelve booleano, pero anteriormente esta parcialmente 
aplicada con x y como vimos anteriormente x es de tipo a) 
ordenarSegun :: (a -> a -> Bool)  (Finalmente nos damos cuenta que recibe otro parametro de tipo a porque a esa composicion luego le pasamos xs, otro elemento de la
lista que es de tipo a) -}

{- Logica: Recibe una funcion que espera 2 parametros que son 2 elementos de la lista, y una lista, entonces ordena  -}

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

{- between ::? -> ? -> ? -> ? 
   between ::? -> ? -> ? > Bool (La operacion principal es && que se reduce a un booleano)
   between ::? -> ? -> ? -> Bool 
   between ::Ord a => a -> a -> a -> Bool  (Aca notamos que minimamente los parametros tendran que ser el mismo tipo, a, y como estamos comparando, entonces todos
deben ser de la misma familia Ord)  -}

{- Logica: Recibe 3 parametros , y me dice si el segundo esta en el medio entre el primero y el tercero, osea si estan ordenados -}

between ::Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [Depto 3 80 7500 "Palermo", Depto 1 45 3500 "Villa Urquiza", Depto 2 50 5000 "Palermo", Depto 1 45 5500 "Recoleta"]