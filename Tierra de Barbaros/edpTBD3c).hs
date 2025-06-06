import Data.Char (toUpper)

type Objeto = Barbaro -> Barbaro  -- Este type representa una funcion que recibe un barbaro y devuelve otro barbaro

data Barbaro = Barbaro {
    nombreDeBarbaro :: String,
    fuerzaDeBarbaro :: Int,
    habilidadesDeBarbaro :: [String],
    objetosDeBarbaro :: [Objeto]         -- Entonces aca sera una lista de funciones que van de barbaro a barbaro
} -- deriving Show

-- Accesors "De cambio"
mapNombreDeBarbaro :: (String -> String) -> Barbaro -> Barbaro
mapNombreDeBarbaro funcionModificacion unBarbaro = unBarbaro { nombreDeBarbaro = (funcionModificacion . nombreDeBarbaro) unBarbaro }

mapFuerzaDeBarbaro :: (Int -> Int) -> Barbaro -> Barbaro
mapFuerzaDeBarbaro funcionModificacion unBarbaro = unBarbaro { fuerzaDeBarbaro = (funcionModificacion . fuerzaDeBarbaro) unBarbaro }

mapHabilidadDeBarbaro :: ([String] -> [String]) -> Barbaro -> Barbaro
mapHabilidadDeBarbaro funcionModificacion unBarbaro = unBarbaro { habilidadesDeBarbaro = (funcionModificacion . habilidadesDeBarbaro) unBarbaro }

mapObjetosDeBarbaro :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
mapObjetosDeBarbaro funcionModificacion unBarbaro = unBarbaro { objetosDeBarbaro = (funcionModificacion . objetosDeBarbaro) unBarbaro }

--1) a) Las espadas aumentan la fuerza de los barbaros en 2 unidades por cada kilogramo de peso
espadas :: Int -> Objeto
espadas pesoDeEspada unBarbaro = mapFuerzaDeBarbaro ((+) (pesoDeEspada * 2)) unBarbaro 

agregarHabilidad :: String -> Objeto
agregarHabilidad unaHabilidad unBarbaro = mapHabilidadDeBarbaro (++ [unaHabilidad]) unBarbaro

--1) b) Los "amuletosMisticos" puerco-marranos otorgan una habilidad dada a un barbaro
amuletosMisticos :: String -> Objeto
amuletosMisticos unaHabilidad unBarbaro = agregarHabilidad "Hacer Magia" unBarbaro

--1) c) Las "varitasDefectuosas" añaden la habilidad de hacer magia, pero desaparecen todos los demas objetos del barbaro 
varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = (agregarHabilidad "Hacer Magia" . mapObjetosDeBarbaro (const [])) unBarbaro

varitasDefectuosas' :: Objeto
varitasDefectuosas' unBarbaro = unBarbaro { habilidadesDeBarbaro = habilidadesDeBarbaro unBarbaro ++ ["Hacer Magia"], objetosDeBarbaro = [] }

varitasDefectuosas'' :: Objeto
varitasDefectuosas'' unBarbaro = (agregarHabilidad "Hacer Magia" . desaparecerObjetos) unBarbaro

desaparecerObjetos :: Objeto
desaparecerObjetos unBarbaro = unBarbaro { objetosDeBarbaro = [varitasDefectuosas] }

--1) d) Una ardilla, que no hace nada
ardilla :: Objeto
ardilla = id

--1) e) Una cuerda, que combina dos objetos distintos, obteniendo uno que realiza las transformaciones de los otros dos
cuerda :: Objeto -> Objeto -> Objeto
cuerda unaFuncion otraFuncion = unaFuncion . otraFuncion

--2) El megafono es un objeto que potencia al barbaro, concatenando sus habilidades y poniendolas en mayusculas
megafono :: Objeto
megafono unBarbaro = mapHabilidadDeBarbaro (ponerEnMayuscula . concatenar) unBarbaro

concatenar :: [String] -> [String]
concatenar unaHabilidad = [concat unaHabilidad]

ponerEnMayuscula :: [String] -> [String]
ponerEnMayuscula unaHabilidad = map (map toUpper) unaHabilidad

-- 3) a) invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”   

type Aventura = [Evento]
type Evento = Barbaro -> Bool

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes unBarbaro = tieneHabilidad "Escribir Poesía Atroz" unBarbaro

tieneHabilidad :: String -> Evento
tieneHabilidad unaHabilidad unBarbaro = elem unaHabilidad (habilidadesDeBarbaro unBarbaro)

--Misma pero usando composicion
tieneHabilidad':: String -> Evento
tieneHabilidad' unaHabilidad unBarbaro = (elem unaHabilidad . habilidadesDeBarbaro) unBarbaro

-- 3) b) cremalleraDelTiempo: Un bárbaro sobrevive si no tiene pulgares. Los bárbaros llamados Faffy y Astro no tienen pulgares, los demás sí. 
-- Mi solucion
cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = seLlama "Faffy" unBarbaro || seLlama "Astro" unBarbaro 

seLlama :: String -> Evento
seLlama unNombre unBarbaro = nombreDeBarbaro unBarbaro == unNombre

-- Otra solucion usando Pattern Matching
cremalleraDelTiempo' :: Evento
cremalleraDelTiempo' unBarbaro = (not . tienePulgares . nombreDeBarbaro) unBarbaro

tienePulgares :: String -> Bool
tienePulgares "Faffy" = False
tienePulgares "Astro" = False
tienePulgares _ = True

-- 3) c) ritualDeFechorias: Un bárbaro puede sobrevivir si pasa una o más pruebas como las siguientes: 
  -- saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.
  -- gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letras de sus habilidades. El poder necesario para aprobar es 4 veces la cantidad de 
  -- objetos del bárbaro.
  -- caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) si sus habilidades contienen más de 3 vocales y comienzan con mayúscula.
-- Respuesta:

{- saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.
Entonces recibe un barbaro devuelve un booleano (Es un Evento) podemos usar la funcion que definimos anteriormente "tieneHabilidad" y crearemos otra "tieneFuerzaMayorA" -}
saqueo :: Evento
saqueo unBarbaro = tieneHabilidad "Robar" unBarbaro  && tieneFuerzaMayorA 80 unBarbaro

{- "tieneFuerzaMayorA" la creamos por las dudas tengamos que repetir logica en algun momento, que esta ultima recibira un barbaro, un entero y devuelve un booleano, dado el caso que
el entero que devuelve llamar la funcion de acceso "fuerzaDeBarbaro" de ese barbaro con el entero que recibe (En este caso comparamos si es mayor) es mayor  entonces devuelve true,
caso contrario no -} 
tieneFuerzaMayorA :: Int -> Evento
tieneFuerzaMayorA  unaFuerza unBarbaro = fuerzaDeBarbaro unBarbaro > unaFuerza

--Misma pero usando composicion
tieneFuerzaMayorA' :: Int -> Evento
tieneFuerzaMayorA' unaFuerza unBarbaro =  ((> unaFuerza) . fuerzaDeBarbaro ) unaBarbaro

{-gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letras de sus habilidades. El poder necesario para aprobar es 4 veces la cantidad de objetos
del bárbaro. -}
{- Entonces como vinimos haciendo hasta ahora delegamos el problema en partes pequeñas y para que sea mas expresivo antes de definir el cuerpo de las mismas, entonces vemos que sera 
una comparacion nomas entre "poderDeGritoDeGuerra" con "cantidadDeLetrasDeHabilidades", notamos tambien que "poderDeGritoDeGuerra" es 4 veces la cantidad de objetos del barbaro  -}
gritoDeGuerra :: Evento
gritoDeGuerra unBarbaro = poderGritoDeGuerra unBarbaro >= (cantidadDeLetrasDeHabilidades unBarbaro)

--"poderDeGritoDeGuerra" recibe un barbaro a ese barbaro multiplica por 4 lo que da la el "length" de la lista de objetos del mismo (usando la funcion de acceso "objetosDeBarbaro")   
poderDeGritoDeGuerra :: Barbaro -> Int
poderDeGritoDeGuerra unBarbaro = 4 * (length (objetosDeBarbaro unBarbaro)) 

--Mismo pero con composicion
poderDeGritoDeGuerra' :: Barbaro -> Int
poderDeGritoDeGuerra' unBarbaro = ((*4) . length . objetosDeBarbaro) unBarbaro

{- Para sacar la cantidad de letras de una lista de strings pensamos en usar una "funcion criterio" individual capaz de sacar la cantidad de letras de UN elemento de la lista, es
"length" luego a esta se la tenemos que aplicar a toda una lista de string, entonces usamos "map length "unaLista" ", luego a eso lo sumamos,, usamos composicion   -}
cantidadDeLetrasDeHabilidades :: Barbaro -> Int
cantidadDeLetrasDeHabilidades unBarbaro = (sum . map length . objetosDeBarbaro) unBarbaro

-- caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) si sus habilidades contienen más de 3 vocales y comienzan con mayúscula. (Significa que
-- sus habilidades deben cumplir con este estandar)
{- Este dato importante que nos da el video nos termina de dar a entender que debemos usar "all" con una "funcion criterio" que sea capaz de denotar si UNA habilidad tiene mas de 3
vocales y comienzan con mayuscula, para aplicarla a una lista de habilidades de un barbaro, lo planteamos con composicion -}
caligrafia :: Evento
caligrafia unBarbaro = (all tieneMasDe3VocalesYEmpiezaConMayuscula . habilidadesDeBarbaro) unBarbaro

{- Notamos que esta funcion que estamos delegando de la anterior, nuestra "funcion criterio" INDIVIDUAL, hace mas de una cosa, entonces podemos delegarla tambien para ser mas 
declarativos -}
tieneMasDe3VocalesYEmpiezaConMayuscula :: String -> Bool
tieneMasDe3VocalesYEmpiezaConMayuscula unaHabilidad = tieneMasDe3Vocales unaHabilidad && empiezaConMayuscula unaHabilidad

{- Podemos aca entonces a partir de UN string, filtrar los elementos de dicha cadena con aquellos que solamente sean VOCALES (Aca podemos delegar para no aplicar la logica ahi y ser
mas declarativos), luego de haber filtrado (sabemos que "filter" usa una funcion criterio QUE ES BOOLEANA para patoviquear la lista y devolver la lista patoviqueada) y teniendo esa 
lista podemos fijarnos entonces si su longitud es mayor a 3, lo planteamos con composicion    -}
tieneMasDe3Vocales :: String -> Bool
tieneMasDe3Vocales unaHabilidad = ((>3) . length . filter esVocal) unaHabilidad

{- "esVocal" que la estamos delegando de la funcion anterior, debe ser capaz de identificar si UN string INDIVIDUAL tiene vocales o no, el filter filtrara en base a si es vocal o no
para esto usaremos "elem" que recibe el string para fijarse "aeiouAEIOU" y luego el string, esto es posible porque un String es en realidad un alias para [Char]  -}
esVocal :: String -> Bool
esVocal unaHabilidad = elem "aeiouAEIOU" unaHabilidad

-- Otra solucion usando pattern matching, solo considerando las vocales despues de una supuesta mayuscula
esVocal' :: String -> Bool
esVocal 'a' = True
esVocal 'e' = True
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal _ = False

{- "empiezaConMayuscula" que la estamos delegando tiene que ser capaz de ver si para UN string su primera letra es mayuscula, como String es un alias de [Char], entonces podemos usar
la funcion "head" que devuelve el primer caracter de una lista, luego usando una funcion predefinida "isUpper" que literalmente es fijarse si un char es mayuscula o no, devolvemos si
es o no el primer caracter una mayuscula, planteamos todo con composicion   -}
empiezaConMayuscula :: String -> Bool
empiezaConMayuscula unaHabilidad = (isUpper . head) unaHabilidad

{------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Mi solucion para "ritualDeFechorias" 
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------}

{- Basicamente yo lo que entendi (que no es muy diferente de la solucion que proponen), primero que recibe un barbaro y devuelve un boolosea sigue siendo un evento, luego basicamente
esta funcion es una que une todas las anteriores, y en el caso que UNA se cumpla entonces podemos constatar que dicho barbaro sobrevivio 
Limitacion: Esta "harcodeada" para que trabaje unicamente con los eventos definidos anteriormente, si quisieramos agregar uno deberiamos agregarlo aca cambiando el codigo  -}
ritualDeFechoriasPROPIA :: Evento
ritualDeFechoriasPROPIA unBarbaro = saqueo unBarbaro || gritoDeGuerra unBarbaro || caligrafia unBarbaro

{------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
La que propone el ayudante (Esta buena porque demostramos que sabemos usar expresiones lambda o funciones incognitas)
Ventaja: No esta limitada a un conjunto de eventos si no que se extiende a cualquier evento anexado en una lista de eventos
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------}
{- Recibe una lista de eventos (una aventura), y termina siendo un evento ya que recibe un barbaro para que dicho parametro se pase a cada elemento de la lista de eventos
IMPORTANTE!! ya como en puntos anteriores siempre que hablemos de listas de funciones podriamos considerar usar expresiones lambda o "funciones incognitas"
como vimos seria que se cumpla "alguna" de esas funciones con el unico parametro barbaro que recibe, entonces ya nos da una idea de usar "any" a la cual le pasamos la expresion lambda
y la lista de eventos (La logica bien bien de esto la priorizaria cuando lo escriba a mano, pero masomenos me doy una idea)  -}
ritualDeFechorias :: Aventura -> Evento  -- Aventura = [Evento]

{- Aca todo se anexa con todo, primero any necesita una "funcion criterio" y una lista a aplicar, como la lista que le estamos pasando es de tipo [Evento] osea cada elemento de la 
lista es de tipo Evento, any espera que la "funcion criterio" DEBE operar con el tipo de los elementos de la lista, entonces de ahi el parametro "evento" (que ya es una sintaxis 
propia de la funcion incognita) reconoce que es de tipo Evento, osea del mismo tipo que los elementos de la lista "unosEventos", entonces despues del otro lado de la flechita (que es
el cuerpo de la funcion incognita) hacemos que cada parametro osea cada elemento de tipo Evento reciba un barbaro, y luego la logica del any hace que se haga para todos los elementos
de la lista que recibe, y de ahi saca una booleano  -}
ritualDeFechorias unosEventos unBarbaro = any (\unEvento -> unEvento unBarbaro) unosEventos







