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

saqueo :: Evento
saqueo unBarbaro = tieneHabilidad "Robar" unBarbaro  && tieneFuerzaMayorA 80 unBarbaro

tieneFuerzaMayorA :: Int -> Evento
tieneFuerzaMayorA  unaFuerza unBarbaro = fuerzaDeBarbaro unBarbaro > unaFuerza

--Misma pero usando composicion
tieneFuerzaMayorA' :: Int -> Evento
tieneFuerzaMayorA' unaFuerza unBarbaro =  ((> unaFuerza) . fuerzaDeBarbaro ) unaBarbaro

gritoDeGuerra :: Evento
gritoDeGuerra unBarbaro = poderGritoDeGuerra unBarbaro >= (cantidadDeLetrasDeHabilidades unBarbaro)
   
poderDeGritoDeGuerra :: Barbaro -> Int
poderDeGritoDeGuerra unBarbaro = 4 * (length (objetosDeBarbaro unBarbaro)) 

--Mismo pero con composicion
poderDeGritoDeGuerra' :: Barbaro -> Int
poderDeGritoDeGuerra' unBarbaro = ((*4) . length . objetosDeBarbaro) unBarbaro

cantidadDeLetrasDeHabilidades :: Barbaro -> Int
cantidadDeLetrasDeHabilidades unBarbaro = (sum . map length . objetosDeBarbaro) unBarbaro

caligrafia :: Evento
caligrafia unBarbaro = (all tieneMasDe3VocalesYEmpiezaConMayuscula . habilidadesDeBarbaro) unBarbaro

tieneMasDe3VocalesYEmpiezaConMayuscula :: String -> Bool
tieneMasDe3VocalesYEmpiezaConMayuscula unaHabilidad = tieneMasDe3Vocales unaHabilidad && empiezaConMayuscula unaHabilidad

tieneMasDe3Vocales :: String -> Bool
tieneMasDe3Vocales unaHabilidad = ((>3) . length . filter esVocal) unaHabilidad

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

empiezaConMayuscula :: String -> Bool
empiezaConMayuscula unaHabilidad = (isUpper . head) unaHabilidad

-- Mi solucion
ritualDeFechoriasPROPIA :: Evento
ritualDeFechoriasPROPIA unBarbaro = saqueo unBarbaro || gritoDeGuerra unBarbaro || caligrafia unBarbaro

-- La del ayudante 
ritualDeFechorias :: Aventura -> Evento
ritualDeFechorias unaAventura unBarbaro = pasaUnaAventura any unBarbaro unaAventura

-- ritualDeFechorias unaAventura unBarbaro = any (\unEvento -> unEvento unBarbaro) unaAventura  (Funcion anterior sin haber delegado logica)

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes unosBarbaros unaAventura = filter (\unBarbaro -> pasaUnaAventura all unBarbaro unaAventura) unosBarbaros

-- sobrevivientes unosBarbaros unaAventura = filter (\unBarbaro -> all (\unEvento -> unEvento unBarbaro) unaAventura) unosBarbaros (Funcion anterior sin haber delegado logica)

-- Aca delegamos la unica parte donde repetimos logica, que es al momento de aplicar una lista de eventos con algun criterio secuencial a UN barbaro
pasaUnaAventura funcionCriterioSecuencial unBarbaro unaAventura = funcionCriterioSecuencial (\unEvento -> unEvento unBarbaro) unaAventura


{- 4) Dinastia
A) Los barbaros se marean cuando tienen varias habilidades iguales. Por todo esto, nos piden desarrollar una funcion que elimine los elementos repetidos de una lista (sin usar "nub"
ni "nubBy")

> sinRepetidos [1,2,3,4,4,5,5,6,7]
[1,2,3,4,5,6,7] 

Nota: Puede usarse recursividad para este punto  -}
-- Respuesta:

{- Aca se nos esta probando el uso de recursividad, sabemos que la recursividad necesita de un caso base para poder cortar con la recursividad y de ahi ir desenmantelando los demas
resultados -}
-- Podemos ir haciendolo con pattern matching, ya que para filtrar esta funcion tenemos que pensar en la logica de las listas  
sinRepetidos :: (Eq a) => [a] -> [a]
-- Caso base en el cual la lista es la lista vacia, entonces devolvemos que la lista es la vacia, fin de la recursion
sinRepetidos [] = [] 

{- Caso recursivo (primero que nada la cabeza es el primer elemento, y la cola es el que le sigue, tambien hace referencia a todos los demas elementos pero lo pensamos de forma
secuencial)

Descompone usando Pattern Matching la lista en "cabeza" y "cola"

=>  sinRepetidos (cabeza : cola) ...

Luego hacemos una comparacion entre los elementos de la cabeza y de la cola, esta comparacion es el unico elemento "cabeza" hacia todos los demas de la lista, que representa "cola"
entonces dado que ese elemento cabeza se repita con alguno de los demas elementos de la lista al comparar con "cola" volvemos a llamar a la funcion de forma recursiva pasandole el
parametro cola pero sin pasarle el elemento cabeza (Aca es mas marcado el paso, pero la misma naturaleza de pasarle el parametro "cola" a la funcion recursiva significa que va 
reduciendo los elementos anteriores de a 1 para no ir comparando mismos elementos, si bien aca pareciera que se cumple eso porque por cada vez que haya uno igual vamos llamando 
recursivamente y no consideramos el elemento cabeza, algo asi hace pero naturalmente al pasarle el parametro "cola" por si solo)

=>  elem cabeza cola = sinRepetidos cola

Justamente caso contrario donde el elemento cabeza no es igual a ninguno de los demas elementos entonces se da que volvemos a llamar a la funcion de forma recursiva pero ahora si 
teniendo en cuenta el elemento cabeza (Aca notamos la naturaleza de llamar recursivamente a cola, en el anterior fuimos descartando la cabeza, pero aca la estamos teniendo en cuenta
y aun asi cada que le pasamos "cola" a la funcion va hacia el siguiente elemento dejando atras a los que ya comparamos)

=>  otherwise = (cabeza : sinRepetidos cola)  -}
sinRepetidos (cabeza : cola) | elem cabeza cola = sinRepetidos cola  
                             | otherwise = (cabeza : sinRepetidos cola) 



