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
sobrevivientes unosBarbaros unaAventura = filter (\unBarbaro -> pasaUnaAventura all unBarbaro unaAventura) unosBarbaros)

-- sobrevivientes unosBarbaros unaAventura = filter (\unBarbaro -> all (\unEvento -> unEvento unBarbaro) unaAventura) unosBarbaros (Funcion anterior sin haber delegado logica)

{- Bueno en el anterior vimos la logica de todo lo que conlleva las funciones inconigta cuando tenemos que hacer que un parametro se aplique a una lista de funciones, o que una lista
de parametros se aplique a una lista de funciones, ahora con eso ya visto vamos a hacer una abstraccion de logica para no estar repitiendola -}
{- En las funciones "ritualDeFechorias" y "sobrevivientes" terminan repitiendo la logica de cuando se aplica una lista de eventos (una Aventura) a un barbaro, lo que cambia entre una
y la otra es que en la primera hacemos que sea de forma secuencial con "any" y en la segunda hacemos que sea de forma secuencial con "all" entonces delegamos esa parte  -}

-- Aca para no complicar la cosa tipando todo podemos directamente definirla, esto es valido, el motor de Haskell se encarga del tipado
pasaUnaAventura funcionCriterioSecuencial unBarbaro unaAventura = funcionCriterioSecuencial (\unEvento -> unEvento unBarbaro) unaAventura