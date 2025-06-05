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

-- 4)a)
sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = [] 
sinRepetidos (cabeza : cola) | elem cabeza cola = sinRepetidos cola  
                             | otherwise = (cabeza : sinRepetidos cola) 

{-4)b) Los barbaros son una raza muy orgullosa, tanto que quieren saber como van a ser sus descendientes y asegurarse de que los mismos reciban su legado. El descendiente de un 
barbaro comparte su nombre, y un asterisco por cada generacion. Por ejemplo: "Dave", "Dave*", "Dave**", etc. 
Ademas, tienen en principio su mismo poder, habilidades sin repetidos, y los objetos de su padre, pero anres de pasar a la siguiente generacion, utilizan (aplican sobre si mismos)
los objetos
Por ejemplo, el hijo de Dave sera equivalente a: (ardilla . libroPedKing) ("Dave", 100, ["tejer", "escribirPoesia"], [ardilla, libroPedKing]) -}
-- Respuesta:

{- Entonces la funcion descendientes hace las siguientes cosas: 1) Agregamos asterisco al nombre, 2) Aplicamos "sinRepetidos" a las habilidades, para que a a la lista de habilidades
quetienen dejarla sin habilidades repetidas y 3) Aplicar todos los objetos al hijo
Tambien esta funcion a partir de un barbaro nos da sus infinitos descendientes  -}

{- 1) Aplicar "sinRepetidos" a las habilidades: aplicar una funcion al campo "habilidadesDeBarbaro" significa simular un cambio en dicho campo, entonces para esto debemos usar el
"mapHabilidadesDeBarbaro" a la cual le pasaremos "sinRepetidos" y sin repetir logica simulara el cambio a dicho campo con esa funcion

=> mapHabilidadesDeBarbaro sinRepetidos

2) Agregar asterisco al nombre: Nuevamente esto lo podemos pensar como aplicar una funcion a un campo especifico de un barbaro, el "nombreDeBarbaro", ya que podemos pasarle la funcion
"++ String" para que concatene cierto string al final de ese campo, entonces como el paso anterior usamos "mapNombreDeBarbaro" y le pasamos la funcion con un asterisco

=> mapNombreDeBarbaro (++ "*")

3) Aplicar todos los objetos (una lista de funciones de Barbaro a Barbaro) a un barbaro: Como aca estamos hablando de listas de funciones aplicarla a un solo parametro entonces 
podemos delegarla para definir bien su cuerpo y no andar poniendo toda la logica en ese espacio, asi es mas expresivo 

=> utilizarObjetos      

Lo podemos plantear a todo esto como composicion -}
descendiente :: Barbaro -> Barbaro
descendiente unBarbaro = utilizarObjetos . mapNombreDeBarbaro (++ "*") . mapHabilidadesDeBarbaro sinRepetidos 

{- Esta funcion que delegamos de la anterior, la idea es muy parecida a lo que hicimos anteriormente, tenemos una lista de funciones y queremos que se apliquen a un parametro, la 
principal diferencia es que usar "funciones incognitas" son mucho mas amenas cuando se trata de aplicar una lista de funciones a un parametro o a una lista de parametros pero 
secuenciado con un "all" o un "any" asi estamos evaluando condiciones de las mismas

La principal diferencia con las anteriores es que aca no queremos evaluar ninguna condicion ni devolver un booleano, queremos que una lista de funciones se aplique a un barbaro (hasta
aca todo igual) PERO queremos devolver otro barbaro con las funciones aplicadas, esto nos da una idea que tenemos que usar una funcion "foldr"

"foldr" lo que hace es recibir una "funcionOperacion" una semilla y una lista, lo que hace es aplica dicha "funcionOperacion" con el ultimo elemento de la lista Y la semilla, luego 
dicho resultado se va aplicando con los demas elementos de la lista, y termina devolviendo un parametro del mismo tipo que la semilla con todo lo aplicado anteriormente

Entonces si usamos "foldr" le pasamos como "funcionOperacion" la funcion "$" que es la funcion para que se aplique una funcion, como semilla "unBarbaro" de tipo Barbaro y luego la 
lista de objetos de un barbaro "(objetosDeBarbaro unBarbaro)" tenemos todo lo necesario para que, al ultimo elemento de la lista de objetos (una lista de funciones) se aplique la
"funcionOperacion" osea "$" osea que se aplique una funcion CON la semilla "unBarbaro", osea se aplique la ultima funcion con un barbaro, luego ese resultado como es una lista de 
funciones de barbaro a barbaro, termina devolviendo un barbaro, entonces a ese resultado (un barbaro) lo recibe la anteultima funcion que aplica su logica y devuelve otro barbaro
asi hasta que se aplican todas las funciones de la lista de funciones y devuelven un ultimo barbaro (del mismo tipo que la semilla) que se le aplicaron todos los elementos de la lista -}
utilizarObjetos unBarbaro = foldr ($) unBarbaro (objetosDeBarbaro unBarbaro)









