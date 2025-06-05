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
ritualDeFechorias unosEventos unBarbaro = any (\unEvento -> unEvento unBarbaro) unosEventos

--Sabiendo esto, se pide:  Definir la funcion sobrevivientes que tome una lista de barbaros y una aventura, y diga cuales barbaros la sobreviven (es decir si pasan todas las pruebas) 
-- Nota: Ojo, que ritualDeFechorias y sobrevivientes no son tan diferentes ¡No debe haber logica repetida entre estas funciones 
--Respuesta: 

{- Como en la anterior teniamos una lista de eventos (Aventura) y UN barbaro para finalmente devolver un booleano (Evento) aca tenemos una lista de eventos (Aventura) y ahora una
lista de barbaros (que en este caso termina devolviendo una lista de barbaros que sobrevivieron a estas aventuras, distinto a la anterior pero no tan diferente), entonces masomenos es
parecido, podemos primero plantear sobrevivientes y luego ver como no repetir logica y delegarla  -}

{- IMPORTANTE!! 
De arranque un comun denominador entre las dos funciones es que hay listas de funciones, entonces muy probablemente usemos funciones incognitas para poder operar con ellas    -}

{- Como lo dicho anteriormente esta funcion recibe una lista de barbaros, una aventura (Lista de eventos, de funciones que van de barbaro a booleano) y terminan devolviendo una lista
de barbaros que sobrevivieron a las aventuras, ya esto nos da una idea que vamos a patoviquear entonces usaremos "filter" 
Usando masomenos la idea anterior de la funcion incognita aca es masomenos parecido, "filter" necesita de una "funcion criterio" que sepa hacer algo de manera individual y devuelva
un booleano, pero a esta la dividiremos de la siguiente manera

1) Como primeramente recibe una lista de barbaros "unosBarbaros" entonces para nuestro "filter" la "funcion criterio" haremos una funcion incognita que de forma individual sea capaz
que un elemento de estos barbaros sea aplicado a una lista de funciones, entonces primero planteamos una funcion incognita, que su parametro "unBarbaro" sera entonces de tipo Barbaro
pues la lista que recibe primero es de este tipo
2) Luego como queremos que se cumpla todas, entonces nos da una idea que usaremos un "all", esta funcion tambien necesita de una "funcion criterio" y una lista a la cual aplicar ese
criterio, entonces aca usaremos otra funcion incognita, como ahora usaremos una lista de EVENTOS ("unaAventura") entonces nuestro parametro "unEvento" de la funcion incognita sera
de tipo Evento, entonces del otro lado de la flecha, osea el cuerpo de la funcion incognita hacemos que cada elemento de tipo Evento reciba el elemento de tipo Barbaro, y gracias
a la logica del "all" hacemos que esto se haga para toda la lista de Aventuras

Basicamente usando funciones incognitas y filtramos, terminamos desglosando de una lista de barbaros a que un parametro de tipo barbaro sea aplicado a cada elemento de tipo evento
que nace de una lista de aventuras, que esta misma tambien la desglosamos usando funciones incognitas    -}
sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes unosBarbaros unaAventura = filter (\unBarbaro -> all (\unEvento -> unEvento unBarbaro) unaAventura) unosBarbaros

-- Bueno esta solucion asi es exquisita pero estamos repitiendo logica, entonces tenemos que delegar  




{- Anotacion anterior
1) Como recibe una lista de barbaros, primeramente entonces habra una primera funcion incognita, que hara que el parametro de la funcion incognita sea entonces de tipo Barbaro, 
queremos que este parametro de tipo Barbaro sea aplicado a toda una lista de aventuras entonces ya nos damos una idea que usaremos "all"  y como queremos que un parametro se aplique
a una lista de funciones (una Aventura) entonces seguramente tendremos que usar otra funcion incognita 
2) Dentro de la misma primera funcion incognita aca podemos usar la misma logica que "ritualDeFechorias" (pero con la diferencia que ahora queremos que se cumplan todas, y no alguna)
esto nos da una idea que debemos usar "all" a la cual tambien necesita una "funcion criterio" y una lista a la cual aplicar, entonces nuestra "funcion criterio" nuevamente sera OTRA
funcion incognita que le pasaremos ahora una lista de aventuras (lista de eventos, lista de funciones de barbaro a bool) entonces esto lo que hara es que el parametro "unEvento" de
la sintaxis de la funcion incognita sea de tipo "Evento", entonces aca del lado derecho de la flecha hacemos que para cada parametro de tipo Evento de la lista de eventos (osea una
Aventura) se aplique el barbaro que recibe  -}