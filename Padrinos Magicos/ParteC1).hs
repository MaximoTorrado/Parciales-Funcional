data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
} -- deriving Show

type Habilidad = String
type Deseo = Chico -> Chico

-- A) 1) a) aprenderHabilidades habilidades unChico : agrega una lista de habilidades nuevas a las que ya tiene el chico.
mapNombre :: (String -> String) -> Chico -> Chico
mapNombre funcionModificacion unChico = unChico { nombre = funcionModificacion . nombre $ unChico }

mapEdad :: (Int -> Int) -> Chico -> Chico
mapEdad funcionModificacion unChico = unChico { edad = funcionModificacion . edad $ unChico }

mapHabilidades :: ([Habilidad] -> [Habilidad]) -> Chico -> Chico
mapHabilidades funcionModificacion unChico = unChico { habilidades = funcionModificacion . habilidades $ unChico }

mapDeseos :: ([Deseo] -> [Deseo]) -> Chico -> Chico
mapDeseos funcionModificacion unChico = unChico { deseos = funcionModificacion . deseos $ unChico }

aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades unasHabilidades unChico = mapHabilidades (++ unasHabilidades) unChico             

{-A) 1) b) serGrosoEnNeedForSpeed unChico: dado un chico, le agrega las habilidades de jugar a todas las versiones pasadas y futuras del Need For Speed, que son: “jugar need for speed
1”, “jugar need for speed 2”, etc.   -}
serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed unChico = mapHabilidades (++ versionesPasadasNeedForSpeed) unChico

versionesPasadasNeedForSpeed :: [String]
versionesPasadasNeedForSpeed = map (\n -> "jugar need for speed" ++ show n) [1 ..]

-- A) 1) c) serMayor unChico: Hace que el chico tenga 18 años.
serMayor :: Deseo
serMayor unChico = mapEdad (const 18) unChico

-- A) 2) a) wanda: dado un chico, wanda le cumple el primer deseo y lo hace madurar (crecer un año de edad).
wanda :: Deseo 
wanda unChico = (serMayor . head (deseos unChico)) unChico

-- A) 2) b) cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de edad. Como es olvidadizo, no le concede ningún deseo.
cosmo :: Deseo
cosmo unChico = id unChico

cosmoSiNoOlvida :: Deseo
cosmoSiNoOlvida unChico = mapEdad (`div` 2) unChico

-- A) 2) c) muffinMagico: dado un chico le concede todos sus deseos.
muffinMagico :: Deseo
muffinMagico unChico = foldr ($) unChico (deseos unChico)
 
-- B) 1) a)  tieneHabilidad unaHabilidad unChico: Dado un chico y una habilidad, dice si la posee.
tieneHabilidad :: Habilidad -> Chico -> Bool
tieneHabilidad unaHabilidad unChico = (laPosee unaHabilidad . habilidades) unChico

laPosee :: Habilidad -> [Habilidad] -> Bool
laPosee unaHabilidad unasHabilidades = elem unaHabilidad unasHabilidades


-- B) 1) b) esSuperMaduro: Dado un chico dice si es mayor de edad (es decir, tiene más de 18 años) y además sabe manejar.
esSuperMaduro :: Chico -> Bool
esSuperMaduro unChico = edad unChico > 18 && sabeManejar unChico

sabeManejar :: Chico -> Bool
sabeManejar unChico = elem "Manejar" (habilidades unChico)

{- B) 2) Las chicas tienen un nombre, y una condición para elegir al chico con el que van ir al baile. Ejemplos: para Trixie la única condición es que el chico no sea Timmy, ya que 
nunca saldría con él : trixie = Chica “Trixie Tang” noEsTimmy
                       vicky = Chica “Vicky” (tieneHabilidad “ser un supermodelo noruego”) -}

type Condicion = Chico -> Bool
data Chica = Chica {
    nombreChica :: String,
    condicion :: Condicion 
} 

-- EXTRA!! Un posible planteo para "noEsTimmy"
noEsTimmy :: Chico -> Bool
noEsTimmy unChico = nombre unChico /= "Timmy"

{- B) 2) a) quienConquistaA unaChica losPretendientes: Dada una chica y una lista de pretendientes, devuelve al que se queda con la chica, es decir, el primero que cumpla con la 
condición que ella quiere. Si no hay ninguno que la cumpla, devuelve el último pretendiente (una chica nunca se queda sola). (Sólo en este punto se puede usar recursividad) -}
quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica [unChico] = unChico
quienConquistaA unaChica (chicoActual : resto) | condicion unaChica chicoActual = chicoActual
                                               | otherwise = quienConquistaA unaChica resto

{- C) Da Rules
Como no todo esta permitido en el mundo magico, Jorgen Von Strangulo esta encargado de controlar que no se viole lo establecido en "da Rules"

1) infractoresDeDaRules: Dada una lista de chicos, devuelve la lista de los nombres de aquellos que tienen deseos prohibidos. Un deseo esta prohibido si, al 
aplicarlo, entre las cinco primeras habilidades, hay alguna prohibida. En tanto, son habilidades prohibidas enamorar, matar y dominar el mundo -}
-- Respuesta:

{- La idea general del punto es 1) De una lista de chicos filtrar segun aquellos que a) Aplicar la lista de deseos a ese chico y b) Luego de aplicar cada deseo
mirar su lista de habilidades para ver si tiene alguna habilidad prohibida, y finalmente luego de tener la lista filtrada segun esta tarea, quedarnos solo con los
nombres de aquellos que cumplen esto, que son "infractores" -}

-- Planteamos primero las posibles habilidades prohibidas que compararemos de la lista de habilidades de un chico luego de aplicar el deseo
habilidadesProhibidas :: [Habilidad]
habilidadesProhibidas = ["enamorar", "matar", "dominar el mundo"]

{- Del enunciado vemos que recibe una lista de Chicos, y devuelve una lista solo de nombres de aquellos que luego de aplicar el deseo mirando sus primeras 5 
habilidades cumplen con tener alguna habilidad prohibida, entonces 
1) buscamos filtrar la lista original de chicos, "filter" necesita una "funcion criterio individual" para traducirla a la lista y devolver otra lista con el 
criterio aplicado, la cual delegaremos porque tiene que hacer mas de una cosa (aplicar la lista de deseos al chico, luego fijarse solamente 5 elementos de la lista 
de habilidades luego de aplicar el deseo, y finalmente de la lista de habilidades fijarnos si tiene alguna prohibida)
2) Con la lista de chicos ya filtrada entonces nos quedamos solamente con los nombres de cada uno, aca usaremos "map" y la "funcion criterio individual" sera 
solamente la funcion de acceso "nombre" para que de cada chico solo quedanos con el String que representa los nombres de cada uno -}
infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules unosChicos = map nombre . filter tieneDeseosProhibidos $ unosChicos

{- "tieneDeseosProhibidos" la delegamos para poder filtrar una lista de chicos, como esta es la "funcion criterio individual" del filter de arriba tiene que tener
la capacidad de 1) hacer aplicar una lista de deseos a UN chico 2) mirar la lista de habilidades luego de aplicar los deseos para ver si alguno tiene alguna 
prohibida, caso que asi sea entonces devuelve true caso contrario false

Esto nos da una idea que con que cumpla solo una (de tener una habilidad prohibida luego de aplicar la lista de deseos a un chico) ya devuelve true, lo que hace
al chico un "infractor", entonces podremos usar "any" 
"any" necesita una "funcion criterio individual" y una lista  para que se fije en la lista si alguna cumple con los elementos de esta lista, la "funcion criterio
individual" que llamamos "deseosProhibidos" la DELEGAREMOS porque esta sera la autora de 1) Aplicar UN deseo a UN chico y 2) Luego de aplicar UN deseo a UN chico
mirar 5 elementos de su lista de habilidades para ver si cumple con tener alguna prohibida o no   
Luego la logica del "any" se encargara de recibir esta funcion criterio individual que aplica a un chico en su cuerpo pero ahora con una lista de deseos, entonces
aplica toda la lista de deseos a un chico y luego mira sus 5 primeras habilidades -}
tieneDeseosProhibidos :: Chico -> Bool
tieneDeseosProhibidos unChico = any (`deseoProhibido` unChico) (deseos unChico)

{- Finalmente esta funcion que delegamos, que debe ser individual, que debe ser capaz de agarrar un deseo y un chico, lo mismo como queremos ver si tiene alguna
habilidad prohibida usamos "`elem` habilidadesProhibidas", luego la lista que debe fijarse es de aquella que sale 1) de aplicar el deseo "(unDeseo unChico)" 
2) de recibir la lista de habilidades luego de aplicar el deseo "(habilidades (unDeseo unChico))" y finalmente 3) solamente los primeros 5 elementos de la lista
de habilidades luego de aplicar el deseo "(take 5 (habilidades (unDeseo unChico)))"
Por ultimo como queremos ver si se cumple de manera secuencial para una lista de chicos usamos todo con any para conseguir el booleano que responde a lo que se
pide    -}

{- CASI TODA LA DELEGACION ES BASICAMENTE PARA LLEGAR A UN BOOLEANO QUE NECESITA EL FILTER, OSEA 2 DELEGACIONES PARA ESO -}
deseoProhibido :: Deseo -> Chico -> Bool
deseoProhibido unDeseo unChico = any (`elem` habilidadesProhibidas) (take 5 (habilidades (unDeseo unChico)))



-- Terminamos todo Honor y toda Gloria para mi Señor Jesus



