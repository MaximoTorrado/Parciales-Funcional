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
                       vicky = Chica “Vicky” (tieneHabilidad “ser un supermodelo noruego”)

Se pide definir el data Chica -}
-- Respuesta:

{- Sabemos que el data de una Chica tiene un nombre (String) y una condicion, que abstrayendo la logica debe basicamente recibir un chico y fijarse si es o no es apto para ella, esto
tambien nos damos cuenta si miramos las funciones que usan en el cuerpo de cada una, trixie usa "noEsTimmy" si bien no esta definida pareciera que toma un chico y devolveria True en
caso que no sea timmy, luego vicky usa "tieneHabilidad "ser un supermodelo noruego"" que si vemos el tipado de "tieneHabilidad" tambien recibe un chico mas una habilidad y devuelve 
un booleano (aplicado parcialmente), entonces      -}

type Condicion = Chico -> Bool
data Chica = Chica {
    nombreChica :: String,
    condicion :: Condicion 
} 

-- EXTRA!! Un posible planteo para "noEsTimmy"
noEsTimmy :: Chico -> Bool
noEsTimmy unChico = nombre unChico /= "Timmy"

{- 2) B) a) quienConquistaA unaChica losPretendientes: Dada una chica y una lista de pretendientes, devuelve al que se queda con la chica, es decir, el primero que cumpla con la 
condición que ella quiere. Si no hay ninguno que la cumpla, devuelve el último pretendiente (una chica nunca se queda sola). (Sólo en este punto se puede usar recursividad) -}
-- Respuesta: 

{- Como vamos a usar logica de recursividad en una lista entonces podemos usar Pattern Matching para ir recibiendo (cabeza : cola) o lo que seria en este caso
(chico : resto)   -}
quienConquistaA :: Chica -> [Chico] -> Chico

{- Caso base: Recibe una chica y recibe una lista de chicos con un UNICO chico, entonces ni se lo pasamos al campo condicion de la chica si no que al ser el unico
devolvemos al chico  -}
quienConquistaA unaChica [unChico] = unChico

{- Caso recursivo: Recibe una chica y ahora si recibe una lista con mas de un chico, nos enfocaremos gracias a recursividad en operar solo con el elemento cabeza
de la lista (en este caso el primer chico)
Lo que haremos ahora es llamar al campo o funcion de acceso "condicion" de la chica al cual se termina traduciendo (devolviendo) una funcion que ESPERA UN CHICO,
entonces le pasamos el elemento cabeza "chicoActual" y si se cumple la condicion entonces devolvemos dicho chico   -}
quienConquistaA unaChica (chicoActual : resto) | condicion unaChica chicoActual = chicoActual

{- Dado el caso que NO cumple con la condicion entonces lo que haremos sera volver a llamar a la funcion (usando recursividad) pero solo pasandole la cola o bien
llamado "resto", que gracias a la logica de la listas cada que se haga esto se ira procesando unicamente los que no se procesaron anteriormente -}
                                               | otherwise = quienConquistaA unaChica resto


-- b) Dar un ejemplo de consulta para una nueva chica, cuya condicion para elegir un chico es que este sepa cocinar
-- Respuesta:

{- EJEMPLO PARA VER BIEN COMO AL PASARLE UN CHICO AL CAMPO "condicion" DE LA CHICA DEVOLVERA TRUE O FALSE
1) Definición de la chica normal menos la trola de trixie:

veronica :: Chica
veronica = Chica "Veronica" (tieneHabilidad "cocinar")

Definición de los chicos
chico1 :: Chico
chico1 = Chico "Timmy" 16 ["bailar tango", "cantar"] []

chico2 :: Chico
chico2 = Chico "Juan" 17 ["jugar futbol", "cocinar", "pintar"] []

chico3 :: Chico
chico3 = Chico "Pedro" 18 ["manejar", "programar", "nadar"] []

pretendientes :: [Chico]
pretendientes = [chico1, chico2, chico3]

Notamos que una chica normal menos la yegua de trixie se define con el campo "condicion" con la funcion "tieneHabilidad" esto para que con el elemento cabeza
o "chicoActual" dicho campo "condicion" se apliquen todas las habilidades que tiene, si el elemento cabeza o "chicoActual" no tiene las habilidades que busca
entonces por recursividad va a los demas con la cola

2) La trola de trixie:

trixie = Chica “Trixie Tang” noEsTimmy 

La trola esta al definirse directamente le chupa un huevo las habilidades, puede tenerla pero la muy entregada obvia la lista de habilidades (por eso no utiliza
"tieneHabilidad"  que haria que se apliquen todas las habilidades de un chico) si no que usa "noEsTimmy" que en vez de su campo "condicion" haga buscar las 
habilidades en el campo "habilidades" del elemento cabeza o "chicoActual", va directamente a su campo "nombre" y se fija si es o no Timmy  -}