import Text.Show.Functions
import Data.Char (isUpper, toUpper)

{- 1. Se sabe que los bárbaros tienen nombre, fuerza, habilidades y objetos, que los ayudarán más adelante en su lucha contra el mal. Por ejemplo: 
dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas] -}

data Barbaro = Barbaro {
  nombre :: String,
  fuerza :: Int, 
  habilidades :: [String],
  objetos :: [Objeto] 
}

type Objeto = Barbaro -> Barbaro
-- Como vemos abajo los "Objeto" son funciones que reciben un barbaro y devuelven un barbaro despues de aplicar dicho objeto

-- luego tambien como vemos que los "Objetos" se basan en devolver un barbaro pero con ciertos atributos de su data cambiado, entonces planteamos los "accesors" a cada atributo para no repetir logica cada que devolvemos un barbaro con los atributos cambiados
mapNombre :: (String -> String) -> Barbaro -> Barbaro
mapNombre f barbaro = barbaro { nombre = f . nombre $ barbaro}

mapFuerza :: (Int -> Int) -> Barbaro -> Barbaro
mapFuerza f barbaro = barbaro { fuerza = f . fuerza $ barbaro}

mapHabilidades :: ([String] -> [String]) -> Barbaro -> Barbaro
mapHabilidades f barbaro = barbaro { habilidades = f . habilidades $ barbaro}

mapObjetos :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
mapObjetos f barbaro = barbaro { objetos = f . objetos $ barbaro}

-- Se pide definir los siguientes objetos y definir algunos bárbaros de ejemplo
-- 1. Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.
espada :: Int -> Objeto 
espada peso barbaro = mapFuerza (+ peso * 2) barbaro
-- se supone que cada espada tiene un peso, entonces recibe un peso el barbaro y usa "mapFuerza" que recibe la funcion "(+ peso * 2)" que es sumar el doble del peso, y a dicho barbaro

-- 2. Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.
amuletoMistico :: String -> Objeto
amuletoMistico habilidad = agregarHabilidad habilidad

-- amuletoMistico habilidad barbaro = mapHabilidades (++ [habilidad]) barbaro       (REFACTOR)
-- recibe la habilidad y el barbaro, y como habilidades es [Habilidad] una lista de string, podemos concatenar otra lista cargada con dicha habilidad que es un string, a dicho barbaro

-- 3. Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.
varitaDefectuosa :: Objeto
varitaDefectuosa barbaro = (agregarHabilidad "HacerMagia" . desaparecerObjetos) barbaro  

--varitaDefectuosa barbaro = (mapHabilidades (++ ["hacerMagia"]) . desaparecerObjetos) barbaro  (REFACTOR)
-- entre esta y la funcion anterior notamos una repeticion de logica al agregar una habilidad usando "mapHabilidad" entonces podemos generalizar lo conocido y parametrizar lo distinto en "agregarHabilidad"
agregarHabilidad :: String -> Barbaro -> Barbaro
agregarHabilidad habilidad barbaro = mapHabilidades (++ [habilidad]) barbaro

desaparecerObjetos :: Objeto 
desaparecerObjetos barbaro = barbaro { objetos = [varitaDefectuosa]}

-- primero el "desaparecerObjetos" se puede interpretar como dejar sin elementos a la lista de objetos, o dejarla solo con "varitaDefectuosa" ambos siempre usando esta sintaxis de  generar un cambio en un atributo, luego para agregar una habilidad hacemos lo mismo que el punto anterior ya que es constante que se agrega ese unico String "hacerMagia"

-- 4. Una ardilla, que no hace nada.
ardilla :: Objeto
ardilla = id

-- 5. Una cuerda, que combina dos objetos distintos,obteniendo uno que realiza las transformaciones de los otros dos. 
cuerda :: Objeto -> Objeto -> Objeto
cuerda objeto objeto2 = objeto . objeto2  

-- basicamente nos esta diciendo que es la composicion entre ambas funciones

{- 2. El megafono es un objeto que potencia al bárbaro, concatenando sus habilidades y poniéndolas en mayúsculas. 

dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]
*Main> megafono dave
Barbaro "Dave" 100 ["TEJERESCRIBIRPOESIA"] [<function>,<function>]

Sabiendo esto, definir al megafono, y al objeto megafonoBarbarico, que está formado por una cuerda, una ardilla y un megáfono. Recuerden que esta la función toUpper que toma una Char y devuelve un Char pero en mayúscula
Main> toUpper ‘a’  -}

megafono :: Objeto
megafono barbaro = mapHabilidades (ponerEnMayuscula . concatenar) barbaro

concatenar :: [String] -> [String]
concatenar habilidades = [concat habilidades]

-- recordemos que "mapHabilidades" ya tiene acceso a el atributo "habilidades" de cada data Barbaro, entonces solo espera una funcion que afecte dicho atributo, como queremos concatenar
-- usamos "concat" ahora esta funcion devuelve un string pero nosotros queremos concatenar los strings y mantenerlos en una lista, entonces lo encerramos en []

ponerEnMayuscula :: [String] -> [String]
ponerEnMayuscula habilidades = map (map toUpper) habilidades

-- queremos mapear ahora [String] con "toUpper" pero como labura con Char -> Char entonces al map le pasamos otro "map toUpper" y la lista, asi lo hace para cada char del String

megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono 

-- si esta formado por una "cuerda" es el llamado a una funcion que espera 2 funciones y las compone, en este caso compone a "ardilla" y a "megafono" 

-- 3. - Aventuras 
-- Los bárbaros suelen ir de aventuras por el reino luchando contra las fuerzas del mal, pero ahora que tienen nuestra ayuda, quieren que se les diga si un grupo de bárbaros puede sobrevivir a cierta aventura.  Una aventura se compone de uno o más eventos, por ejemplo:

type Aventura = [Evento]
-- las aventuras estan compuestas por uno o mas eventos

type Evento = Barbaro -> Bool
-- vemos por los enunciados de abajo que son funciones que reciben un barbaro y nos devuelven si sobreviven o no a dicho evento

-- 1. invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”
invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes barbaro = elem "Escribir Poesia Atroz" (habilidades barbaro)

invasionDeSuciosDuendes' :: Evento
invasionDeSuciosDuendes' barbaro = elem "Escribir Poesia Atroz" . habilidades $ barbaro

-- 2. cremalleraDelTiempo: Un bárbaro sobrevive si no tiene pulgares. Los bárbaros llamados Faffy y Astro no tienen pulgares, los demás sí. 
cremalleraDelTiempo :: Evento
cremalleraDelTiempo barbaro = not . tienePulgares . nombre $ barbaro

-- del barbaro necesitamos su nombre, y que luego no "tienePulgares" que la delegamos a una funcion con matcheo, y como Matos dijo es mejor hacer la afirmacion y luego negarla con not

tienePulgares :: String -> Bool
tienePulgares "faffy" = False
tienePulgares "Astro" = False
tienePulgares _ = True

-- 3. ritualDeFechorias: Un bárbaro puede sobrevivir si pasa una o más pruebas como las siguientes: 
 
--   a. saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.
-- primero planteamos estos eventos para luego modelar "rituarDeFechorias" porque son en base a estos 3 eventos
saqueo :: Evento
saqueo barbaro = tieneHabilidad "robar" barbaro && esFuerte barbaro

-- delegamos en las siguientes funciones

tieneHabilidad :: String -> Barbaro -> Bool
tieneHabilidad habilidad barbaro = elem habilidad . habilidades $ barbaro
-- conseguimos las "habilidades" de dicho barbaro y nos fijamos si la habilidad que recibimos por parametro esta dentro de [String] del barbaro

esFuerte :: Barbaro -> Bool
esFuerte barbaro = (>80) . fuerza $ barbaro

-- b. gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letras de sus habilidades. El poder necesario para aprobar es 4 veces la cantidad de objetos del bárbaro.
-- vamos planteando para despues definir
gritoDeGuerra :: Evento
gritoDeGuerra barbaro = poderDeGritoDeGuerra barbaro >= cantidadDeLetrasDeHabilidades barbaro

poderDeGritoDeGuerra :: Barbaro -> Int
poderDeGritoDeGuerra barbaro = (*4) . length . objetos $ barbaro

-- agarramos los "objetos" de dicho barbaro que solo nos interesa la cantidad entonces con "length" y luego las multiplicamos por 4

cantidadDeLetrasDeHabilidades :: Barbaro -> Int
cantidadDeLetrasDeHabilidades barbaro = sum . map length . habilidades $ barbaro

-- agarramos las habilidades del barbaro [String] y como queremos la cantidad de letras entonces mapeamos "length" a cada elemento osea a cada string, dandonos una lista con la cantidad
-- de letras por cada elemento, para terminar sumamos las cantidades con "sum"

--   c. caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) si sus habilidades contienen más de 3 vocales y comienzan con mayúscula.  
-- arrancamos partiendo para despues ir definiendo
caligrafia :: Evento
caligrafia barbaro = all tieneMasDe3VocalesYEmpiezaConMayuscula (habilidades barbaro)

-- tenemos que ver que se cumpla para todos los elementos de [String] de "habilidades" de dicho barbaro

-- nuevamente volvemos a ir partiendo para luego definir
tieneMasDe3VocalesYEmpiezaConMayuscula :: String -> Bool
tieneMasDe3VocalesYEmpiezaConMayuscula habilidad = tieneMasDe3Vocales habilidad && empiezaConMayuscula habilidad

-- recordemos que "tieneMasDe3VocalesYEmpiezaConMayuscula" se la estamos pasando a un "all" mas una [String] entonces recibe los elementos de [String] que seran :: String -> Bool para ver si cumple o no

-- volvemos a partir para luego ir definiendo
tieneMasDe3Vocales :: String -> Bool
tieneMasDe3Vocales habilidad = (>3) . length . filter esVocal $ habilidad

-- Del string de "habilidad" del barbaro recordemos que es un alias de [Char] entonces podemos filtrarla de forma que queden solamente las vocales del string, para luego ver la cantidad y si es mayor a 3 

esVocal :: Char -> Bool
esVocal 'a' = True
esVocal 'e' = True
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal _ = False

-- Recordemos que el "filter" se encarga de ir haciendo secuencialmente a los elementos del "String" de "habilidad", que dichos elementos son "Char" y asi va filtrando en quienes no son vocales

empiezaConMayuscula :: String -> Bool
empiezaConMayuscula habilidad = isUpper . head $ habilidad 

-- Finalmente hacemos "ritualDeFechorias"
ritualDeFechorias :: [Evento] -> Evento
ritualDeFechorias eventos barbaro = any (\x -> x barbaro) eventos

-- lo que hace ritual de fechorias es aplicar todos los elementos [Evento] a un barbaro, y fijarse si dicho barbaro pasa alguna por eso usamos "any", como los elementos son Evento y "type Evento = Barbaro -> Bool" devuelven entonces al "any" le pasamos una funcion incognita y la [Evento], la incognita va recibiendo los elementos con "x" y los aplica a "barbaro" 

-- Definir la función sobrevivientes que tome una lista de bárbaros y una aventura, y diga cuáles bárbaros la sobreviven (es decir, pasan todas las pruebas)
sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes barbaros eventos = filter (\x -> all (\y -> y x) eventos) barbaros

-- tenemos [Barbaro] y una aventura, osea [Evento] = [Barbaro -> Bool], entonces lo que queremos es filtrar dichos barbaros segun aquellos que sobreviven a todos los eventos de la aventura, entonces 
-- 1) filter con una incognita y [Barbaro], la incognita va recibiendo los elementos de [Barbaro] en "x" y dentro de la definicion de la incognita debemos tener otra funcion capaz de aplicar cada evento a cada barbaro y decirnos cuales pasaron todos los eventos entonces
--  2) ya con el "todos" sabemos que debera ser un "all" que recibe otra funcion individual y ahora la [Evento] de la aventura, optamos por que la funcion sea una incognita que va recibiendo los elementos de [Evento] con "y" y entonces como cada elemento de [Evento] es una "Barbaro -> Bool" y en la incognita anterior del filter ya estamos recibiendo los elementos de [Barbaro] que es un Barbaro, entonces aplicamos cada elemento de [Barbaro] a los elementos de [Evento] con " y x"

-- 4) Los bárbaros se marean cuando tienen varias habilidades iguales. Por todo esto, nos piden desarrollar una función que elimine los elementos repetidos de una lista (sin utilizar nub ni nubBy)

--  sinRepetidos [1,2,3,4,4,5,5,6,7]
-- [1,2,3,4,5,6,7]

sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = [] -- caso base de la lista vacia, no hay nada que eliminar 
sinRepetidos (head : tail) -- caso recursivo recibiendo la lista con la logica (head : tail), head es el elemento actual y tail se crea una lista con todos los demas elementos
  | elem head tail = sinRepetidos tail  
  | otherwise = (head : sinRepetidos tail) 

-- con "elem head tail" nos fijamos el elemento actual que recibimos de head (no por la funcion si no porque este nombre le pusimos) esta contemplado en alguno de los elementos de la lista tail, en caso de estarlo entonces vuelve a llamar a la funcion ahora sin el elemento anterior de head, pasandole solo la lista con los demas elementos de tail. Luego esa lista que recibe de tail se encargara de aplicar de nuevo la logica (head : tail) para que el primer elemento de la lista de tail sea ahora la head y se cree otra lista en tail con los demas elementos
-- caso contrario donde el elemento actual head no esta contemplado en la lista de tail, entonces usando la logica (head : tail) mantenemos dicho elemento principal head, pero ahora llamando de nuevo a la funcion con la lista de los demas elementos de tail (que se encargara ahora de cambiar de elemento principal, siendo ahora el primer elemento de la lista de tail)

--4)b)  Los bárbaros son una raza muy orgullosa, tanto que quieren saber cómo van a ser sus descendientes y asegurarse de que los mismos reciban su legado. El descendiente de un bárbaro comparte su nombre, y un asterisco por cada generación. Por ejemplo "Dave*", "Dave**" , "Dave***" , etc.  Además, tienen en principio su mismo poder, habilidades sin repetidos, y los objetos de su padre, pero antes de pasar a la siguiente generación, utilizan (aplican sobre sí mismos) los objetos. Por ejemplo, el hijo de Dave será equivalente a: (ardilla.varitasDefectuosas) (Barbaro "Dave*" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas])
descendiente :: Barbaro -> Barbaro
descendiente barbaro = (utilizarObjetos . mapNombre (++ "*") . mapHabilidades sinRepetidos) barbaro

-- recibimos un barbaro y a ese queremos limpiarle todas las habilidades duplicadas que recibio por herencia, osea tenemos que hacerle un cambio a su atributo [Habilidad], entonces usamos "mapHabilidades" que directamente entra a [Habilidad], le pasamos entonces "sinRepetidos" para que elimine dichos elementos iguales, luego queremos agregarle al nombre un asterisco, lo hacemos con "mapNombre" y finalmente queremos aplicar todos los objetos del barbaro sobre si mismo entonces lo partimos en otra funcion "utilizarObjetos"

utilizarObjetos :: Barbaro -> Barbaro
utilizarObjetos barbaro = foldl (\acu x -> x acu) barbaro (objetos barbaro)

-- recibe un barbaro y queremos aplicar la [Objetos] del mismo y devolver el barbaro luego de aplicarse esos [Objeto], entonces usamos un foldl que recibe una incognita una semilla que sera el barbaro, y la [Objeto] que sale del atributo objetos del barbaro, entonces por sintaxis del foldl recibimos primero el valor de la semilla en "acu" y luego los elementos de [Objeto] en "x" y como "type Objeto = Barbaro -> Barbaro" entonces aplicamos cada elemento de [Evento] en el barbaro con "x acu" sin necesidad de aplicar parcialmente algo 

-- Definir la funcion "descendientes", que dado un barbaro nos de sus infinitos descendientes

-- Literalmente se toma a fondo lo de dar "infinitos descendientes" ya que "iterate" crea una lista infinita, en la primera vuelta aplica "descendiente" a un "unBarbaro", y luego lo hace infinitamente a los resultados anteriores partiendo de ese primero, y por cada resultado va almacenando en un elemento de la lista infinita 
-- => [("Dave", 100, ["tejer", "escribirPoesia"], [ardilla, libroPedKing]),...,("Dave***", ..., [...], [...]), ...  -}

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = iterate descendiente unBarbaro
