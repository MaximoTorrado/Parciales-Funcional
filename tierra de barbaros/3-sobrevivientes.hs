import Text.Show.Functions
import Data.Char (toUpper, isUpper)

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

-- luego tambien como vemos que los "Objetos" se basan en devolver un barbaro pero con ciertos atributos de su data cambiado, entonces planteamos los "accesors" a cada atributo para no repetir logica cada
-- que devolvemos un barbaro con los atributos cambiados
mapNombre :: (String -> String) -> Barbaro -> Barbaro
mapNombre f barbaro = barbaro { nombre = f . nombre $ barbaro}

mapFuerza :: (Int -> Int) -> Barbaro -> Barbaro
mapFuerza f barbaro = barbaro { fuerza = f . fuerza $ barbaro}

mapHabilidades :: ([String] -> [String]) -> Barbaro -> Barbaro
mapHabilidades f barbaro = barbaro { habilidades = f . habilidades $ barbaro}

mapObjetos :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
mapObjetos f barbaro = barbaro { objetos = f . objetos $ barbaro}

-- 1. Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.
espada :: Int -> Objeto 
espada peso barbaro = mapFuerza (+ peso * 2) barbaro

-- 2. Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.
amuletoMistico :: String -> Objeto
amuletoMistico habilidad = agregarHabilidad habilidad

-- 3. Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.
varitaDefectuosa :: Objeto
varitaDefectuosa barbaro = (agregarHabilidad "HacerMagia" . desaparecerObjetos) barbaro  

agregarHabilidad :: String -> Barbaro -> Barbaro
agregarHabilidad habilidad barbaro = mapHabilidades (++ [habilidad]) barbaro

desaparecerObjetos :: Objeto 
desaparecerObjetos barbaro = barbaro { objetos = [varitaDefectuosa]}

-- 4. Una ardilla, que no hace nada.
ardilla :: Objeto
ardilla = id

-- 5. Una cuerda, que combina dos objetos distintos,obteniendo uno que realiza las transformaciones de los otros dos. 
cuerda :: Objeto -> Objeto -> Objeto
cuerda objeto objeto2 = objeto . objeto2  

{- 2. El megafono es un objeto que potencia al bárbaro, concatenando sus habilidades y poniéndolas en mayúsculas. 
Sabiendo esto, definir al megafono, y al objeto megafonoBarbarico, que está formado por una cuerda, una ardilla y un megáfono. Recuerden que esta la función toUpper que toma una Char y devuelve un Char pero en mayúscula
Main> toUpper ‘a’  -}

megafono :: Objeto
megafono barbaro = mapHabilidades (ponerEnMayuscula . concatenar) barbaro

concatenar :: [String] -> [String]
concatenar habilidades = [concat habilidades]

ponerEnMayuscula :: [String] -> [String]
ponerEnMayuscula habilidades = map (map toUpper) habilidades

megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono 

-- 3. - Aventuras 
-- Los bárbaros suelen ir de aventuras por el reino luchando contra las fuerzas del mal, pero ahora que tienen nuestra ayuda, quieren que se les diga si un grupo de bárbaros puede
-- sobrevivir a cierta aventura.  Una aventura se compone de uno o más eventos, por ejemplo:

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

--   b. gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letras de sus habilidades. El poder necesario para aprobar es 4 veces la cantidad de objetos
--  del bárbaro.
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
caligrafia :: Evento
caligrafia barbaro = all tieneMasDe3VocalesYEmpiezaConMayuscula (habilidades barbaro)

tieneMasDe3VocalesYEmpiezaConMayuscula :: String -> Bool
tieneMasDe3VocalesYEmpiezaConMayuscula habilidad = tieneMasDe3Vocales habilidad && empiezaConMayuscula habilidad

tieneMasDe3Vocales :: String -> Bool
tieneMasDe3Vocales habilidad = (>3) . length . filter esVocal $ habilidad

esVocal :: Char -> Bool
esVocal 'a' = True
esVocal 'e' = True
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal _ = False

empiezaConMayuscula :: String -> Bool
empiezaConMayuscula habilidad = isUpper . head $ habilidad 

-- Finalmente hacemos "ritualDeFechorias"
ritualDeFechorias :: [Evento] -> Evento
ritualDeFechorias eventos barbaro = any (\x -> x barbaro) eventos

-- Definir la función sobrevivientes que tome una lista de bárbaros y una aventura, y diga cuáles bárbaros la sobreviven (es decir, pasan todas las pruebas)

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes barbaros aventura = 





