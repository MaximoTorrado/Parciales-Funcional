import Text.Show.Functions
import Data.Char (toUpper)

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

