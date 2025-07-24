import Text.Show.Functions

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

-- primero el "desaparecerObjetos" se puede interpretar como dejar sin elementos a la lista de objetos, o dejarla solo con "varitaDefectuosa" ambos siempre usando esta sintaxis de 
-- generar un cambio en un atributo, luego para agregar una habilidad hacemos lo mismo que el punto anterior ya que es constante que se agrega ese unico String "hacerMagia"

-- 4. Una ardilla, que no hace nada.
ardilla :: Objeto
ardilla = id

-- 5. Una cuerda, que combina dos objetos distintos,obteniendo uno que realiza las transformaciones de los otros dos. 
cuerda :: Objeto -> Objeto -> Objeto
cuerda objeto objeto2 = objeto . objeto2  

-- basicamente nos esta diciendo que es la composicion entre ambas funciones










