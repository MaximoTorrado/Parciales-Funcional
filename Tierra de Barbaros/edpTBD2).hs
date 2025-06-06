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

--a) Las espadas aumentan la fuerza de los barbaros en 2 unidades por cada kilogramo de peso
espadas :: Int -> Objeto
espadas pesoDeEspada unBarbaro = mapFuerzaDeBarbaro ((+) (pesoDeEspada * 2)) unBarbaro 

agregarHabilidad :: String -> Objeto
agregarHabilidad unaHabilidad unBarbaro = mapHabilidadDeBarbaro (++ [unaHabilidad]) unBarbaro

--b) Los "amuletosMisticos" puerco-marranos otorgan una habilidad dada a un barbaro
amuletosMisticos :: String -> Objeto
amuletosMisticos unaHabilidad unBarbaro = agregarHabilidad "Hacer Magia" unBarbaro

--c) Las "varitasDefectuosas" añaden la habilidad de hacer magia, pero desaparecen todos los demas objetos del barbaro 
varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = (agregarHabilidad "Hacer Magia" . mapObjetosDeBarbaro (const [])) unBarbaro

varitasDefectuosas' :: Objeto
varitasDefectuosas' unBarbaro = unBarbaro { habilidadesDeBarbaro = habilidadesDeBarbaro unBarbaro ++ ["Hacer Magia"], objetosDeBarbaro = [] }

varitasDefectuosas'' :: Objeto
varitasDefectuosas'' unBarbaro = (agregarHabilidad "Hacer Magia" . desaparecerObjetos) unBarbaro

desaparecerObjetos :: Objeto
desaparecerObjetos unBarbaro = unBarbaro { objetosDeBarbaro = [varitaDefectuosa] }

--d) Una ardilla, que no hace nada
ardilla :: Objeto
ardilla = id

--e) Una cuerda, que combina dos objetos distintos, obteniendo uno que realiza las transformaciones de los otros dos
cuerda :: Objeto -> Objeto -> Objeto
cuerda unaFuncion otraFuncion = unaFuncion . otraFuncion

{- 2) El megafono es un objeto que potencia al barbaro, concatenando sus habilidades y poniendolas en mayusculas

> megafono dave
("Dave", 100, ["TEJERESCRIBIRPOESIA"], [ardilla, libroPedKing]) 

Sabiendo esto, definir al megafono, y al objeto megafonoBarbarico, que esta formado por una cuerda, una ardilla y un megafono -}
-- Respuesta:

{- Primero arrancamos modelando al "megafono", sera entonces una objeto (una funcion que va de barbaro a barbaro) que especificamente concatena la lista de habilidades del barbaro y
las pone en mayusculas como vamos a generar un cambio al campo "habilidadDeBarbaro"  entonces tendremos que usar "mapHabilidadesDeBarbaro" a la cual le pasaremos la composicion, que
a final de cuentas es una funcion entonces sera la "funcionModificacion" -}

{- IMPORTANTE!!
Notar como aca estamos delegando, osea partiendo el problema en partes mas pequeñas, para luego nosotros encargarnos de definir las funciones que hacen cada cosa, podriamos tambien
mandarnos a hacer la logica directamente en la composicion pero esto en la mayoria de casos no es tan expresivo, ahora delegarlo en funciones que se entiende en base al nombre nomas
que es lo que hacen, y luego definirlas es mucho mas expresivo -}

megafono :: Objeto
megafono unBarbaro = mapHabilidadesDeBarbaro (ponerEnMayusculas . concatenar) unBarbaro

{- Como la composicion hara la tarea de "funcionModificacion" si miramos el tipado de "mapHabilidadesDeBarbaro" entonces ambas definiciones de las funciones que vamos a componer
deben ser del mismo tipo osea [String] -> [String] (La misma logica de la definicion de "mapHabilidadesDeBabaro" termina reduciendo esta funcion de manera que nos sirva a nosotros y
simule el cambio a dicho campo), ya que esto es lo que recibe "mapHabilidadesDeBarbaro" mas un barbaro  -}

concatenar :: [String] -> [String]
concatenar unaHabilidad = [concat unaHabilidad]

ponerEnMayuscula :: [String] -> [String]
ponerEnMayuscula unaHabilidad = map (map toUpper) unaHabilidad

{- 1) concatenar: esta funcion entonces recibe una lista de habilidades (una lista de String) y termina concatenando todo con "concat" y todo eso que concatena lo encapsulamos en una
lista 

2) ponerEnMayusculas: recibe la lista de habilidades ahora con un solo elemento que representa todos los elementos pero concatenados, ahora como esto tiene que ir letra por letra
poniendo en mayuscula los elementos de una lista podemos pensar en usar "map", map necesita una "funcion criterio" y una lista

Hay un tema y es que la "funcion criterio" debe ser capaz de hacer la tarea que queremos traducir a toda la lista pero de forma INDIVIDUAL, el problema es que la funcion "toUpper" 
no sabe poner en mayuscula todo un elemento, que es todo un String, sabe hacerlo solo con char, entonces si hacemos "map toUpper" entonces ahi si es capaz de poner en mayuscula todo
un elemento INDIVIDUAL de una lista, que es un String, ahora si podemos usar esa como "funcion criterio"    -}

{- Finalmente armamos el objeto "megafonoBarbarico" que es jugar con las funciones que creamos anteriormente, y viendo la definicion de "cuerda" es basicamente una composicion entre
la funcion ardilla y megafono -}

megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono












{- IMPORTANTE!!
Cuando tenemos funciones que sabemos que deben hacer mas de una cosa de arranque pensamos en usar COMPOSICION para hacer las acciones de forma secuencial, luego
una forma de que nos quede mucho mas "lindo" es directamente nosotros ponerle los alias de las funciones que cada una hara cierta accion para que nosotros luego
en la definicion de cada una usemos las funciones predefinidas que querramos o la logica que querramos

Esto lo hacemos puramente para que en la composicion quede mas "limpio" solo componiendo las funciones, pero tranquilamente podria mandarme a intentar hacer la
definicion y la composicion en el mismo lugar    -}