{- Rechapos! - En la tierra de Udrogoth los reyes bárbaros formaron un imperio de guerreros, dragones y magia. Ahora marcharon a la guerra contra el mal y 
dejaron a sus herederos a cargo.... y bueno, hay que conformarse. Sin embargo, nos encargaron confeccionar un programa en Haskell para 
manejar los asuntos del reino, y evitar así el tener que gobernar.

Consideraciones: 
Escribir el tipo de todas las funciones principales
Emplear sinónimos de tipo cuando sea posible.
No se permite usar recursividad salvo que se indique lo contrario
Definir las funciones en estilo point-free cuando sea posible       -}

{- Punto 1
Se sabe que los barbaros tienen nombre, fuerza, habilidades y objetos, que lo ayudaran mas adelante en su lucha contra el mal, por ejemplo:
dave = ("Dave", 100, ["tejer", "escribirPoesia"], [ardilla, libroPedKing]) 

Se pide definir los siguientes objetos y definir algunos bárbaros de ejemplo
a) Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.
b) Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.
c) Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.
d) Una ardilla, que no hace nada.
e) Una cuerda, que combina dos objetos distintos,obteniendo uno que realiza las transformaciones de los otros dos. -}

{- -----------------------------------------------------------------------------------------------------------------------------------------------------------
RESPUESTAS
--------------------------------------------------------------------------------------------------------------------------------------------------------------

Primero que nada arrancamos modelando el barbaro sabiendo como es su estructura con el ejemplo, sabemos que tiene un campo de tipo String,
otro de tipo Int, otro que es una lista de Strings, y finalmente otro que es una lista de FUNCIONES       -}

{- que son aquellas que simulan los "objetos" de cada barbaro, que si miramos con atencion notamos que son funciones que reciben un barbaro
y devuelven otro barbaro con alguna modificacion en algun campo, osea (Barbaro -> Barbaro)  -}

type Objeto = Barbaro -> Barbaro  -- Este type representa una funcion que recibe un barbaro y devuelve otro barbaro

data Barbaro = Barbaro {
    nombreDeBarbaro :: String,
    fuerzaDeBarbaro :: Int,
    habilidadesDeBarbaro :: [String],
    objetosDeBarbaro :: [Objeto]         -- Entonces aca sera una lista de funciones que van de barbaro a barbaro
} -- deriving Show  (Lo pongo en comentario porque no se bien donde pero tiene que "usarse" porque si no rompe los huevos)

{- FUNDAMENTAL!!

Notamos que las FUNCIONES que nos mandan a hacer SIEMPRE GENERAN un CAMBIO a ALGUN CAMPO o FUNCION DE ACCESO de un parametro data Barbaro
entonces lo que podemos hacer son TENER PREPARADO las FUNCIONES "MAP" para SOLO PASARLE LA FUNCION DE MODIFICACION y no REPETIR LOGICA al
simular cambios en los campos -}

{- Basicamente lo que estamos haciendo es hacer una funcion que en su definicion usa la sintaxis para "simular" un cambio en determinado 
campo, es siempre la misma forma nomas que para cada funcion cambiariamos el campo donde simulamos el cambio  -}

-- Accesors "De cambio"
mapNombreDeBarbaro :: (String -> String) -> Barbaro -> Barbaro
mapNombreDeBarbaro funcionModificacion unBarbaro = unBarbaro { nombreDeBarbaro = (funcionModificacion . nombreDeBarbaro) unBarbaro }

mapFuerzaDeBarbaro :: (Int -> Int) -> Barbaro -> Barbaro
mapFuerzaDeBarbaro funcionModificacion unBarbaro = unBarbaro { fuerzaDeBarbaro = (funcionModificacion . fuerzaDeBarbaro) unBarbaro }

mapHabilidadDeBarbaro :: ([String] -> [String]) -> Barbaro -> Barbaro
mapHabilidadDeBarbaro funcionModificacion unBarbaro = unBarbaro { habilidadesDeBarbaro = (funcionModificacion . habilidadesDeBarbaro) unBarbaro }

mapObjetosDeBarbaro :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
mapObjetosDeBarbaro funcionModificacion unBarbaro = unBarbaro { objetosDeBarbaro = (funcionModificacion . objetosDeBarbaro) unBarbaro }

-- a) Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.

-- Respuesta:

{- Logica: Podriamos recibir el peso del barbaro (pesoDeBarbaro) y un barbaro al cual aplicar el peso de la espada, y devolver un barbaro
con el campo "fuerzaDeBarbaro" cambiado ¿Como? Llamando a la funcion "mapFuerzaDeBarbaro" y pasandole ((+) (pesoDeEspada * 2)) unBarbaro 
que basicamente es para componer la suma con el peso aplicado parcialmente, y reemplazando en la funcion quedaria algo asi:   

mapNombreDeBarbaro ((+) pesoDeEspadaX2) unBarbaro = unBarbaro { nombreDeBarbaro = ((+) pesoDeEspadaX2 . fuerzaDeBarbaro) unBarbaro }

Y es re valido   -}

{- IMPORTANTE!! 
Notamos aca que SI se puede usar type de algo para "unir" agregandole mas parametros que necesite el tipado de la funcion -}

espadas :: Int -> Objeto
espadas pesoDeEspada unBarbaro = mapFuerzaDeBarbaro ((+) (pesoDeEspada * 2)) unBarbaro 










