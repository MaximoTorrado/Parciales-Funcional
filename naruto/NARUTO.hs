{- Una persona muy muy muy fanática de esta gran historia, nos pidió crear el mundo de Naruto Uzumaki en el Paradigma Funcional. Así que, por favor, aplica tus conocimientos y ¡concedele el deseo! 🙏

Parte 1
En este mundo conocemos el nombre, las herramientas, los jutsus y el rango de cada ninja. El rango es un número que comienza en 0 y no puede ser negativo. Las herramientas ninjas son de mucha ayuda para realizar misiones. 
De cada una conocemos el nombre y la cantidad disponible. Algunos ejemplos son: bombas de humo, kunais, shurikens y sellos explosivos. Para poder utilizarlas se pide modelar: -}

data Ninja = Ninja {
  nombre :: String, 
  herramientas :: [Herramienta], 
  jutsus :: [Jutsu],                 --     nosotros aca planteamos el atributo jutsus pero todavia no sabemos que son hasta que los modelemos
  rango :: Int
}

-- esto lo deducimos de que sabemos de cada Herramienta su nombre y su cantidad disponible
type Herramienta = (String, Int)

-- que pida que el atributo "rango" no sea negativo significa que en cualquier caso que querramos hacer algun cambio a dicho atributo su maximo sea 0, osea agregamos la misma condicion que parcial anterior al accesor
mapRango :: (Int -> Int) -> Ninja -> Ninja
mapRango f ninja = ninja { rango = max 0 . f . rango $ ninja }

-- luego leyendo los puntos que le siguen vemos que tratan sobre al menos al principio agregar herramientas, entonces creamos su accesor
mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramientas f ninja = ninja { herramientas = f . herramientas $ ninja }

-- si leemos mas a fondo vemos "cada ninja puede obtener una cantidad especifica de una herramienta" osea que en este punto no es solamente agregar una tupla "Herramienta = (String, Int)" si no que especificamente agregar
-- al segundo miembro de la tupla que representa la cantidad, entonces para eso y no repetir logica creamos su accesor a un miembro de una tupla de la siguiente manera
mapCantidad :: (Int -> Int) -> Herramienta -> Herramienta 
mapCantidad f (nombre, cantidad) = (nombre, f cantidad)

-- a. obtenerHerramienta: cada ninja debe poder obtener una cantidad específica de una herramienta en particular teniendo en cuenta que:
-- i. si la suma de todas sus herramientas más la cantidad a obtener es menor o igual a 100, puede hacerlo sin problemas;
-- ii. en caso contrario, obtiene la cantidad que pueda sin excederse de 100 herramientas.  

obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta herramienta ninja = mapHerramientas((mapCantidad . min . cuantasHerramientasPuedeObtener) ninja herramienta :) ninja
-- la idea es que esta funcion agregue una tupla a [Herramienta] de un ninja, esto lo haremos con "mapHerramienta" que le pasamos una funcion y a dicho ninja

cuantasHerramientasPuedeObtener :: Ninja -> Int
cuantasHerramientasPuedeObtener ninja = (100 -) . cantidadDeHerramientas $ ninja

cantidadDeHerramientas :: Ninja -> Int
cantidadDeHerramientas ninja = sum . map snd . herramientas $ ninja

-- b. usarHerramienta: cuando un ninja usa alguna de sus herramientas no mide cuántas utiliza, por lo que se queda sin ella y no debe figurar más entre sus pertenencias.
usarHerramienta :: String -> Ninja -> Ninja
usarHerramienta otroNombreHerramienta ninja = mapHerramientas (filter ((/=) otroNombreHerramienta . nombreHerramienta)) $ ninja

nombreHerramienta :: Herramienta -> String
nombreHerramienta = fst

-- la idea es de la [Herramienta] quitar aquella herramienta que le pasamos, sabemos entonces que usaremos el "mapHerramienta" para hacer el cambio y le pasaremos un filter para quitar aquella tupla de [Herramienta] y a dicho  ninja, al ser una tupla tenemos que tener alguno de los miembros para identificarla entonces, a todas las tuplas nos quedamos solo con el primer miembro nombre con "nombreHerramienta" que es un llamado a fst, luego con la lista que nos quedo todas con el string del primer miembro de las tuplas las filtramos con todas aquellas que no tengan el nombre que recibimos por parametro, y listo 

{- Parte 2
En su vida cotidiana, cada ninja tiene que cumplir misiones. Por suerte no es un trabajo solitario, ¡trabajan en equipos! De cada misión se especifica qué cantidad de ninjas requiere, el rango recomendable para realizarla, qué ninjas enemigos hay que derrotar y la herramienta (¡obviamente con su cantidad!) de recompensa en caso de cumplirla con éxito. Se pide modelar: -}

data Mision = Mision {
  cantidadDeNinjas :: Int,  -- aca no nos habla de una lista o conjunto de ninjas, si no especificamente "cantidad" osea solo Int
  rangoRecomendado :: Int, 
  ninjasEnemigos :: [Ninja],   -- aca si se hace referencia a un conjunto de ninjas
  recompensa :: Herramienta
} -- deriving Show


-- a. esDesafiante: dado un equipo de ninjas, una misión es desafiante cuando al menos alguien del equipo tiene menor rango que el recomendado y hay que derrotar al menos 2 enemigos.
esDesafiante :: [Ninja] -> Mision -> Bool
esDesafiante ninjas mision = any (not . estaCalificado mision) ninjas && ((>=2) . length . ninjasEnemigos) mision

estaCalificado :: Mision -> Ninja -> Bool
estaCalificado mision ninja = rango ninja >= rangoRecomendado mision

-- de [Ninja] queremos ver si "alguno" no tiene el rango recomendado, osea no esta calificado (como recomendo matos es mejor hacer la afirmacion y luego negarla con . not), osea un "any" que recibe la funcion capaz de a partir de una mision y un ninja ver si el "rango" del ninja es mayor al "rangoRecomendado" de la mision, que la partimos en "estaCalificado" que hace eso que dije xD, y luego el any se encarga de mirar para todos los elementos de "ninjas" luego que "haya que derrotar al menos 2" significa que la "ninjasEnemigos :: [Ninja]" debe tener al menos 2 elementos que representan los ninjas a derrotar 

-- b. esCopada: esto pasa cuando la recompensa de la misión son 3 bombas de humo, 5 shurikens o 14 kunais.
esCopada :: Mision -> Bool
esCopada mision = esCopadita (recompensa mision)
-- le pasamos a "esCopadita" la tupla "Herramienta" del atributo recompensa de la mision, para que dicha tupla sea evaluada si es o no alguna de las planteadas

esCopadita :: Herramienta -> Bool
esCopadita ("bombas de humo", 3) = True
esCopadita ("shuriken", 5) = True
esCopadita ("kunai", 14) = True
esCopadita _ = False
-- con matcheo directamente recibiendo los miembros de la tupla y asignando una respuesta, esta al ser booleana le ponemos True y False

esCopada' :: Mision -> Bool
esCopada' mision = esCopadita' (recompensa mision)
-- lo mismo

recoveco :: [Herramienta]
recoveco = [("bombas de humo", 3), ("shuriken", 5), ("kunai", 14)]
-- agregamos en una lista de tuplas con aquellas que nos plantearon ver

esCopadita' :: Herramienta -> Bool
esCopadita' herramienta = elem herramienta recoveco
-- notamos que podemos hacer un elem con tupla [tuplas]

-- c. esFactible: para que una misión sea factible no tiene que ser desafiante y además el grupo debe contar con la cantidad de ninjas necesaria o la suma total de herramientas del equipo debe ser superior a 500.
esFactible :: [Ninja] -> Mision -> Bool
esFactible ninjas mision = (not . esDesafiante ninjas) mision && ((>=) (cantidadDeNinjas mision) . length) ninjas || sumaHerramientasMayorA500 ninjas

-- primero nos fijamos que no sea desafiante negando "esDesafiante" que recibe ninjas y una mision, entonces hacemos esa composicion respetando el orden aplicando parcialmente con ninjas, luego de [Ninja] queremos su cantidad y luego comparar si es mayor con la "cantidadDeNinjas" que pide la mision ó si la suma de las herramientas es mayor a 500                     

sumaHerramientasMayorA500 :: [Ninja] -> Bool
sumaHerramientasMayorA500 ninjas = ((>= 500) . sum . map cantidadDeHerramientas) ninjas

-- de cada elemento de [Ninja] osea de cada Ninja queremos su [Herramienta], porque asi como esta tendria [[Herramienta],..., [Herramienta]] para ver la cantidad de las mismas (que las modela el segundo miembro de la tupla) y para eso deberiamos a cada Ninja entrar a su atributo [Herramienta], luego mapear con snd para tener la cantidad de cada herramienta, esto ya lo hicimos en "cantidadDeHerramientas" entonces si mapeamos eso a [Ninja] obtenemos [3, 5, 74,] las cantidades de cada herramienta, entonces finalmente sumamos para tener el total y nos fijamos si es o no mayor a 500 

-- Las misiones se pueden completar con éxito o no:
-- d. fallarMision: la vida no siempre es fácil... ni en nuestro mundo ni en el mundo ninja. Cuando una misión falla sólo quedan en el equipo quienes tengan el rango recomendado o superior. Quienes queden sufrirán la vergüenza de ver su rango disminuido en 2 unidades. ¡Por el resto del equipo no te preocupes! Te prometemos que están bien. 😝
fallarMision :: [Ninja] -> Mision -> [Ninja]
fallarMision ninjas mision = (sacar2Rangos . filter ( (>=) (rangoRecomendado mision) . rango)) ninjas 

-- tenemos [Ninja] y necesitamos de la mision para obtener el rango recomendado, queremos filtrar [Ninja] segun aquellos que pasan el rango recomendado (asi descartamos los que no) y los que queden le sacamos 2 de su rango, podriamos partir en 2 el filtrado pero se entiende, luego la lista filtrada que recibe "sacar2Rangos" hace eso 

sacar2Rangos :: [Ninja] -> [Ninja]
sacar2Rangos ninjas = map (mapRango (subtract 2)) ninjas 
-- recibe [Ninja] y como queremos a cada elemento, que es un ninja a su atributo rango restarle dos, mapeamos con "mapRango" pasandole "subtract 2" 

-- e. cumplirMision: si todo sale bien, se promociona de rango a cada miembro del equipo. Además obtendrán la recompensa teniendo en cuenta la restricción del máximo de herramientas.
cumplirMision :: [Ninja] -> Mision -> [Ninja]
cumplirMision ninjas mision = map (obtenerHerramienta (recompensa mision) . promover) ninjas  -- mapRango (+1)

promover :: Ninja -> Ninja
promover ninja = mapRango (+1) ninja
--  esto lo hacemos como para que no se vea la repeticion de logica con el punto anterior

-- ¡Todavía no hablamos de los jutsus! Técnicas especiales que nacen de la energía interior de cada ninja. Es como un superpoder que hace que las misiones sean más simples 😅.

-- Algunas de las que conocemos son:
-- f. clonesDeSombra: reduce la cantidad de ninjas que se necesitan para una misión en el mismo número que los clones de sombra creados. ¡El tamaño del equipo no puede ser menor a 1!
clonesDeSombra :: Int -> Mision -> Mision
clonesDeSombra clones mision = mision { cantidadDeNinjas = max 1 (cantidadDeNinjas mision - clones) }

-- justamente nosotros queremos hacer un cambio a un atributo del data Mision, usamos esta forma (como haciamos en parciales anteriores sin accesors), dado el caso que querramos hacer un accesor podriamos hacer lo siguiente

{- recordando que: 
data Mision = Mision {
  cantidadDeNinjas :: Int,  -- aca no nos habla de una lista o conjunto de ninjas, si no especificamente "cantidad" osea solo Int
  ...
} deriving Show -}

mapCantidadDeNinjas :: (Int -> Int) -> Mision -> Mision
mapCantidadDeNinjas f mision = mision { cantidadDeNinjas = f . cantidadDeNinjas $ mision }

-- para luego
clonesDeSombra' :: Int -> Jutsu -- Mision -> Mision
clonesDeSombra' clones mision = mapCantidadDeNinjas (subtract clones) mision

-- g. fuerzaDeUnCentenar: elimina a todos los enemigos con rango menor a 5
fuerzaDeUnCentenar :: Jutsu -- Mision -> Mision
fuerzaDeUnCentenar mision = mision { ninjasEnemigos = filter ((<5) . rango) (ninjasEnemigos mision) }

-- nuevamente nosotros queremos hacer un cambio a un atributo de una mision, entonces usamos esta forma (como la usamos en parciales anteriores) sin un accesor, donde queremos filtrar [Ninja] de "ninjasEnemigos" de la mision segun aquellos cuyo rango es menor a 5, componiendo estas dos (todo eso podemos mandarlo al filter ya que se encargar de hacerlo a todos los elementos de [Ninja])

-- ahora alternativa con un accesor
mapNinjasEnemigos :: ([Ninja] -> [Ninja]) -> Mision -> Mision
mapNinjasEnemigos f mision = mision { ninjasEnemigos = f . ninjasEnemigos $ mision }

-- ahora si
fuerzaDeUnCentenar' :: Mision -> Mision
fuerzaDeUnCentenar' mision = mapNinjasEnemigos (filter ((<5) . rango)) mision

-- para ambas cuando usamos la alternativa del accesor, tenemos que tener en cuenta que si miramos el accesor, ya se encarga de conseguir [Ninja] se "ninjasEnemigos" entonces nosotros tenemos que pensar solo en la composicion de una funcion que ya que ese [Ninja] de "ninjasEnemigos" y eso es lo que le pasaremos, siempre pensando en lo secuencial de la composicion

-- Se pide modelar ejecutarMision. Cuando se ejecuta una misión, todos los ninjas del grupo usan todos sus jutsus en la misión. Luego, si la misión es copada o factible, se cumple. Caso contrario la misión se falla.
ejecutarMision :: [Ninja] -> Mision -> [Ninja]
ejecutarMision ninjas mision = completarMision ninjas . usarTodosSusJutsus ninjas $ mision

-- de [Ninja] y una mision, queremos que los [Jutsu] de cada ninja sean aplicados con la mision que le pasamos, entronces partimos en dos funciones donde la primera reciba los ninjas de los cuales sacar los [Jutsu] y aplicar todos en una mision con "usarTodosSusJutsus" y luego de la mision que devuelve mirar si esta completa o no con "completarMision" aplicada parcialmente con los ninjas

usarTodosSusJutsus :: [Ninja] -> Mision -> Mision
usarTodosSusJutsus ninjas mision = foldl (\acu x -> x acu) mision . concat . map jutsus $ ninjas 

-- recibe entonces [Ninja] de los cuales conseguir [Jutsu] para que la mision sea aplicada a cada elemento de [Jutsu], si a [Ninja] mapeamos, para que sea a todos sus elementos, el atributo jutsu obtenemos [[Jutsu],...,[Jutsu]], entonces lo aplanamos con "concat" [[Jutsu]] => [Jutsu], entonces ya tenemos la lista con la cual podemos usar un foldl
-- foldl recibe una incognita una semilla que sera la mision y la lista que aplanamos anteriormente con [Jutsu] entonces por sintaxis del foldl recibe el valor de la semilla en "acu" y luego los elementos de [Jutsu] con "x" entonces solo aplicamos cada elemento de la lista "x" con el valor de la semilla "acu"

completarMision :: [Ninja] -> Mision -> [Ninja]
completarMision ninjas mision
  | esCopada mision || esFactible ninjas mision = cumplirMision ninjas mision  -- osea se los promueve un rango
  | otherwise = fallarMision ninjas mision  -- osea se filtra y los que quedan pierden 2 rangos

-- finalmente aplicado parcialmente con ninjas recibe la mision luego de pasar por todos los [Jutsu] y para completarla se fija la condicion de "esCopada" o "esFactible" que definimos anteriormente, dado el caso entonces "cumplirMision" que tambien definimos anteriormente promoviendo un rango, caso contrario "fallarMision" que tambien definimos anteriormente que saca 2 rangos

-- notamos ahora lo que tiene en comun todos los Jutsus
type Jutsu = Mision -> Mision

-- ¡Hora de la acción! Se pide modelar ejecutarMision. Cuando se ejecuta una misión, todos los ninjas del grupo usan todos sus jutsus en la misión. Luego, si la misión es copada o factible, se cumple. Caso contrario la misión se falla.


{- Parte 2

Existe la Gran Guerra Ninja, una misión de rango 100 que necesita al menos 100000 ninjas para completarse, tiene infinitos enemigos y su recompensa es el abanico de Madara Uchiha.
Se pide modelar la granGuerraNinja sabiendo, además, que tiene infinitos villanos y son Zetsu 1, Zetsu 2, Zetsu 3... Zetsu N, el rango de todos es de 600 y no tienen jutsus ni herramientas. -}

granGuerraNinja :: Mision
granGuerraNinja = Mision { 
  cantidadDeNinjas = 100000,
  rangoRecomendado = 100, 
  ninjasEnemigos = infinitosZetsus,
  recompensa = ("Abanico", 1)
}

-- si queremos hacer un "Zetsu 1, Zetsu 2, Zetsu 3... Zetsu N"
infinitosZetsus :: [Ninja]
infinitosZetsus = map zetsu [1..]

zetsu :: Int -> Ninja
zetsu n = Ninja {
  nombre = "Zetsu" ++ show n,
  rango = 600, 
  jutsus = [],
  herramientas = []
} 

-- Sabiendo esto y teniendo en cuenta un equipo de ninjas finitos, responder qué devuelve y por qué en las siguientes funciones: 
-- a. esDesafiante
--solo es verdad si de los 100000 ninjas tienen rango mayor igual a 100, cualquier otro caso no podra terminar de evaluar nunca porque quiere mirar la longitud de ninjasEnemigos y en granGuerraNinja es una lista infinita 

-- b. esCopada
-- devuelve false porque lo que hace es mirar si el atributo recompensa de la mision es alguna de las planteadas, nunca entra en juego el atributo de la lista infinita, y ademas no tiene dicha recompensa que busca

-- c. fuerzaDeUnCentenar       
-- no termina nunca de evaluar porque de la mision trabaja con el atributo de la lista infinita, y ademas quiere filtrarla segun una condicion pero nunca va a poder terminar de evaluar la lista como para poder filtrarla