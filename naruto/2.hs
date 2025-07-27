{- Una persona muy muy muy fanática de esta gran historia, nos pidió crear el mundo de Naruto Uzumaki en el Paradigma Funcional. Así que, por favor, aplica tus conocimientos y ¡concedele el deseo! 🙏

Parte 1
En este mundo conocemos el nombre, las herramientas, los jutsus y el rango de cada ninja. El rango es un número que comienza en 0 y no puede ser negativo. Las herramientas ninjas son de mucha ayuda para realizar misiones. 
De cada una conocemos el nombre y la cantidad disponible. Algunos ejemplos son: bombas de humo, kunais, shurikens y sellos explosivos. Para poder utilizarlas se pide modelar: -}

data Ninja = Ninja {
  nombre :: String, 
  herramientas :: [Herramienta], 
  justus :: [Jutsu],  -- jutsus :: [Jutsu],       nosotros aca planteamos el atributo jutsus pero todavia no sabemos que son hasta que los modelemos
  rango :: Int
} -- deriving Show

type Herramienta = (String, Int)

mapRango :: (Int -> Int) -> Ninja -> Ninja
mapRango f ninja = ninja { rango = max 0 . f . rango $ ninja }

mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramientas f ninja = ninja { herramientas = f . herramientas $ ninja }

mapCantidad :: (Int -> Int) -> Herramienta -> Herramienta 
mapCantidad f (nombre, cantidad) = (nombre, f cantidad)

-- a. obtenerHerramienta: cada ninja debe poder obtener una cantidad específica de una herramienta en particular teniendo en cuenta que:
-- i. si la suma de todas sus herramientas más la cantidad a obtener es menor o igual a 100, puede hacerlo sin problemas;
-- ii. en caso contrario, obtiene la cantidad que pueda sin excederse de 100 herramientas.  
obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta herramienta ninja = mapHerramientas((mapCantidad . min . cuantasHerramientasPuedeObtener) ninja herramienta :) ninja

cuantasHerramientasPuedeObtener :: Ninja -> Int
cuantasHerramientasPuedeObtener ninja = (100 -) . cantidadDeHerramientas $ ninja

cantidadDeHerramientas :: Ninja -> Int
cantidadDeHerramientas ninja = sum . map snd . herramientas $ ninja

-- b. usarHerramienta: cuando un ninja usa alguna de sus herramientas no mide cuántas utiliza, por lo que se queda sin ella y no debe figurar más entre sus pertenencias.
usarHerramienta :: String -> Ninja -> Ninja
usarHerramienta otroNombreHerramienta ninja = mapHerramientas (filter ((/=) otroNombreHerramienta . nombreHerramienta)) $ ninja

nombreHerramienta :: Herramienta -> String
nombreHerramienta = fst

{- Parte 2
En su vida cotidiana, cada ninja tiene que cumplir misiones. Por suerte no es un trabajo solitario, ¡trabajan en equipos! De cada misión se especifica qué cantidad de ninjas requiere, el rango recomendable para realizarla, qué
ninjas enemigos hay que derrotar y la herramienta (¡obviamente con su cantidad!) de recompensa en caso de cumplirla con éxito. Se pide modelar: -}

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

-- de [Ninja] queremos ver si "alguno" no tiene el rango recomendado, osea no esta calificado (como recomendo matos es mejor hacer la afirmacion y luego negarla con . not), osea un "any" que recibe la funcion capaz de a partir de 
-- una mision y un ninja ver si el "rango" del ninja es mayor al "rangoRecomendado" de la mision, que la partimos en "estaCalificado" que hace eso que dije xD, y luego el any se encarga de mirar para todos los elementos de "ninjas"
-- luego que "haya que derrotar al menos 2" significa que la "ninjasEnemigos :: [Ninja]" debe tener al menos 2 elementos que representan los ninjas a derrotar 

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

-- primero nos fijamos que no sea desafiante negando "esDesafiante" que recibe ninjas y una mision, entonces hacemos esa composicion respetando el orden aplicando parcialmente con ninjas, luego de [Ninja] queremos su cantidad 
-- y luego comparar si es mayor con la "cantidadDeNinjas" que pide la mision ó si la suma de las herramientas es mayor a 500                     

sumaHerramientasMayorA500 :: [Ninja] -> Bool
sumaHerramientasMayorA500 ninjas = ((>= 500) . sum . map cantidadDeHerramientas) ninjas

-- de cada elemento de [Ninja] osea de cada Ninja queremos su [Herramienta], porque asi como esta tendria [[Herramienta],..., [Herramienta]] para ver la cantidad de las mismas (que las modela el segundo miembro de la tupla) y para 
-- eso deberiamos a cada Ninja entrar a su atributo [Herramienta], luego mapear con snd para tener la cantidad de cada herramienta, esto ya lo hicimos en "cantidadDeHerramientas" entonces si mapeamos eso a [Ninja] obtenemos
-- [3, 5, 74,] las cantidades de cada herramienta, entonces finalmente sumamos para tener el total y nos fijamos si es o no mayor a 500 

-- Las misiones se pueden completar con éxito o no:
-- d. fallarMision: la vida no siempre es fácil... ni en nuestro mundo ni en el mundo ninja. Cuando una misión falla sólo quedan en el equipo quienes tengan el rango recomendado o superior. Quienes queden sufrirán la vergüenza de ver
-- su rango disminuido en 2 unidades. ¡Por el resto del equipo no te preocupes! Te prometemos que están bien. 😝
fallarMision :: [Ninja] -> Mision -> [Ninja]
fallarMision ninjas mision = (sacar2Rangos . filter ( (>=) (rangoRecomendado mision) . rango)) ninjas 
-- tenemos [Ninja] y necesitamos de la mision para obtener el rango recomendado, queremos filtrar [Ninja] segun aquellos que pasan el rango recomendado (asi descartamos los que no) y los que queden le sacamos 2 de su rango, podriamos
-- partir en 2 el filtrado pero se entiende, luego la lista filtrada que recibe "sacar2Rangos" hace eso 

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

-- type Jutsu =        ahora planteamos esto despues de modelar los jutsus

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

-- nuevamente nosotros queremos hacer un cambio a un atributo de una mision, entonces usamos esta forma (como la usamos en parciales anteriores) sin un accesor, donde queremos filtrar [Ninja] de "ninjasEnemigos" de la mision
-- segun aquellos cuyo rango es menor a 5, componiendo estas dos (todo eso podemos mandarlo al filter ya que se encargar de hacerlo a todos los elementos de [Ninja])

-- ahora alternativa con un accesor
mapNinjasEnemigos :: ([Ninja] -> [Ninja]) -> Mision -> Mision
mapNinjasEnemigos f mision = mision { ninjasEnemigos = f . ninjasEnemigos $ mision }

-- ahora si
fuerzaDeUnCentenar' :: Mision -> Mision
fuerzaDeUnCentenar' mision = mapNinjasEnemigos (filter ((<5) . rango)) mision


-- para ambas cuando usamos la alternativa del accesor, tenemos que tener en cuenta que si miramos el accesor, ya se encarga de conseguir [Ninja] se "ninjasEnemigos" entonces nosotros tenemos que pensar solo en la composicion de
-- una funcion que ya que ese [Ninja] de "ninjasEnemigos" y eso es lo que le pasaremos, siempre pensando en lo secuencial de la composicion

-- notamos ahora lo que tiene en comun todos los Jutsus
type Jutsu = Mision -> Mision

-- ¡Hora de la acción! Se pide modelar ejecutarMision. Cuando se ejecuta una misión, todos los ninjas del grupo usan todos sus jutsus en la misión. Luego, si la misión es copada o factible, se cumple. Caso contrario la misión se falla.
-- ejecutarMision :: [Ninja] -> Mision -> [Ninja]
-- ejecutarMision ninjas mision = mirarMision ninjas . usarTodosJutsus ninjas $ mision 

-- usarTodosJutsus :: [Ninja] -> Mision -> [Ninja]
















