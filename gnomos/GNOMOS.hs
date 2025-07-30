import Data.List (intersect)

-- 🧙‍♂️ Gnomos!!! Queremos modelar parte de un juego inspirado en el Gnomoria, donde se maneja una aldea y la misma puede transformarse con el trabajo de los gnomitos que la habitan. Para ello definimos y modelamos los materiales de construcción para los edificios, los edificios para construir en la aldea y la aldea misma de la siguiente forma:

data Aldea = Aldea {
  poblacion             :: Int,
  materialesDisponibles :: [Material],
  edificios             :: [Edificio]
} deriving (Show, Eq)

data Edificio = Edificio {
  tipoEdificio :: String,
  materiales   :: [Material]
} deriving (Show, Eq)

data Material = Material {
  nombre  :: String,
  calidad :: Int
} deriving (Show, Eq)

-- ✅ 1. Desarrollar las siguientes funciones:
-- a. esValioso: Recibe un Material y retorna True si su calidad es mayor a 20.
esValioso :: Material -> Bool 
esValioso material = (>20) . calidad $ material

-- > esValioso (Material "Madera de pino" 25)
-- True

-- b. unidadesDisponibles: Recibe el nombre de un material y una Aldea, y retorna la cantidad de materiales disponibles con ese nombre en la aldea.
unidadesDisponibles :: String -> Aldea -> Int 
unidadesDisponibles nombreMaterial aldea = length . filter (== nombreMaterial) . listaNombres $ aldea


-- de la aldea queremos [Material] que nos da el atributo materialesDisponibles, luego Material tiene atributo nombre, entonces a cada elemento de [Material] le mapeamos ese atributo para tener una [String] con los nombres, la filtramos por todas aquellas que sean iguales a el "nombreMaterial" que recibimos (solo con los que tengan ese nombre) quedandonos con una lista solo con la cantidad de materiales con ese nombre, finalmente conseguimos la cantidad con length 

listaNombres :: Aldea -> [String]
listaNombres aldea = map nombre . materialesDisponibles $ aldea

-- parto en esta funcion porque mas abajo en el punto 2 tenemos que nuevamente llegar a la lista de nombres, entonces para no repetir logica hago lo mismo aca 

-- > disponibles "Acero" (Aldea 50 [(Material "Acero" 15), (Material "Acero" 20), (Material "Piedra" 5)] [])
-- 2

--c. valorTotal: Recibe una aldea y retorna la suma de la calidad de todos los materiales que hay en la aldea (tanto los disponibles como los usados en sus edificios).
valorTotal :: Aldea -> Int 
valorTotal aldea = (+ sumaCalidadUsados aldea) . sumaCalidadDisponibles $ aldea

-- como queremos la suma de dos cosas podemos partirla en "sumaCalidadDisponibles" y en "sumaCalidadUsados" una recibe la aldea por composicion y la otra espera lo que devuelve la anterior para ser sumado, y la aplicamos parcialmente con aldea para que pueda obtener dicha suma

sumaCalidadDisponibles :: Aldea -> Int 
sumaCalidadDisponibles aldea = sum  . map calidad . materialesDisponibles $ aldea

-- recibe la aldea, de la misma queremos [Material] que nos da el atributo materialesDisponibles, de la misma Material tiene un atributo calidad que podemos mapear para conseguir una [Int] con las calidades de cada material disponible, finalmente sumamos cada cantidad para el total de las calidades disponibles

sumaCalidadUsados :: Aldea -> Int 
sumaCalidadUsados aldea = sum . map calidad . concat . map materiales . edificios $ aldea

-- recibe la aldea, de la misma queremos primero [Edificio] del atributo edificios, luego Edificio como data tiene un atributo materiales que nos da [Material], entonces si mapeamos con dicho atributo materiales conseguimos [[Material],...,[Material]], entonces para aplanarlo usamos "concat" [[Material],..., [Material]] => [Material], entonces al igual que el paso anterior mapeamos con calidad y sumamos cada una  

-- > valorTotal (Aldea 50 [(Material "Acero" 15), (Material "Piedra" 5)] [(Edificio "Barraca" [(Material "Acero" 20)])])
-- 40

-- ✅ 2. Desarrollar las siguientes tareas para que los gnomos puedan realizar en una aldea:
-- a. tenerGnomito: Aumenta la población de la aldea en 1.
tenerGnomito :: Aldea -> Aldea
tenerGnomito aldea = aldea { poblacion = poblacion aldea + 1 }

-- lo planteo asi porque mirando masomenos las demas no se pide generar un cambio al atributo poblacion de una aldea, en caso de haberlo usariamos un accesor

-- b. lustrarMaderas: Aumenta en 5 la calidad de los materiales disponibles cuyo nombre empiece con la palabra "Madera". El resto de los materiales disponibles no debería verse afectado al realizar esta tarea.
lustrarMaderas :: Aldea -> Aldea 
lustrarMaderas aldea = mapMaterialesDisponibles (map arrancaConMadera) aldea 

-- recibimos la aldea y queremos hacer un cambio al atributo materialesDisponibles, viendo la tarea siguiente tambien se pide un cambio a dicho atributo entonces planteamos un accesor al mismo, como el accesor entra directamente a [Material] desde alli solo a aquellos que arranquen con "Madera" queremos hacerle un cambio, esto con un filter no podremos hacerlo porque filtraria la lista, nosotros solo queremos agregar cosas a aquellos que cumplen algo entonces le mapeamos (porque [Material] es una lista) con otra funcion que partimos "arrancaConMadera" 

mapMaterialesDisponibles :: ([Material] -> [Material]) -> Aldea -> Aldea 
mapMaterialesDisponibles f aldea = aldea { materialesDisponibles = f . materialesDisponibles $ aldea }

arrancaConMadera :: Material -> Material 
arrancaConMadera material 
  | (== "Madera") . nombre $ material = material { calidad = calidad material + 5 }
  | otherwise = material 

-- esta al estar usada en el el accesor "mapMaterialesDisponibles" arranca de [Material] pero como la usamos mapeada para que labure con los elementos de [Material] entonces recibe los elementos de la misma, que son un Material, de cada uno los fijamos entonces si su nombre es "Madera" de serlo como el data tiene el atributo calidad planteamos dicho cambio de esa forma sumandole 5, entonces el map se encarga de hacer eso para todos los elementos de [Material]

-- c. recolectar: Dado un material y una cantidad, incorpora a los materiales disponibles de la aldea ese mismo material tantas veces como se indique.
recolectar :: Material -> Int -> Aldea -> Aldea
recolectar material cantidad aldea = mapMaterialesDisponibles (++ replicate cantidad material) aldea

-- tenemos que generar un cambio en el atributo materialesDisponibles de una aldea lo que queremos hacer es agregar este nuevo material a [Material] uno pensaria que seria mandarlo aplicado parcialmente pero un data Material no tiene atributo cantidad, si de calidad pero no de "cantidad", entonces lo que nos esta diciendo es agregar ese data material n veces, osea n listas con ese material cargado, para eso le pasamos al accesor que directamente entra a [Material], donde "replicate n x" genera n listas de ese x osea "replicate 3 "powerade"" => ["powerade"] ["powerade"] ["powerade"], entonces con "replicate cantidad material" mandamos a que cree por ejemplo [Material "Acero" 10] [Material "Acero" 10], finalmente esas n listas que genera las concatenamos al final de [Material]
-- "replicate n x" si queremos concatenarlo a una [x] es siempre al final con "++" 

-- ✅ 3. Realizar las consultas que permitan:
-- a. Obtener los edificios chetos de una aldea, que son aquellos que tienen algún material valioso.
edificiosChetos :: Aldea -> [Edificio]
edificiosChetos aldea = filter (any esValioso . materiales) . edificios $ aldea

-- de la aldea nos interesa su [Edificio] del mismo queremos filtrarlo segun aquellos que tengan "algun" material valioso, entonces como Edificio tiene un atributo materiales filtramos consiguiendo primero [Material] y viendo si alguno es valioso usando la funcion que creamos primero con "any esValioso"

obtenerEdificiosChetos :: [Edificio]
obtenerEdificiosChetos = edificiosChetos aldeaEjemplo

-- obtenerEdificiosChetos 
-- [Edificio {tipoEdificio = "Torre", materiales = [Material {nombre = "Hierro", calidad = 25}]}]

-- pedia crear una aldea de ejemplo con la cual guardar en una funcion constante que llamarla en consola es lo mismo que llamar a "edificiosCheto" pasandole la aldea, las dos hacen lo mismo 

aldeaEjemplo :: Aldea
aldeaEjemplo = Aldea
  { 
    poblacion = 10,
    materialesDisponibles = [Material "Piedra" 10, Material "Acero" 30], 
    edificios =[ Edificio "Casa" [Material "Madera" 15, Material "Piedra" 5] ,Edificio "Torre" [Material "Hierro" 25] ]
  }

-- b. Obtener una lista de nombres de materiales comunes, que son aquellos que se encuentran en todos los edificios de la aldea.
materialesComunes :: Aldea -> [String]
materialesComunes aldea = foldr1 intersect . map nombresMateriales . edificios $ aldea

-- de la aldea queremos [Edificio] que lo sacamos del atributo edificios, luego como cada Edificio tiene un atributo [Material] entonces [Edificio] = [[Material],...,[Material]] y de cada uno queremos el [String] que representa los nombres de cada material, entonces partimos y mapeamos con "nombresMateriales" con el cual conseguimos [[String]]
-- luego de esa [[String]] queremos quedarnos con [String] que tengan elementos en comun para eso la "simplificamos" usando foldr1 y una funcion binaria que va comparando a de a 2 elementos esta sera "intersect", que toma listas de dos cosas y devuelve otra lista con lo que tiene en comun, intersect [1,2,3,4] [3,4,5,6] => [3,4] 

-- foldr1 con una funcion binaria (si es necesario) es capaz de [[x]] => [x], y lo mismo si fuera [x] => x pero siempre debe haber una funcion binaria con un proposito

nombresMateriales :: Edificio -> [String]
nombresMateriales edificio = map nombre (materiales edificio)

-- recibe Edificio ya que esta siendo mapeada recibiendo los elementos de [Edificio] esto hara las cosas de forma individual, de cada Edificio queremos su [Material] con el atributo materiales, luego mapeamos con nombre para obtener [String], masomenos el movimiento que hace es el siguiente
-- [Edificio] => "map nombresMateriales" => [[Material],...,[Material]] => [[String],..., [String]] 


-- ✅ 4. Definir la función

-- realizarLasQueCumplan :: [Tarea] -> (Aldea -> Bool) -> Aldea -> Aldea
-- Recibe: Una lista de tareas, un criterio que debería cumplir la aldea luego de realizar cada tarea y una aldea inicial.
-- Debe retornar cómo quedaría la aldea si se realizaran las tareas válidas (una tras otra).
-- Una tarea es válida si, después de realizarse sobre una aldea (la original o la resultante de haber realizado otra tarea previa), la misma cumple con el criterio indicado.

