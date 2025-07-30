-- 🧙‍♂️ Gnomos!!! Queremos modelar parte de un juego inspirado en el Gnomoria, donde se maneja una aldea y la misma puede transformarse con el trabajo de los gnomitos que la habitan. Para ello definimos y modelamos los materiales de construcción para los edificios, los edificios para construir en la aldea y la aldea misma de la siguiente forma:

data Material = Material {
  nombre  :: String,
  calidad :: Int
} deriving (Show, Eq)

data Edificio = Edificio {
  tipoEdificio :: String,
  materiales   :: [Material]
} deriving (Show, Eq)

data Aldea = Aldea {
  poblacion             :: Int,
  materialesDisponibles :: [Material],
  edificios             :: [Edificio]
} deriving (Show, Eq)

-- ✅ 1. Desarrollar las siguientes funciones:
-- a. esValioso: Recibe un Material y retorna True si su calidad es mayor a 20.

-- > esValioso (Material "Madera de pino" 25)
-- True

-- b. unidadesDisponibles: Recibe el nombre de un material y una Aldea, y retorna la cantidad de materiales disponibles con ese nombre en la aldea.

-- > disponibles "Acero" (Aldea 50 [(Material "Acero" 15), (Material "Acero" 20), (Material "Piedra" 5)] [])
-- 2

--c. valorTotal: Recibe una aldea y retorna la suma de la calidad de todos los materiales que hay en la aldea (tanto los disponibles como los usados en sus edificios).

-- > valorTotal (Aldea 50 [(Material "Acero" 15), (Material "Piedra" 5)] [(Edificio "Barraca" [(Material "Acero" 20)])])
-- 40

-- ✅ 2. Desarrollar las siguientes tareas para que los gnomos puedan realizar en una aldea:
-- a. tenerGnomito: Aumenta la población de la aldea en 1.

-- b. lustrarMaderas: Aumenta en 5 la calidad de los materiales disponibles cuyo nombre empiece con la palabra "Madera". El resto de los materiales disponibles no debería verse afectado al realizar esta tarea.

-- c. recolectar: Dado un material y una cantidad, incorpora a los materiales disponibles de la aldea ese mismo material tantas veces como se indique.

-- ✅ 3. Realizar las consultas que permitan:
-- a. Obtener los edificios chetos de una aldea, que son aquellos que tienen algún material valioso.

-- b. Obtener una lista de nombres de materiales comunes, que son aquellos que se encuentran en todos los edificios de la aldea.

-- ✅ 4. Definir la función

-- realizarLasQueCumplan :: [Tarea] -> (Aldea -> Bool) -> Aldea -> Aldea
-- Recibe: Una lista de tareas, un criterio que debería cumplir la aldea luego de realizar cada tarea y una aldea inicial.
-- Debe retornar cómo quedaría la aldea si se realizaran las tareas válidas (una tras otra).
-- Una tarea es válida si, después de realizarse sobre una aldea (la original o la resultante de haber realizado otra tarea previa), la misma cumple con el criterio indicado.

-- ✅ 5. Hacer consultas usando realizarLasQueCumplan de forma tal que:
-- i. Se tengan gnomitos 3 veces (como 3 tareas independientes entre sí), asegurando que siempre haya suficiente unidad de comida disponible (que la cantidad de unidades de comida en la aldea supere la cantidad de gnomitos a tener). Un material con el nombre "Comida" se considera una unidad de comida.

-- ii. Se recolecten 30 unidades de madera de pino de calidad igual a la calidad máxima de las maderas disponibles, y luego se lustren las maderas disponibles de la aldea, asegurando siempre que todos los materiales disponibles sean valiosos.
