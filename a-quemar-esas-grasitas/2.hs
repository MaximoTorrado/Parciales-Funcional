module Lib where

type Peso = Int
type Tiempo = Int
type Grados = Int

-- 1. Modelar a los Gimnastas y las operaciones necesarias para hacerlos ganar tonificación y quemar calorías considerando que por cada 500 calorías quemadas se baja 1 kg de peso.
data Gimnasta = Gimnasta {
  peso :: Int, 
  tonificacion :: Int
} deriving (Show, Eq)

tonificar :: Int -> Gimnasta -> Gimnasta 
tonificar n gimnasta = gimnasta { tonificacion = tonificacion gimnasta + n }

quemarCalorias :: Int -> Gimnasta -> Gimnasta
quemarCalorias kcal gimnasta = gimnasta { peso = peso gimnasta - kcal `div` 500 }


data Rutina = Rutina {
  nombre :: String, 
  duracionTotal :: Tiempo
  -- ejercicios :: [Ejercicio]  esto esta bien pero deberiamos primero plantear los ejercicios del punto 2 para recien ahi plantear un type "Ejercicio"
} deriving (Show, Eq)

--2.a  Modelar los siguientes ejercicios del gimnasio:
  -- La cinta es una de las máquinas más populares entre los socios que quieren perder peso. Los gimnastas simplemente corren sobre la cinta y queman calorías en función de la velocidad promedio alcanzada (quemando 10 
  -- calorías por la velocidad promedio por minuto).

cinta :: Int -> Ejercicio -- Tiempo -> Gimnasta -> Gimnasta
cinta velocidad tiempo gimnasta = quemarCalorias (tiempo * velocidad * 10) gimnasta
  
  -- 2.a.i La cinta puede utilizarse para realizar dos ejercicios diferentes:
  -- La caminata es un ejercicio en cinta con velocidad constante de 5 km/h.

caminata :: Ejercicio   -- Tiempo -> Gimnasta -> Gimnasta
caminata tiempo gimnasta = cinta 5 tiempo gimnasta 

-- en el video lo que hace es hacer un point free de ambos "tiempo" y "gimnasta", pero es lo mismo es un llamado a cinta que caminara por n tiempo pero si sabemos que su velocidad sera constante, que es 5

  -- 2.a.ii El pique arranca en 20 km/h y cada minuto incrementa la velocidad en 1 km/h, con lo cual la velocidad promedio depende de los minutos de entrenamiento.
  
pique :: Ejercicio   -- Tiempo -> Gimnasta -> Gimnasta
pique tiempo gimnasta = cinta (tiempo `div` 2 + 20) tiempo gimnasta 

-- supuestamente modela la "velocidad promedio" que deberia ir tomando => (20 + (20 + tiempo)) / 2 => tiempo / 2 + 20 => tiempo `div` 2 + 20. 
-- Otro ejemplo si la velocidad iniciara en 67km y subiera 7 por minuto seria asi tiempo * 7 `div` 2 + 67 => tiempo * loQueAumenta `div` 2 + loQueArranca
  
  -- 2.b Las pesas son el equipo preferido de los que no quieren perder peso, sino ganar musculatura. Una sesión de levantamiento de pesas de más de 10 minutos hace que el gimnasta gane una tonificación equivalente a los kilos 
  -- levantados. Por otro lado, una sesión de menos de 10 minutos es demasiado corta, y no causa ningún efecto en el gimnasta.
  
pesas :: Peso -> Ejercicio   -- Tiempo -> Gimnasta -> Gimnasta
pesas kilos tiempo gimnasta 
  | tiempo > 10 = tonificar kilos gimnasta
  | otherwise = id gimnasta 
  
-- recibe los kilos el tiempo y el gimnasta, si el tiempo que le pasamos vemos que es mayor a 10 entonces llamamos a "tonificar" que agrega cierto numero a el Int del mismo, y sera igual a los kilos que le pasemos, caso contrario
-- devuelve el mismo gimnasta usando "id"  

  -- 2.c La colina es un ejercicio que consiste en ascender y descender sobre una superficie inclinada y quema 2 calorías por minuto multiplicado por la inclinación con la que se haya montado la superficie.
colina :: Grados -> Ejercicio   -- Tiempo -> Gimnasta -> Gimnasta
colina inclinacion tiempo gimnasta = quemarCalorias (2 * tiempo * inclinacion) gimnasta

-- la montaña, que consiste en 2 colinas sucesivas (asignando a cada una la mitad del tiempo total), donde la segunda colina se configura con una inclinación de 5 grados más que la 
-- inclinación de la primera. Además de la pérdida de peso por las calorías quemadas en las colinas, la montaña incrementa en 3 unidades la tonificación del gimnasta.

-- la montaña entonces hacer 2 colinas una atras de la otra para el mismo gimnasta, de forma sucesiva, de forma secuencial, entonces una manera de hacer esto es con composicion de "colina" minimo 2 veces 
montaña :: Grados -> Ejercicio   -- Tiempo -> Gimnasta -> Gimnasta
montaña inclinacion tiempo gimnasta = tonificar 3 . colina (inclinacion + 5) (tiempo `div` 2) . colina inclinacion (tiempo `div` 2) $ gimnasta

-- entonces secuencialmente las 2 colinas con la mitad de tiempo ambas, cambiando en la segunda la inclinacion 5 grados mas, y siempre la tonificacion es constante luego de montaña que es agregar 3  

-- Ahora mirando todos los ejercicios intentemos crear un type para ello, mirando que tienen en comun entre todos, lo que no tienen en comun puede ser parcialmente aplicado para ser ejercicios

type Ejercicio = Tiempo -> Gimnasta -> Gimnasta