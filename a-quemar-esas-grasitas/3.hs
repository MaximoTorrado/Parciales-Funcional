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

--2.a  Modelar los siguientes ejercicios del gimnasio:

type Ejercicio = Tiempo -> Gimnasta -> Gimnasta

cinta :: Int -> Ejercicio -- Tiempo -> Gimnasta -> Gimnasta
cinta velocidad tiempo gimnasta = quemarCalorias (tiempo * velocidad * 10) gimnasta
  
-- 2.a.i La cinta puede utilizarse para realizar dos ejercicios diferentes:
-- La caminata es un ejercicio en cinta con velocidad constante de 5 km/h.
caminata :: Ejercicio   -- Tiempo -> Gimnasta -> Gimnasta
caminata tiempo gimnasta = cinta 5 tiempo gimnasta 

-- 2.a.ii El pique arranca en 20 km/h y cada minuto incrementa la velocidad en 1 km/h, con lo cual la velocidad promedio depende de los minutos de entrenamiento. 
pique :: Ejercicio   
pique tiempo gimnasta = cinta (tiempo `div` 2 + 20) tiempo gimnasta 
  
-- 2.b Las pesas son el equipo preferido de los que no quieren perder peso, sino ganar musculatura. Una sesión de levantamiento de pesas de más de 10 minutos hace que el gimnasta gane una tonificación equivalente a los kilos 
-- levantados. Por otro lado, una sesión de menos de 10 minutos es demasiado corta, y no causa ningún efecto en el gimnasta.  
pesas :: Peso -> Ejercicio   
pesas kilos tiempo gimnasta 
  | tiempo > 10 = tonificar kilos gimnasta
  | otherwise = id gimnasta 

-- 2.c La colina es un ejercicio que consiste en ascender y descender sobre una superficie inclinada y quema 2 calorías por minuto multiplicado por la inclinación con la que se haya montado la superficie.
colina :: Grados -> Ejercicio  
colina inclinacion tiempo gimnasta = quemarCalorias (2 * tiempo * inclinacion) gimnasta

-- la montaña, que consiste en 2 colinas sucesivas (asignando a cada una la mitad del tiempo total), donde la segunda colina se configura con una inclinación de 5 grados más que la inclinación de la primera. Además de la pérdida 
-- de peso por las calorías quemadas en las colinas, la montaña incrementa en 3 unidades la tonificación del gimnasta.
montaña :: Grados -> Ejercicio   -- Tiempo -> Gimnasta -> Gimnasta
montaña inclinacion tiempo gimnasta = tonificar 3 . colina (inclinacion + 5) (tiempo `div` 2) . colina inclinacion (tiempo `div` 2) $ gimnasta

-- 3. Implementar una función realizarRutina, que dada una rutina y un gimnasta retorna el gimnasta resultante de realizar todos los ejercicios de la rutina, repartiendo el tiempo total de la rutina en partes iguales. Mostrar 
-- un ejemplo de uso con una rutina que incluya todos los ejercicios del punto anterior.

data Rutina = Rutina {
  nombre :: String, 
  duracionTotal :: Tiempo,
  ejercicios :: [Ejercicio]
} 

realizarRutina :: Gimnasta -> Rutina -> Gimnasta
realizarRutina gimnastaInicial rutina = foldl (\acu x -> x (tiempoParaEjercicio rutina) acu) gimnastaInicial (ejercicios rutina)

-- tenemos que hacer que un gimnasta se aplique a una [Ejercicio] de una rutina, para eso usamos foldl que recibe una funcion incognita, una semilla y la lista, la lista sera la que sale del atributo "ejercicios" de la rutina, la 
-- semilla sera el "gimnastaInicial" y la incognita recibe primero el valor de la semilla en "acu" y luego los elementos de la lista en "x" 
-- recordemos que los elementos de la lista son "type Ejercicio = Tiempo -> Gimnasta -> Gimnasta" entonces no solo tenemos que pasarle el gimnasta si no que tambien tenemos que pasarle el tiempo, y como pide "repartiendo el tiempo
-- total de la rutina en partes iguales" lo podemos partir y definir en otra funcion "tiempoParaEjercicio"

-- realizarRutina (Gimnasta 90 0) (Rutina "Mi Rutina" 30 [caminata, pique, pesas 5])
-- Gimnasta {peso = 84, tonificacion = 0}
-- realizarRutina (Gimnasta 90 0) (Rutina "Mi Rutina" 30 [caminata, pique, pesas 5, colina 30, montaña 30])
-- Gimnasta {peso = 88, tonificacion = 3}

tiempoParaEjercicio :: Rutina -> Tiempo
tiempoParaEjercicio rutina = (div (duracionTotal rutina) . length . ejercicios) rutina

-- para que sean en tiempos iguales conseguimos la lista de ejercicios de la rutina, vemos la cantidad de ejercicios y lo dividimos por el "duracionTotal" de la rutina

































