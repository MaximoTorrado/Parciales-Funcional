module Lib where

type Peso = Int   -- estos 2 los planteamos pero los usamos mas adelante
type Tiempo = Int
type Grados = Int  -- con este

-- 1. Modelar a los Gimnastas y las operaciones necesarias para hacerlos ganar tonificación y quemar calorías considerando que por cada 500 calorías quemadas se baja 1 kg de peso.
data Gimnasta = Gimnasta {
  peso :: Int, 
  tonificacion :: Int
} deriving (Show, Eq)

-- usamos la segunda forma de modelar un cambio en un atributo de un data creando una copia con un atributo cambiado, podriamos usar el constructor devolviendo otro data pero deberiamos poner uno por uno todos los atributos
tonificar :: Int -> Gimnasta -> Gimnasta 
tonificar n gimnasta = gimnasta { tonificacion = tonificacion gimnasta + n }

quemarCalorias :: Int -> Gimnasta -> Gimnasta
quemarCalorias kcal gimnasta = gimnasta { peso = peso gimnasta - kcal `div` 500 }

--2.a  Modelar los siguientes ejercicios del gimnasio: La cinta es una de las máquinas más populares entre los socios que quieren perder peso. Los gimnastas simplemente corren sobre la cinta y queman calorías en función de la velocidad promedio alcanzada (quemando 10 calorías por la velocidad promedio por minuto).

cinta :: Int -> Ejercicio -- Tiempo -> Gimnasta -> Gimnasta
cinta velocidad tiempo gimnasta = quemarCalorias (tiempo * velocidad * 10) gimnasta
  
-- 2.a.i La cinta puede utilizarse para realizar dos ejercicios diferentes: La caminata es un ejercicio en cinta con velocidad constante de 5 km/h.

caminata :: Ejercicio   -- Tiempo -> Gimnasta -> Gimnasta
caminata tiempo gimnasta = cinta 5 tiempo gimnasta 

-- en el video lo que hace es hacer un point free de ambos "tiempo" y "gimnasta", pero es lo mismo es un llamado a cinta que caminara por n tiempo pero si sabemos que su velocidad sera constante, que es 5

-- 2.a.ii El pique arranca en 20 km/h y cada minuto incrementa la velocidad en 1 km/h, con lo cual la velocidad promedio depende de los minutos de entrenamiento.

pique :: Ejercicio   -- Tiempo -> Gimnasta -> Gimnasta
pique tiempo gimnasta = cinta (tiempo `div` 2 + 20) tiempo gimnasta 

-- supuestamente modela la "velocidad promedio" que deberia ir tomando => (20 + (20 + tiempo)) / 2 => tiempo / 2 + 20 => tiempo `div` 2 + 20. 
-- Otro ejemplo si la velocidad iniciara en 67km y subiera 7 por minuto seria asi tiempo * 7 `div` 2 + 67 => tiempo * loQueAumenta `div` 2 + loQueArranca
  
-- 2.b Las pesas son el equipo preferido de los que no quieren perder peso, sino ganar musculatura. Una sesión de levantamiento de pesas de más de 10 minutos hace que el gimnasta gane una tonificación equivalente a los kilos levantados. Por otro lado, una sesión de menos de 10 minutos es demasiado corta, y no causa ningún efecto en el gimnasta.

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

-- 3. Implementar una función realizarRutina, que dada una rutina y un gimnasta retorna el gimnasta resultante de realizar todos los ejercicios de la rutina, repartiendo el tiempo total de la rutina en partes iguales. Mostrar 
-- un ejemplo de uso con una rutina que incluya todos los ejercicios del punto anterior.

data Rutina = Rutina {
  nombre :: String, 
  duracionTotal :: Tiempo,
  ejercicios :: [Ejercicio]
} 

realizarRutina :: Gimnasta -> Rutina -> Gimnasta
realizarRutina gimnastaInicial rutina = foldl (\acu x -> x (tiempoParaEjercicio rutina) acu) gimnastaInicial (ejercicios rutina)

-- tenemos que hacer que un gimnasta se aplique a una [Ejercicio] de una rutina, para eso usamos foldl que recibe una funcion incognita, una semilla y la lista, la lista sera la que sale del atributo "ejercicios" de la rutina, la semilla sera el "gimnastaInicial" y la incognita recibe primero el valor de la semilla en "acu" y luego los elementos de la lista en "x" 
-- recordemos que los elementos de la lista son "type Ejercicio = Tiempo -> Gimnasta -> Gimnasta" entonces no solo tenemos que pasarle el gimnasta si no que tambien tenemos que pasarle el tiempo, y como pide "repartiendo el tiempo total de la rutina en partes iguales" lo podemos partir y definir en otra funcion "tiempoParaEjercicio"

-- realizarRutina (Gimnasta 90 0) (Rutina "Mi Rutina" 30 [caminata, pique, pesas 5])
-- Gimnasta {peso = 84, tonificacion = 0}
-- realizarRutina (Gimnasta 90 0) (Rutina "Mi Rutina" 30 [caminata, pique, pesas 5, colina 30, montaña 30])
-- Gimnasta {peso = 88, tonificacion = 3}

tiempoParaEjercicio :: Rutina -> Tiempo
tiempoParaEjercicio rutina = (div (duracionTotal rutina) . length . ejercicios) rutina

-- para que sean en tiempos iguales conseguimos la lista de ejercicios de la rutina, vemos la cantidad de ejercicios y lo dividimos por el "duracionTotal" de la rutina

--4. Definir las operaciones necesarias para hacer las siguientes consultas a partir de una lista de rutinas:
  -- 4.a ¿Qué cantidad de ejercicios tiene la rutina con más ejercicios?
mayorCantidadDeEjercicios :: [Rutina] -> Int
mayorCantidadDeEjercicios rutinas = maximum . map (length . ejercicios) $ rutinas

-- a partir de [Rutina] = [Rutina, ... RutinaN] sabemos que "Rutina" como data solo tiene atributo [Ejercicio], entonces [Rutina] tendra un atributos [[Ejercicio],... [EjercicioN]], entonces como queremos conseguir esta lista de listas usamos un map que consiga "ejercicios" de cada elemento de [Rutina] osea de cada ejercicio, y de cada una consiga su cantidad de elementos para ver cual tiene mas ejercicios, consiguiendo esto [[Ejercicio],... [EjercicioN]] => [3,1,..,8]         y finalmente vemos cual tiene mas ejercicios usando "maximum"                           

-- 4.b ¿Cuáles son los nombres de las rutinas que hacen que un gimnasta dado gane tonificación?
nombresDeRutinasTonificantes :: Gimnasta -> [Rutina] -> [String]
nombresDeRutinasTonificantes gimnasta rutinas = (map nombre . filter ((>) (tonificacion gimnasta) . tonificacion . realizarRutina gimnasta)) rutinas

-- a partir de un gimnasta y una lista de rutinas, queremos filtrar [Rutina] para que se que solo con aquellos que luego de realizar dicha rutina cambian de tonificacion para luego conseguir los nombres de cada rutina que cumple con esto, la hermosura esta en la funcion que le pasamos al "filter" donde adentro hacemos que el gimnasta realice una rutina de "rutinas" con "realizarRutina gimnasta" luego que se fije su "tonificacion" AHORA podemos comparar esta con "tonificacion gimnasta" que es LA TONIFICACION CON LA QUE VINO EL GIMNASTA porque NO esta recibiendo el gimnasta luego de realizar la rutina como SI lo hace "tonificacion" solo, haciendo eso  podemos comparar la de luego de realizar y la original, con [Rutina] filtrada solo conseguimos los "nombre" de todos los elementos con un mapeo

-- 4.c ¿Hay alguna rutina peligrosa para cierto gimnasta? Decimos que una rutina es peligrosa para alguien si lo hace perder más de la mitad de su peso. 
hayPeligrosa :: Gimnasta -> [Rutina] -> Bool
hayPeligrosa gimnasta rutinas = any ((< peso gimnasta `div` 2) . peso . realizarRutina gimnasta) rutinas 

-- lo mismo que el anterior pero ahora queremos ver si "alguna cumple" entonces usamos "any", nuevamente tenemos que dentro de any recibir una funcion que primero aplique la rutina a un gimnasta y a ese mismo gimnasta luego de realizar la rutina mire su peso "peso . realizarRutina gimnasta" para luego comparar si es menor de la mitad del peso del gimnasta original "(< peso gimnasta `div` 2)" que nuevamente aca no esta recibiendo el gimnasta luego de realizar la rutina como si lo hace "peso" solo, si no que ya esta cargado con el gimnasta que recibe de parametro mira esto secuencialmente con todas las rutinas de "rutinas" y se fija si alguna cumple
