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



tiempoParaEjercicio :: Rutina -> Tiempo
tiempoParaEjercicio rutina = (div (duracionTotal rutina) . length . ejercicios) rutina

--4. Definir las operaciones necesarias para hacer las siguientes consultas a partir de una lista de rutinas:
  -- 4.a ¿Qué cantidad de ejercicios tiene la rutina con más ejercicios?

mayorCantidadDeEjercicios :: [Rutina] -> Int
mayorCantidadDeEjercicios rutinas = maximum . map (length . ejercicios) $ rutinas

-- a partir de [Rutina] = [Rutina, ... RutinaN] sabemos que "Rutina" como data solo tiene atributo [Ejercicio], entonces [Rutina] tendra un atributos [[Ejercicio],... [EjercicioN]], entonces como queremos conseguir esta 
-- lista de listas usamos un map que consiga "ejercicios" de cada elemento de [Rutina] osea de cada ejercicio, y de cada una consiga su cantidad de elementos para ver cual tiene mas ejercicios, consiguiendo esto
-- [[Ejercicio],... [EjercicioN]] => [3,1,..,8]         y finalmente vemos cual tiene mas ejercicios usando "maximum"                           

  -- 4.b ¿Cuáles son los nombres de las rutinas que hacen que un gimnasta dado gane tonificación?

nombresDeRutinasTonificantes :: Gimnasta -> [Rutina] -> [String]
nombresDeRutinasTonificantes gimnasta rutinas = (map nombre . filter ((>) (tonificacion gimnasta) . tonificacion . realizarRutina gimnasta)) rutinas

-- a partir de un gimnasta y una lista de rutinas, queremos filtrar [Rutina] para que se que solo con aquellos que luego de realizar dicha rutina cambian de tonificacion para luego conseguir los nombres de cada rutina que 
-- cumple con esto, la hermosura esta en la funcion que le pasamos al "filter" donde adentro hacemos que el gimnasta realice una rutina de "rutinas" con "realizarRutina gimnasta" luego que se fije su "tonificacion" AHORA
-- podemos comparar esta con "tonificacion gimnasta" que es LA TONIFICACION CON LA QUE VINO EL GIMNASTA porque NO esta recibiendo el gimnasta luego de realizar la rutina como SI lo hace "tonificacion" solo, haciendo eso 
-- podemos comparar la de luego de realizar y la original, con [Rutina] filtrada solo conseguimos los "nombre" de todos los elementos con un mapeo

  -- 4.c ¿Hay alguna rutina peligrosa para cierto gimnasta? Decimos que una rutina es peligrosa para alguien si lo hace perder más de la mitad de su peso. 

hayPeligrosa :: Gimnasta -> [Rutina] -> Bool
hayPeligrosa gimnasta rutinas = any ((< peso gimnasta `div` 2) . peso . realizarRutina gimnasta) rutinas 

-- lo mismo que el anterior pero ahora queremos ver si "alguna cumple" entonces usamos "any", nuevamente tenemos que dentro de any recibir una funcion que primero aplique la rutina a un gimnasta y a ese mismo gimnasta luego
-- de realizar la rutina mire su peso "peso . realizarRutina gimnasta" para luego comparar si es menor de la mitad del peso del gimnasta original "(< peso gimnasta `div` 2)" que nuevamente aca no esta recibiendo el gimnasta
-- luego de realizar la rutina como si lo hace "peso" solo, si no que ya esta cargado con el gimnasta que recibe de parametro
-- mira esto secuencialmente con todas las rutinas de "rutinas" y se fija si alguna cumple




