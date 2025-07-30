import Text.Show.Functions

{- Parte 1

Hablemos de los protagonistas: los perritos 🐶. De cada uno conocemos:
 su raza,
 sus juguetes favoritos,
 el tiempo que va a permanecer en la guardería,
 y la energía que tiene. -}

data Perro = Perro {
  raza :: String, 
  juguete :: [String],
  tiempoDePermanencia :: Int,
  energia :: Int 
} deriving (Show)

-- Si miramos bien de que trata cada ejercicio son cambios a algun atributo del data Perro, pero si miramos mas a fondo solo hacemos los accesors de aquellos atributos que tengan los cambios para no repetir logica
mapJuguetes :: ([String] -> [String]) -> Perro -> Perro
mapJuguetes f perro = perro { juguete = f . juguete $ perro }

mapEnergia :: (Int -> Int) -> Perro -> Perro
mapEnergia f perro = perro { energia = max 0 . f . energia $ perro }
-- con esto que agregamos hicimos que por cada disminucion de la energia siempre sea positiva

-- De las guarderías sabemos que tienen un nombre y una rutina para entretener a los pichichos. La rutina es un conjunto de actividades, compuestas por el ejercicio y el tiempo que éste dura en minutos. 
data Guarderia = Guarderia {
  nombre :: String, 
  rutina :: [Actividad]          -- entonces sera una lista de tuplas [Actividad] => [(Ejercicio, Int),...,(Ejercicio, Int)]
}

-- type Ejercicio =                esto lo definimos mas abajo luego de plantear los ejercicios para ver bien que tienen en comun entre ellos
type Actividad = (Ejercicio, Int)

-- Algunos de estos ejercicios son:
-- jugar: disminuye en 10 unidades la energía del perrito 🪫. ¡No puede quedar un valor negativo!
jugar :: Ejercicio  -- Perro -> Perro
jugar perro = mapEnergia (subtract 10) perro

-- para "disminuir" o "restar" algo usamos "subtract". Luego como no queremos que sea negativo pero estamos usando el accesor entonces la condicion "max 0" la pondremos en el accesor directamente
-- jugar (Perro "dalmata" ["pelota", "hueso", "cuerda"] 60 50)
-- Perro {raza = "dalmata", juguete = ["pelota","hueso","cuerda"], tiempoDePermanencia = 60, energia = 40}

-- ladrar: aumenta la energía la mitad de los ladridos que se establezcan. 🗣️
ladrar :: Int -> Ejercicio   -- Perro -> Perro
ladrar ladridos perro = mapEnergia (+ ladridos `div` 2) perro

-- ladrar 10 (Perro "dalmata" ["pelota", "hueso", "cuerda"] 60 50)
-- Perro {raza = "dalmata", juguete = ["pelota","hueso","cuerda"], tiempoDePermanencia = 60, energia = 55}

-- regalar: ¡cómo no le vamos a dar un juguetito! Añade el juguete que se especifique a los favoritos. 🎁
regalar :: String -> Ejercicio     --  Perro -> Perro
regalar juguete perro = mapJuguetes (juguete :) perro

-- regalar "tela" (Perro "dalmata" ["pelota", "hueso", "cuerda"] 60 50)
-- Perro {raza = "dalmata", juguete = ["tela","pelota","hueso","cuerda"], tiempoDePermanencia = 60, energia = 50}

-- para agregar al principio es "String :"

-- diaDeSpa: si el perro va a permanecer 50 minutos como mínimo en la guardería o es de raza extravagante, su energía pasa a ser 100 🔋 y se le regala el juguete "peine de goma” 🪮. Si no, no pasa nada. Las razas extravagantes son dálmata y pomerania.
diaDeSpa :: Ejercicio      -- Perro -> Perro 
diaDeSpa perro 
  | tiempoDePermanencia perro >= 50 || esDeRazaExtravagante perro = mapEnergia (const 100) . regalar "peine de goma" $ perro
  | otherwise = id perro 

esDeRazaExtravagante :: Perro -> Bool
esDeRazaExtravagante perro = raza perro == "dalmata" || raza perro == "pomerania"

-- diaDeSpa (Perro "pomerania" ["pelota", "hueso", "cuerda"] 22 50)
-- Perro {raza = "pomerania", juguete = ["peine de goma","pelota","hueso","cuerda"], tiempoDePermanencia = 22, energia = 100}
-- diaDeSpa (Perro "dalmata" ["pelota", "hueso", "cuerda"] 21 50)  
-- Perro {raza = "dalmata", juguete = ["peine de goma","pelota","hueso","cuerda"], tiempoDePermanencia = 21, energia = 100}
-- diaDeSpa (Perro "chihuahua" ["pelota", "hueso", "cuerda"] 21 50)
-- Perro {raza = "chihuahua", juguete = ["pelota","hueso","cuerda"], tiempoDePermanencia = 21, energia = 50}

-- diaDeCampo: ¡nada más lindo que ver perritos jugar! Lástima que así siempre pierden el primer juguete.😅
diaDeCampo :: Ejercicio          -- Perro -> Perro
diaDeCampo perro = mapJuguetes (drop 1) perro

-- el "take n" toma esa cantidad y descarta el resto, el "drop n" dropea esa cantidad y se queda con el resto, ambas desde el inicio 

-- diaDeCampo (Perro "chihuahua" ["pelota", "hueso", "cuerda"] 21 50)
-- Perro {raza = "chihuahua", juguete = ["hueso","cuerda"], tiempoDePermanencia = 21, energia = 50}

-- Ahora mirando los ejercicios que planteamos podemos generalizar lo comun 
type Ejercicio = Perro -> Perro

{- Modelar:
Los ejercicios antes mencionados.
A Zara, una perra dálmata que tiene como juguetes favoritos una pelota y una mantita, permanece 1 hora y media en la guardería y su energía es de 80. 
La GuarderíaPdePerritos que tiene como rutina las siguientes actividades: 
 
 Ejercicio         Tiempo (min)
Jugar                 30
Ladrar 18             20
Regalar pelota        0
Día de spa            120
Día de campo          720 -}

zara :: Perro
zara = Perro { raza = "dalmata", juguete = ["pelota", "mantita"], tiempoDePermanencia = 90, energia = 80 }

guarderiaDePerritos :: Guarderia
guarderiaDePerritos = Guarderia { nombre = "guarderiaPDePerritos", rutina = [(jugar, 30), (ladrar 18, 20), (regalar "Pelota", 0), (diaDeSpa, 0), (diaDeCampo, 0)] }

-- recordemos que el atributo "rutina" del data Guarderia es [Actividad] que es una lista de tuplas

-- Y entonces… ¿cuándo abreeeeeee? ✋Primero debemos asegurarnos de:
-- Saber si un perro puede estar en una guardería. Para eso, el tiempo de permanencia tiene que ser mayor que el de la rutina. 🕰️
puedeEstar :: Guarderia -> Perro -> Bool
puedeEstar guarderia perro = tiempoDePermanencia perro > tiempoRutina (rutina guarderia)

tiempoRutina :: [Actividad] -> Int
tiempoRutina actividades = sum . map snd $ actividades 

-- el atributo rutina de una guarderia es [Actividad] = [(Ejercicio, Int)], entonces como el segundo miembro representa el tiempo de cada actividad, queremos quedarnos solo con ese miembro, entonces mapeamos con "snd" a cada tupla quedandonos con [1,4223,32] y finalmente sumamos los tiempos de la rutina total

-- Reconocer a perros responsables. Estos serían los que aún después de pasar un día de campo siguen teniendo más de 3 juguetes. 🧸
esPerroResponsable :: Perro -> Bool
esPerroResponsable perro = sigueTeniendo (diaDeCampo perro)

sigueTeniendo :: Perro -> Bool
sigueTeniendo perro = (>3) . length . juguete $ perro

-- a sigue teniendo directamente le pasamos el perro que devuelve despues de haber pasado por el ejercicio "diaDeCampo" y miramos la cantidad de juguetes que tiene

-- Que un perro realice una rutina de la guardería (que realice todos sus ejercicios). Para eso, el tiempo de la rutina no puede ser mayor al tiempo de permanencia. En caso de que esta condición no se cumpla, el perro no hace nada. 🐕

-- aca en un principio yo entendi que recibimos una guarderia para de dicha hacer los movimientos para obtener su rutina y seguido su [Ejercicio], pero tambien podria haber recibido directamente [Ejercicio] suponiendo que al llamar la funcion directamente pido que me pasen esa [Ejercicio]

realizarRutina :: Guarderia -> Perro -> Perro 
realizarRutina guarderia perro 
  | esPosible (rutina guarderia) perro = aplicarRutina (rutina guarderia) perro
  | otherwise = id perro

-- recibimos una guarderia y un perro, la condicion la partimos en otra funcion para no andar poniendo todo el choclo ahi, le mandamos directamente [Actividad] de la rutina y el perro del cual podra obtener el atributo tiempoDePermanencia, luego para aplicar tambien lo partimos en otra funcion para definir mas adelante 

esPosible :: [Actividad] -> Perro -> Bool
esPosible rutina perro = tiempoRutina rutina < tiempoDePermanencia perro

-- recibe [Actividad] = [(Ejercicio, Int)], nosotros especificamente queremos ver el total del tiempo que la logica ya la hicimos en "tiempoRutina" que mapea snd y suma, luego miramos si es menor al atributo tiempoDePermanencia del perro

aplicarRutina :: [Actividad] -> Perro -> Perro
aplicarRutina rutina perro = foldl (\acu x -> x acu) perro (ejercicios rutina) 

-- queremos que el valor Perro sea aplicado a todos los elementos de [Actividad] pero ojo recordemos que [Actividad] = [(Ejercicio, Int)], nosotros queremos en realidad aplicar todos los elementos de [Ejercicio] a dicho valor, entonces primero obtenemos esa [Ejercicio] con "ejercicios", entonces con un foldl que recibe una incognita una semilla y la [Ejercicio], la incognita recibe primero el valor de la semilla que sea un Perro con "acu" y luego los elementos de [Ejercicio] en "x", entonces a todos los elementos de [Ejercicio] le aplicamos dicho perro "x acu"

ejercicios :: [Actividad] -> [Ejercicio]
ejercicios rutina = map fst rutina

-- de la lista de tuplas entonces que representa una actividad queremos solo la primera para tener una [Ejercicio] entonces mapeamos con fst

-- Dados unos perros, reportar todos los que quedan cansados después de realizar la rutina de una guardería. Es decir, que su energía sea menor a 5 luego de realizar todos los ejercicios. 💤
perrosCansados :: Guarderia -> [Perro] -> [Perro]
perrosCansados guarderia perros = filter (estaCansados . realizarRutina guarderia) perros

estaCansados :: Perro -> Bool
estaCansados perro = (< 5) . energia $ perro

-- asi directamente usando lo que definimos anteriormente es mas visible, queremos filtrar [Perro] por aquellos que quedan cansados (energia menor a 5) pero luego de aplicar una rutina de una guarderia, y tenemos "realizarRutina" que si le aplicamos parcialmente la guarderia tenemos todo porque se encarga de llegar a [Ejercicio] y aplicarlo a un perro y devolver un perro despues de el ejercicio, finalmente "estaCansado" se fija de dicho perro la condicion, y el filter se encarga de hacerlo secuencialmente para cada elemento de [Perro] sin necesidad de nosotros hacelo, aunque la idea esta buena

-- yo lo habia pensado y planteado masomenos "de 0" guiandome por el ejercicio 6 y 7 de monsters inc donde se aplica una lista de funciones a una lista de valores, la idea no esta mal pero podria ser mas facil usando las funciones que fuimos creando anteriormente

{- Parte 3

¡Infinita diversión! ♾️ Pi es un perrito un poco especial… Su raza es labrador y tiene muchos, muchos, incontables juguetes favoritos. Con la particularidad de que son todas soguitas numeradas del 1 al infinito. Su tiempo de permanencia es de 314 minutos y su energía es de 159.
Luego de modelar a Pi, respondé las siguientes preguntas justificando y escribiendo la consulta que harías en la consola:  -}

perroPi :: Perro 
perroPi = Perro {
  raza = "labrador", 
  juguete = sogasInfinitas,
  tiempoDePermanencia = 314,
  energia = 159
}

sogasInfinitas :: [String]
sogasInfinitas = map (\x -> "Soguita" ++ show x) [1..]

-- como lo que queremos es infinitas sogas pero para eso entonces tenemos que ordenarlas en una lista infinita de [Soguita 1, Soguita 2, ..., entonces con un map con una incognita y una lista infinita que parte del 1, la incognita va tomando elementos de la lista infinita en "x" y por cada una hace "Soguita" que se concatene con el elemento de la lista "x" usando "show" 

--1. ¿Sería posible saber si Pi es de una raza extravagante?
-- Si, es posible y da "False", si bien tiene un atributo que es una lista infinita gracias a lazy evaluation solo evalua los atributos que se necesitan, y como para dictaminar esto se necesita solamente de el atributo raza entonces todo bien

--esDeRazaExtravagante perroPi
--False

--2. ¿Qué pasa si queremos saber si Pi tiene…
--  a. … algún huesito como juguete favorito? 
tieneJuguete :: String -> Perro -> Bool
tieneJuguete juguetazo perro  = elem juguetazo . juguete $ perro

-- Depende, dado el caso que el juguete que querramos averiguar esta concatenado a la lista infinita podra encontrarlo en algun momento, ahora el caso donde no este ese juguete y nosotros esperemos un False no podremos porque nunca terminara de evaluar la lista infinita entonces termina rompiendo

-- tieneJuguete "Huesito" perroPi
-- True (si lo tiene)
-- tieneJuguete "Huesito" perroPi
-- -     (si no lo tiene, nunca termina de evaluar)

--  b. …alguna pelota luego de pasar por la Guardería de Perritos?
-- Si, al hacer la rutina de guarderiaDePerritos los atributos que estan en juego son energia, tiempoDePermanencia y por ultimo juguete pero a esta ultima obligatoriamente agrega al principio una pelota como juguete, esto es valido agregar al principio de una lista infinita, entonces al querer buscar dicha pelota en la lista infinita la encontrar

-- realizarRutina guarderiaDePerritos perroPi . tieneJuguete "Pelota" perroPi
-- True

--  c. … la soguita 31112?
-- Si, eventualmente encontrara dicho juguete dentro de la lista infinita porque esta contemplado en su dominio por mas grande que sea (quizas tarde un toque jajaj, mentira no tarda nada comprobado XD)

-- tieneJuguete "Soguita31112" perroPi
-- True

-- 3. ¿Es posible que Pi realice una rutina?
-- Recien lo comprobe le cambia la energia peeero al "realizarRutina" finalmente devuelve un Perro, entonces cuando quiera mostrar a perroPi que en si hace la rutina la puede hacer sin problemas, pero cuando quiera mostrarlo nunca termina de mostrar las soguitas justamente en el atributo de la lista infinita
-- la idea que se me ocurre es hacer una funcion rutina que si le genere el cambio que es a los atributos energia y tiempoDePermanencia sin mostrarlo, pero no serviria de nada digamos y no esta en este contexto

-- realizarRutina guarderiaDePerritos perroPi
-- -      (nunca puede terminar de mostrar los juguetes)

-- 4. ¿Qué pasa si le regalamos un hueso a Pi?  
-- esta haciendo referencia a la funcion "regalar" y justamente lo mismo que la anterior la definimos para que devuelva el perro con el atributo juguetes cambiado, indiferentemente si es agregar al principio (que se podria) o al final (que no se podria) no podriamos mostrarlo nunca por lo mismo nunca termina de mostrar los juguetes