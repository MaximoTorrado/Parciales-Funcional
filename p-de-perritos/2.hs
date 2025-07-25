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

type Tiempo = Int                                       
-- type Guarderia = (Ejercicio, Tiempo)
-- type Ejercicio =                                   esto veremos que generalizamos luego de plantear los ejercicios, para ver cuales se tienen que aplicar parcialmente para ser un ejercicio y asi 

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

 -- diaDeSpa: si el perro va a permanecer 50 minutos como mínimo en la guardería o es de raza extravagante, su energía pasa a ser 100 🔋 y se le regala el juguete "peine de goma” 🪮. Si no, no pasa nada. Las razas extravagantes 
 -- son dálmata y pomerania.
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
type Guarderia = (Ejercicio, Tiempo)