data Perro = Perro {
    raza :: String,
    juguetesFavoritos :: [String],
    tiempoDePermanencia :: Int,
    energia :: Int 
} 

type Actividad = (Ejercicio, Int)
--getters
ejercicio :: Actividad -> Ejercicio
ejercicio = fst
duracionActividad :: Actividad -> Int
duracionActividad = snd

type Ejercicio = Perro -> Perro

data Guarderia = Guarderia {
    nombre :: String,
    rutina :: [Actividad]
} 

mapRaza :: (String -> String) -> Perro -> Perro 
mapRaza funcionModificacion unPerro = unPerro { raza = funcionModificacion . raza $ unPerro }

mapJuguetesFavoritos :: ([String] -> [String]) -> Perro -> Perro 
mapJuguetesFavoritos funcionModificacion unPerro = unPerro { juguetesFavoritos = funcionModificacion . juguetesFavoritos $ unPerro }

mapTiempoDePermanencia :: (Int -> Int) -> Perro -> Perro 
mapTiempoDePermanencia funcionModificacion unPerro = unPerro { tiempoDePermanencia = funcionModificacion . tiempoDePermanencia $ unPerro }

mapEnergia :: (Int -> Int) -> Perro -> Perro 
mapEnergia funcionModificacion unPerro = unPerro { energia = funcionModificacion . energia $ unPerro }

--1)
jugar :: Ejercicio 
jugar unPerro = mapEnergia max 0 . substract 10 $ unPerro

--2) ladrar: aumenta la energía la mitad de los ladridos que se establezcan.
ladrar :: Int -> Ejercicio
ladrar cantidadDeLadridos unPerro = mapEnergia ((+) . div cantidadDeLadridos 2) unPerro

-- 3) regalar: ¡cómo no le vamos a dar un juguetito! Añade el juguete que se especifique a los favoritos. 
regalar :: String -> Ejercicio
regalar unJuguete unPerro = mapJuguetesFavoritos (agregarJuguete unJuguete) unPerro

agregarJuguete :: String -> [String] -> [String]
agregarJuguete unJuguete unosJuguetes = unJuguete : unosJuguetes

-- Usando (++)
agregarJuguete' :: String -> [String] -> [String]
agregarJuguete' unJuguete unosJuguetes = (++) [unJuguete] unosJuguetes

-- En ambas podemos jugar con el orden del otro lado del igual para agregar al final o al principio
diaDeSpa :: Ejercicio
diaDeSpa unPerro | tiempoDePermanencia unPerro >= 50 || esDeRazaExtravagante unPerro = (mapEnergia (const 100) . mapJuguetesFavoritos (agregarJuguete "peine de goma")) unPerro
                 | otherwise = unPerro

esDeRazaExtravagante :: Perro -> Bool
esDeRazaExtravagante unPerro = raza unPerro == "dalmata" || raza unPerro == "pomerania"

{-Lo mismo pero usando Pattern Matching (para hacer esto tengo que añadir otra funcion que directamente use la funcion de acceso para que la ultima reciba un String y un perro), ya 
solo recibiendo un String podemos usar Pattern Matching al ser un [Char] -}
esPerroExtravagante :: Perro -> Bool
esPerroExtravagante unPerro = (esDeRazaExtravagante' . raza) unPerro

esDeRazaExtravagante' :: String -> Bool
esDeRazaExtravagante' "dalmata" = True
esDeRazaExtravagante' "pomerania" = True
esDeRazaExtravagante' _ = False

-- 5) diaDeCampo: ¡nada más lindo que ver perritos jugar! Lástima que así siempre pierden el primer juguete.😅 
diaDeCampo :: Ejercicio
diaDeCampo unPerro = mapJuguetesFavoritos (drop 1) unPerro

-- Modelar a "Zara", una perra dalmata que tiene como juguetes favoritos una pelota y una mantita, permanece 1 hora y media en la guarderia y su energia es de 80
zara :: Perro
zara = Perro {
    raza = "dalmata",
    juguetesFavoritos = ["pelota", "mantita"],
    tiempoDePermanencia = 90,
    energia = 80
}

{- Modelar la "GuarderiaPdePerritos" que tiene como rutina las siguientes actividades:
   Ejercicio              Tiempo
    jugar                   30
   ladrar 18                20
   regalar "pelota"         0
   diaDeSpa                 120
   diaDeCampo               720               -}
-- Respuesta: 

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = Guarderia {
    nombre = "GuarderiaPdePerritos",
    rutina = [(jugar, 30), (ladrar 18, 20), (regalar "pelota", 0), (diaDeSpa, 120), (diaDeCampo, 720)]
}

-- B) 1) Saber si un perro puede estar en una guardería. Para eso, el tiempo de permanencia tiene que ser mayor que el de la rutina. 🕰️  
puedeEstar :: Perro -> Guarderia -> Bool
puedeEstar unPerro unaGuarderia = tiempoDePermanencia unPerro > tiempoTotalRutinaGuarderia unaGuarderia

tiempoTotalRutinaGuarderia :: Guarderia -> Int
tiempoTotalRutinaGuarderia unaGuarderia = sum (map duracionActividad (rutina unaGuarderia))

-- B) 2) Reconocer a perros responsables. Estos serian los que aun despues de pasar un diaDeCampo siguen teniendo mas de 3 juguetes
-- Respuesta: 
perroResponsable :: Perro -> Bool
perroResponsable unPerro = cantidadJuguetesDespuesDeDiaDeCampo unPerro > 3

cantidadJuguetesDespuesDeDiaDeCampo :: Perro -> Int
cantidadJuguetesDespuesDeDiaDeCampo unPerro = (length . juguetesFavoritos . diaDeCampo) unPerro

-- B) 3) Que un perro realice una rutina de la guardería (que realice todos sus ejercicios). Para eso, el tiempo de la rutina no puede ser mayor al tiempo de permanencia. En caso de
-- que esta condición no se cumpla, el perro no hace nada.
-- Respuesta: 

-- Esta pidiendo usar funciones anteriores donde se cumpla tenemos que aplicar el primer elemento de todas las tuplas de la lista de rutina a un perro
puedeRealizarRutina :: Guarderia -> Ejercicio
puedeRealizarRutina unaGuarderia unPerro | puedeEstar unPerro unaGuarderia = realizarRutina unaGuarderia unPerro
                                         | otherwise = unPerro

{- Entonces para aplicar toda una lista de ejercicios (funciones que reciben un perro y devuelven un perro con algun cambio simulado a un campo del perro) podemos usar "foldr $" (Aca
si no nos importa el orden, porque arrancaria del ultimo hacia el primero, si quisieramos otra cosa usariamos foldl $) que recibe $ (para que se aplique) luego la semilla "unPerro"
para que se aplique al ultimo ejercicio de la lista de ejercicios de una rutina de una guarderia (Aca la delegamos, ya que tenemos que quedarnos solo con el primer elemento de cada
tupla de la lista de tuplas, muy parecido a lo que hicimos en "tiempoTotalRutinaGuarderia" que nos quedamos con los segundos elementos) -} 
realizarRutina :: Guarderia -> Ejercicio
realizarRutina unaGuarderia unPerro = foldr ($) unPerro (listaEjercicios unaGuarderia)

{- Aca lo que delegamos para que foldr $ reciba la lista de ejercicios nomas, recibe una guarderia y luego con un map con la funcion "getter" del primer elemento de la tupla para que
la aplique a la lista de tuplas de una guarderia (que representa una rutina), y finalmente devuelve la lista pero solo con el primer elemento de las tuplas, osea de ejercicios   -}
listaEjercicios :: Guarderia -> [Ejercicio]
listaEjercicios unaGuarderia = (map ejercicio (rutina unaGuarderia))  









