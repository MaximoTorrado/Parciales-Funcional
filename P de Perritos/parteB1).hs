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


{- Parte B: Y entonces… ¿cuándo abreeeeeee? ✋Primero debemos asegurarnos de:
Saber si un perro puede estar en una guardería. Para eso, el tiempo de permanencia tiene que ser mayor que el de la rutina. 🕰️  -}
-- Respuesta: 

{- Recibimos un perro, una guarderia, y devolvemos un bool, de la guarderia tenemos que tener una funcion que desglose el segundo miembro de la lista de tuplas que tiene para 
conseguir el tiempo final de la rutina entera, luego comparar con el tiempo de permanencia del perro -}
puedeEstar :: Perro -> Guarderia -> Bool
puedeEstar unPerro unaGuarderia = tiempoDePermanencia unPerro > tiempoTotalRutinaGuarderia unaGuarderia


{- IMPORTANTE!!
Aca tengo que tener una funcion que recibe una guarderia, de esa guarderia me quiero enfocar en su campo "rutina" que es una lista de tuplas, y en especifico quedarme unicamente con
el segundo campo de cada tupla, para eso puedo usar map con una funcion criterio individual que es capaz de quedarse unicamente con esa parte de una tupla individual, que esa funcion
ya la tengo y es el "getter" segundo "duracionActividad", entonces al map le paso esa funcion y la lista que nace del campo rutina de la guarderia que le pasamos para que map termine
aplicando esa funcion individual a cada elemento de la lista y devolviendo otra lista con la funcion aplicada a todos los elementos, quedandonos entonces otra lista con unicamente
los segundos elementos de la lista de tuplas, finalmente usamos sum que recibe una lista y suma todos los elementos, entonces sumamos todos los enteros de cada tiempo de cada una   -}
tiempoTotalRutinaGuarderia :: Guarderia -> Int
tiempoTotalRutinaGuarderia unaGuarderia = sum (map duracionActividad (rutina unaGuarderia))











