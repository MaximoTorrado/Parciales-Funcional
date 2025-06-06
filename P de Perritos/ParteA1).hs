{- Tenes un perrito y te toca ir a la oficina durante el dia? P de Perritos, la guarderia perruna, abrio sus puertas para que puedas dejar a tu fiel compañero en sus buenas manos
¡Ayudala a dar un excelente servicio con tus conocimientos del paradigma funcional!  -}

{- Hablemos de los protagonitas: los perritos. De cada uno conocemos: su raza, sus juguetes favoritos, el tiempo que va a permanencer en la guarderia, y la energia que tiene   
De las guarderias sabemos que tienen un nombre y una rutina para entretener a los pichichos. La rutina es un conjunto de actividades, compuestas por el ejercicio y el tiempo que 
este dura en minutos. Algunos de estos ejercicios son -}

-- data
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

-- 1) jugar: disminuye en 10 unidades la energía del perrito  ¡No puede quedar un valor negativo!
-- Respuesta: 

-- Primero que nada como notamos que los ejercicios buscan simular cambios en lso campos de un perro, armamos los "mapX" de cada campo 
mapRaza :: (String -> String) -> Perro -> Perro 
mapRaza funcionModificacion unPerro = unPerro { raza = funcionModificacion . raza $ unPerro }

mapJuguetesFavoritos :: ([String] -> [String]) -> Perro -> Perro 
mapJuguetesFavoritos funcionModificacion unPerro = unPerro { juguetesFavoritos = funcionModificacion . juguetesFavoritos $ unPerro }

mapTiempoDePermanencia :: (Int -> Int) -> Perro -> Perro 
mapTiempoDePermanencia funcionModificacion unPerro = unPerro { tiempoDePermanencia = funcionModificacion . tiempoDePermanencia $ unPerro }

mapEnergia :: (Int -> Int) -> Perro -> Perro 
mapEnergia funcionModificacion unPerro = unPerro { energia = funcionModificacion . energia $ unPerro }

-- Ahora si con la funcion, la funcion max recibe 2 parametros comparables y devuelve el mas alto, si podemos "max 0" le ponemos un limite a los numeros negativos
jugar :: Ejercicio 
jugar unPerro = mapEnergia max 0 . substract 10 $ unPerro

--2) ladrar: aumenta la energía la mitad de los ladridos que se establezcan.
-- Respuesta: 

{- Tomamos a los ladridos como algo que nos pasan por parametro, entonces dicho numero a la mitad sera lo que simularemos el cambio en el campo energia de un perro -}
ladrar :: Int -> Ejercicio
ladrar cantidadDeLadridos unPerro = mapEnergia ((+) . div cantidadDeLadridos 2) unPerro

-- 3) regalar: ¡cómo no le vamos a dar un juguetito! Añade el juguete que se especifique a los favoritos. 
-- Respuestas: 

-- Ya mirando los enunciados que le siguen vamoas a repetir logica al añadir juguetes, entonces de arranque la los delegamos 
regalar :: String -> Ejercicio
regalar unJuguete unPerro = mapJuguetesFavoritos (agregarJuguete unJuguete) unPerro

{- IMPORTANTE!!
Recordar que "mapJuguetesFavoritos" espera una funcion que va de [String] -> [String], entonces nosotros al momento de delegar debe respetar ese tipado
Entonces siempre que delegamos una funcion, pero que esta sea usada dentro de una composicion que recibe un "mapX" siempre el tipado que devuelva debe ser del mismo que espera
"funcionCriterio" -}

-- Usando :
agregarJuguete :: String -> [String] -> [String]
agregarJuguete unJuguete unosJuguetes = unJuguete : unosJuguetes

-- Usando (++)
agregarJuguete' :: String -> [String] -> [String]
agregarJuguete' unJuguete unosJuguetes = (++) [unJuguete] unosJuguetes

-- En ambas podemos jugar con el orden del otro lado del igual para agregar al final o al principio

{- 4) diaDeSpa: si el perro va a permanecer 50 minutos como mínimo en la guardería o es de raza extravagante, su energía pasa a ser 100 🔋 y se le regala el juguete "peine de goma”
 Si no, no pasa nada. Las razas extravagantes son dálmata y pomerania. -}
-- Respuesta:

{- IMPORTANTE!! Cuando tenemos que simular cambios a mas de un campo podemos usar en composicion las funciones "mapX"
Notamos que aca si se cumple una condicion tenemos que simular cambios en el campo energia y juguetesFavoritos de un mismo perro, entonces lo que podemos hacer es componer ambos
"mapX" ya que la logica de cada una es simular el cambio y finalmente devolver un perro, el cual puede recibir el otro map y simular ahora el cambio para otro campo del mismo perro  -}
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
-- Respuesta: 

-- Vamos a generar un cambio a el campo de juguetes de un perro, usamos su "mapX" y le pasamos "drop 1" que saca n cantidad de elementos de una lista (y contempla si es la vacia)
diaDeCampo :: Ejercicio
diaDeCampo unPerro = mapJuguetesFavoritos (drop 1) unPerro

-- Modelar a "Zara", una perra dalmata que tiene como juguetes favoritos una pelota y una mantita, permanece 1 hora y media en la guarderia y su energia es de 80
-- Respuesta: 

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

