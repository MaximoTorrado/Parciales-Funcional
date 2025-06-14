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
puedeRealizarRutina :: Guarderia -> Ejercicio
puedeRealizarRutina unaGuarderia unPerro | puedeEstar unPerro unaGuarderia = realizarRutina unaGuarderia unPerro
                                         | otherwise = unPerro

realizarRutina :: Guarderia -> Ejercicio
realizarRutina unaGuarderia unPerro = foldr ($) unPerro (listaEjercicios unaGuarderia)

listaEjercicios :: Guarderia -> [Ejercicio]
listaEjercicios unaGuarderia = (map ejercicio (rutina unaGuarderia))  

{- B) 4) Dados unos perros, reportar todos los que quedan cansados después de realizar la rutina de una guardería. Es decir, que su energía sea menor a 5 luego de realizar todos los 
ejercicios. -}

quedanCansados :: [Perro] -> Guarderia -> [Perro]
quedanCansados unosPerros unaGuarderia = filter (\unPerro -> quedaCansado unPerro unaGuarderia) unosPerros

quedaCansado :: Perro -> Guarderia -> Bool
quedaCansado unPerro unaGuarderia = (cansadito . puedeRealizarRutina unaGuarderia) unPerro

cansadito :: Perro -> Bool
cansadito unPerro = energia unPerro < 5

{- ¡Infinita diversión! ♾️ Pi es un perrito un poco especial… Su raza es labrador y tiene muchos, muchos, incontables juguetes favoritos. Con la particularidad de que son todas 
soguitas numeradas del 1 al infinito. Su tiempo de permanencia es de 314 minutos y su energía es de 159.
Luego de modelar a Pi, respondé las siguientes preguntas justificando y escribiendo la consulta que harías en la consola: -}
-- Respuesta: 

-- Modelamos primero a Pi
pi :: Perro 
pi = Perro {
    raza = "Labrador",
    juguetesFavoritos = sogasInfinitas,
    tiempoDePermanencia = 314,
    energia = 159
}

-- Para crear una lista de sogas infinitas (osea una lista infinita de String que cada una sera ["Soga 1", "Soga 2", ...] usamos la siguiente sintaxis)
sogasInfinitas :: [String]

{- la funcion incognita aca recibe un n (que es de tipo elemento de una lista infinita, porque le estamos pasando una lista infinita que arranca del 1) y termina concatenando 
el String "Soguita" con dicho n, luego esta funcion que hasta ahora es individual, el map lo termina haciendo para toda la lista infinita, entonces va concatenando cada n (que es
de tipo elemento de una lista) con "Soguita" y asi "infinitamente" (no tanto porque lazy evaluation lo para)  -}
sogasInfinitas = map (\n -> "Soguita " ++ show n) [1 ..]

-- 1) ¿Sería posible saber si Pi es de una raza extravagante? 
-- Respuesta:

{- Si podra saber si es o no de raza extravagante, ya que como lo unico que necesita esta funcion es mirar el campo raza de un perro, entonces nunca se evalua la lista infinita ya
que nunca entramos al campo que inicia dicha lista (gracias a lazy evaluation) -}

-- 2) ¿Qué pasa si queremos saber si Pi tiene a) algún huesito como juguete favorito b) alguna pelota luego de pasar por la Guardería de Perritos c) la soguita 31112  -}
-- Respuesta:

{- a) No, el tema es que aca si estamos entrando al campo "juguetesFavoritos" que es una lista infinita, y no le estamos poniendo ningun tope con alguna otra funcion como por ejemplo
que tome n cantidad de esa lista infinita y se fije si hay un string "huesito", ademas como sabemos que solo tiene "Soguita 1", "Soguita 2",... iria infinitamente buscando y nunca
cortaria (Una forma de zafarla seria poniendole un limite con un take n y que de la lista infinita se fije n elementos)

b) No, porque nosotros la funcion que usamos para agregar ciertos juguetes lo hicimos para que agregue al principio, entonces luego de un dia de campo perderia ese primer juguete
que justamente es el que estamos buscando e iria infinitamente buscando eso entre las soguitas, entonces no (Y dado el caso que agregaramos al final creo que tampoco podriamos ya
que la lista infinita no tiene final entonces no podemos agregar nada al final)

c) Aca si podriamos usando "Soguita 31112 `elem` (juguetesFavoritos pi)" ya que aca esta busqueda puede evaluar y terminar llegando a un resultado, eventualmente encontrara dicho
String en la lista, y no seguira evaluando luego de terminar  -}

{- 3) ¿Es posible que Pi realice una rutina? -}
-- Respuesta

{- Habiamos planteando anteriormente que un perro puede realizar una rutina en caso que su tiempo de permanencia sea menor que el tiempo total de la rutina, al menos para esta
guarderia P de Perritos, que tiene tiempo total 890, tiene menos tiempo de permanencia entonces no podemos hacer que haga esta rutina de esta guarderia, otra rutina de otra guarderia
con quizas otros tiempos es posible  -}


{- 4) ¿Qué pasa si le regalamos un hueso a Pi? -}
-- Respuesta:

{- Solo sera posible si agregamos al principio ya que gracias a evaluacion lazy podremos ingresar ese elemento y luego no deberemos procesar los demas porque la funcion de agregar
solo hace eso y no mira los demas elementos, en caso de que estemos agregando al final no podremos porque al ser infinita no tiene un tope -}











