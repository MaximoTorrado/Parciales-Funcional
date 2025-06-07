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
-- Respuesta:

{-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
INTENTO: 

quedanCansados :: [Perro] -> Guarderia -> [Perro]
quedanCansados unosPerros unaGuarderia = filter quedaCansado unosPerros

quedaCansado :: Perro -> Guarderia -> Bool
quedaCansado unPerro unaGuarderia = any cansadito (map (\ejercicio -> ejercicio unPerro) (listaEjercicios unaGuarderia))

cansadito :: Perro -> Bool
cansadito unPerro = energia unPerro < 5 
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------}

{- Basicamente lo que hay que hacer es a una lista de perros aplicarle la lista de ejercicios, osea aplicar una lista de funciones a una lista de parametros, y finalmente luego de
haber aplicado las funciones tenemos que filtrar con aquellos que tengan el campo energia menor a 5, con esto ya nos damos una idea que usaremos "filter"

"filter" necesita una funcio criterio INDIVIDUAL que sea CAPAZ de APLICAR TODA LA LISTA DE FUNCIONES a UN PERRO y ADEMAS pueda MARCAR AQUELLOS que ESTAN CANSADOS O NO

IMPORTANTE!! Cuando hay una "Condicion extra" que hacer aplicar toda una lista de funciones a un parametro 
ya el hecho que tengamos que hacer aplicar toda una lista de funciones a un parametro nos llama a usar "funcion incognita" pero ADEMAS como tiene OTRA CONDICION ADEMAS de HACER 
APLICAR la lista de funciones al parametro (ver su campo energia para ver si esta cansado) SIGNIFICA QUE DENTRO DEL CUERPO DE LA FUNCION INCOGNITA TENDREMOS OTRA FUNCION QUE RESPONDA
A LA CONDICION "EXTRA" 

Entonces a lo que "filter" termina recibiendo una funcion criterio INDIVIDUAL que sera 
1) funcion incognita (\x -> ...) x´s que hace que se aplique una lista de funciones a un solo parametro de tipo "x´s" 
2) "quedaCansado" que hace que se aplique toda una lista de funciones a un solo parametro de tipo "x´s" Y ADEMAS se fije si esta cansado (Booleano)
Luego la logica del filter pedira que 1y2) anidado sea booleano, lo traduce a una lista de parametros de tipo "x´s" y devuelve otra lista con el criterio aplicado

Este ultimo nos da una idea que 1y2) tendra que ser una funcion composicion que permita el aplicar toda la lista de funciones y devuelva un bool si esta cansado o no   -}

quedanCansados :: [Perro] -> Guarderia -> [Perro]
quedanCansados unosPerros unaGuarderia = filter (\unPerro -> quedaCansado unPerro unaGuarderia) unosPerros

{- Esta es la funcion criterio individual que delegamos de arriba, que como debe ser algo que aplica mas de una cosa tendra que ser una composicion, recibe un perro y una guarderia
y devuelve un booleano, hacemos que se aplique la lista de funciones con "puedeRealizarRutina" (Que ademas se fija si puede o no, la cual hicimos que vaya aplicando la lista a un
solo parametro usando foldr $, osea devuelve un perro con todas las funciones aplicadas)  que luego recibe "cansadito" y se fija si dicho perro tiene la energia menor a 5

Finalmente como esto lo estamos pensando de manera individual luego la recibe arriba el filter y la traduce para una lista de perros, y termina devolviendo otra lista con los perros
que cumplieron estar cansados    -}
quedaCansado :: Perro -> Guarderia -> Bool
quedaCansado unPerro unaGuarderia = (cansadito . puedeRealizarRutina unaGuarderia) unPerro

cansadito :: Perro -> Bool
cansadito unPerro = energia unPerro < 5







