type Grito = (String, Int, Bool)

onomatopeya :: (a, b, c) -> a
onomatopeya (o, _, _) = o

intensidad :: (a, b, c) -> b
intensidad (_,i,_) = i

mojoLaCama :: (a, b, c) -> c
mojoLaCama (_,_,m) = m

-- 1) Obtener la energía que produce un grito, que se calcula como el nivel de terror por la intensidad al cuadrado, en caso de que moje la cama, si no, sólo es el triple del nivel de terror más la intensidad. El nivel de terror es equivalente al largo de la onomatopeya. 
energiaDeGrito :: Grito -> Int
energiaDeGrito grito  
  | mojoLaCama grito = nivelTerror grito * (intensidad grito) ^ 2 
  | otherwise = (nivelTerror grito) * 3 + intensidad grito

nivelTerror grito = length (onomatopeya grito)
nivelTerror' grito = length . onomatopeya $ grito

nivelTerror'' grito = length . onomatopeya

-- 2) Obtener el grito que cada monstruo (un empleado de la empresa) le arranca a un niño (su víctima). Hay muchos monstruos y cada uno tiene su forma de trabajar, que consiste precisamente en recibir a un niño y devolver su grito. De los niños se sabe el nombre, la edad y la altura y se los representa con una tupla con dichas componentes. 

-- Lo que se pide es a partir de un niño obtener su grito correspondiente segun el monstruo que lo asuste, la idea es modelar los monstruos como funciones que reciben un Niño (que ahora modelaremos) y devuelven un Grito

-- modelamos los niños segun lo que sabemos 
type Niño = (String, Int, Double)

-- hacemos algunos accesors "basicos" (los que solo devuelven los miembros de la tupla, no los accesors con map para pasarle una funcion y cambiar dicho miembro)
nombre (n, _, _) = n
edad (_, e, _) = e
altura(_, _, a) = a

-- vamos ahora con los monstruos, recordando lo que decia el enunciado para cada uno 
-- Sullivan (el protagonista) produce un grito "AAAGH" con tantas A como letras tenga el nombre del niño y una intensidad como 20 / edad; hace mojar la cama si el niño tiene menos de 3 años.

sullivan :: Niño -> Grito
sullivan niño = (gritoGenerado, intensidadGenerada, haceMojarLaCama)
  where gritoGenerado      = map (\ _ -> 'A') (nombre niño) ++ "GH"
        intensidadGenerada = div 20 (edad niño)
        haceMojarLaCama    = edad niño < 3

-- Randall Boggs (la lagartija) produce un grito de “¡Mamadera!”, con tanta intensidad como vocales en el nombre de la persona; hace mojar la cama en los niños que miden entre 0,8 y 1,2 de altura.
randall :: Niño -> Grito 
randall niño = ("Mamadera", intensidadGenerada, haceMojarLaCama)
  where intensidadGenerada = length . filter esVocal . nombre $ niño 
        haceMojarLaCama    = altura niño >= 0.8 && altura niño <= 1.2 

esVocal :: Char -> Bool
esVocal caracter = flip elem "aeiouAEIOU" caracter
esVocal' :: Char -> Bool
esVocal' 'a' = True
-- ...
esVocal' 'U' = True
esVocal' _ = False

-- Chuck Norris produce siempre un grito que dice todo el abecedario, con 1000 de nivel de intensidad y siempre hace que mojen la cama.
chuck :: Niño -> Grito
chuck niño = ("abcdefghijklmnopqrstuvwxyz", 1000, True)

-- Un Osito Cariñoso produce un grito “uf”, con un nivel de intensidad igual a la edad del niño y nunca moja la cama.
osito :: Niño -> Grito
osito niño = ("uf", edad niño, False)

{- 3) Hacer una función que reciba una lista de funciones y un elemento, y que devuelva la lista que resulta de aplicar cada función sobre el elemento. Definir dominio e imagen.
Por ejemplo:
pam [doble, triple, raizCuadrada] 9
[18, 27, 3]                                                   -}

-- primer variante map e incognita
pam :: [a -> b] -> a -> [b]
pam funciones valor = map (\x -> x valor) funciones 

-- segunda variante con $
pam' :: [a -> b] -> a -> [b]
pam' funciones valor = map ($ valor) funciones

{- 4) Los monstruos a veces trabajan en equipo, por lo que van varios a la casa de un niño y todos lo asustan. Obtener el conjunto de gritos que logra el equipo.
Por ejemplo:
gritos [sullivan, osito, chuck] (“kevin”, 2, 1.1)
[("AAAAAHG", 10, True), (“uf”,2,False), (“abcdefghijklmnopqrstuvwxyz”,1000,True)]  -}

gritos :: [Niño -> Grito] -> Niño -> [Grito]
gritos monstruos niño = pam monstruos niño 

{- 5) Saber la producción energética de un equipo de monstruos que va a un campamento y asusta a todos los niños que están durmiendo en las carpas. Asumir que los campamentos ya están definidos y son funciones constantes que devuelven una lista con niños.
Por ejemplo:
produccionEnergeticaGritos [sullivan, osito, chuck] campamentoDeExploradores
999999                                                                           -}

produccionEnergeticaGritos :: [Niño -> Grito] -> [Niño] -> Int
produccionEnergeticaGritos monstruos niños = (sumatoriaEnergia . asustarCampamento monstruos) niños

asustarCampamento :: [Niño -> Grito] -> [Niño] -> [Grito]
asustarCampamento monstruos niños = (aplanar . map (\x -> map x niños)) monstruos

sumatoriaEnergia :: [Grito] -> Int
sumatoriaEnergia gritos = foldl (\acu x -> acu + energiaDeGrito x) 0 gritos

aplanar :: [[a]] -> [a]
aplanar listaDeListas = foldl (++) [] listaDeListas

{- 6) Ante la declinación en la producción energética de Monsters inc debido a las nuevas generaciones que ya no se asustan de monstruos obsoletos, la empresa ha decidido poner en marcha un proyecto de transformación de risas en energía, para lo que contrató a comediantes.
Las risas se representan por tuplas en las que se indica la duración y la intensidad. La energía que producen es la duración elevada a la intensidad. Cada comediante recibe un niño y devuelve una risa.
Por ejemplo:
Capusotto produce una risa de duración igual al doble de la edad del niño y la misma intensidad. -}

type Risa = (Int, Int)

energiaRisa :: Risa -> Int
energiaRisa (duracion, intensidad) = duracion ^ intensidad

-- Hacer la función produccionEnergeticaRisas que devuelve el total de energía producido por enviar a un equipo de comediantes a un campamento.  Si es necesario, hacer funciones auxiliares pero no modificar las ya hechas.  
produccionEnergeticaRisas :: [Niño -> Risa] -> [Niño] -> Int
produccionEnergeticaRisas comediantes niños = (sumatoriaEnergiaRisas . hacerReirCampamento comediantes) niños

-- planteamos las 2 variantes
hacerReirCampamento :: [Niño -> Risa] -> [Niño] -> [Risa]
hacerReirCampamento comediantes niños = (concat . map (\x -> pam comediantes x)) niños

hacerReirCampamento' :: [Niño -> Risa] -> [Niño] -> [Risa]
hacerReirCampamento' comediantes niños = (concat . map (\x -> map ($ x) comediantes)) niños

sumatoriaEnergiaRisas :: [Risa] -> Int
sumatoriaEnergiaRisas risas = (sum . map energiaRisa) risas



-- lo que queremos es a partir de unos "comediantes" [Niño -> Risa] y unos "niños" [Niño] queremos el total de energia luego de hacerlos reir, osea queremos primero hacer reir a todos los niños para conseguir [Risa] = [(duracion, intensidad),...,(duracion, intensidad)], y le queremos aplicar "energiaRisa" a cada elemento de [Risa] (que es la potencia entre ambos miembros) para obtener [1,2,23] que simula la risa de cada niño y finalmente sumar todo para obtener el total
-- 1) para aplicar cada elemento de [Niño] a [Niño -> Risa] (que si pensamos nos dara [[Risa]] ) usamos "hacerReirCampamento" que recibe ambos, como en esta funcion mi mision es obtener [Risa] pienso en usar map con una funcion incognita y [Niño] entonces, la incognita va recibiendo los elementos de [Niño] en "x" que es un Niño, pero un solo valor no podemos aplicarlo a [Niño -> Risa] sin alguna funcion secuencial, entonces adentro podemos usar otro 1)  "map ($ x) comediantes" que significa aplica este valor a todos los elementos de [Niño -> Risa] o 2) llamar a la misma pam que se encarga de hacer lo mismo cambiando de orden de los parametros. 
-- notamos que aplicar [Niño] a [Niño -> Risa] nos dara [[Risa]], entonces tenemos que aplanarlo, usamos "concat" que se encarga de [[x]] = [x]
-- 2) una vez obtenida [Risa] ahora solo queremos aplicar a cada elemento de [Risa] "energiaRisa", tiene sentido ya que al ser una lista de tuplas podemos ir por cada tupla haciendo la operacion de la misma, para hacer esto de forma secuencial usamos un mapeado con "energiaRisa", luego tendriamos algo asi [1,34,2,5] entonces finalmente lo sumamos con "sum"






















