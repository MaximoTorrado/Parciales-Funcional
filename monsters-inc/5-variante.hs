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

-- una forma de pensar esto es "la energia que los monstruos obtienen de un campamento es la sumatoria de las "energiaDeGrito" de los gritos obtenidos al asustarlo"

-- variante 1: usando concat y sum
produccionEnergeticaGritos :: [Niño -> Grito] -> [Niño] -> Int
produccionEnergeticaGritos monstruos niños = (sumatoriaEnergia . asustarCampamento monstruos) niños

asustarCampamento :: [Niño -> Grito] -> [Niño] -> [Grito]
asustarCampamento monstruos niños = (concat . map (\x -> map ($ x) monstruos)) niños

sumatoriaEnergia :: [Grito] -> Int
sumatoriaEnergia gritos = (sum . map energiaDeGrito) gritos


-- tenemos "monstruos" que son [Niño -> Grito] y "niños" que son [Niño], y queremos devolver un entero que represente aplicar "energiaDeGrito" a [Grito] que devolveria aplicar cada elemento de [Niño] a cada elemento de [Niño -> Grito], entonces
-- 1) con "asustarCampamento" recibimos [Niño -> Grito] y [Niño] entonces, si queremos aplicar cada elemento de [Niño] a [Niño -> Grito] para obtener un [Grito], uso un map que recibe una funcion incognita y [Niño], la incognita va recibiendo cada elemento de [Niño] en "x", que es un Niño, y queremos aplicarlo a [Niño -> Grito], ahora no podemos mandar "monstruos x" porque "monstruos" es una lista de funciones [Niño -> Grito] y "x" un valor solo, entonces tambien secuencialmente tenemos que aplicar "x" a cada elemento de [Niño -> Grito], lo hacemos con otro map que podria usar otra funcion incognita pero hacemos que reciba "map ($ x) monstruos" que significa aplica este valor "x" a cada elemento de [Niño -> Grito]. Como vimos anteriormente esto nos da [[Grito]] entonces tenemos que aplanarlo, usamos "concat" que ya existe y labura esto perfecto [[x]] = [x]
-- {la anterior es otra forma de aplicar lo que hicimos en la variante anterior, aca pensando en los elementos de "niño" y en la anterior pensando en los elementos de "monstruo"}
-- 2) teniendo la [Grito] que necesitabamos ahora solo queremos aplicar a cada elemento de [Grito] la funcion "energiaDeGrito" y sumarlo, entonces hacemos eso donde primero mapeamos con "energiaDeGrito" a la [Grito] obteniendo una lista de enteros que podemos sumar finalmente con "sum"

