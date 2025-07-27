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

-- primera alternativa a) sin usar funciones locales y sin partir en funciones globales
sullivan' :: Niño -> Grito
sullivan' niño = (map (\ _ -> 'A') (nombre niño) ++ "GH", div 20 (edad niño), edad niño < 3)
-- siendo que recibimos una tupla "Niño" y queremos devolver otra tupla "Grito" modelamos eso, 
-- 1) para el primer miembro del grito, si queremos una 'A' por cada letra que tiene el nombre del niño entonces usamos un map que recibe una incognita y una lista, la lista sera el String del primer miembro de la tupla del niño (que obtenemos con su accesor "basico" "nombre") luego la incognita va recibiendo los elementos de la lista [Char], pero como no nos interesa las obviamos y por cada elemento devolvemos una 'A', una vez llegue al final de la lista anidamos con un "GH", 2) para el segundo miembro dividimos el segundo miembro del niño con el accesor "edad" por 20, y 3) miramos si dicha edad es menor a 3 dandonos el Bool que modela si se meo o no

-- segunda alternativa partiendo en funciones (sin hacerlas de forma local, las cuales podemos reutilizar dado el caso)
sullivan'' :: Niño -> Grito
sullivan'' niño = (gritoGenerado' niño, intensidadGenerada' niño, haceMojarLaCama' niño)

gritoGenerado' :: Niño -> String
gritoGenerado' niño = map (\_ -> 'A') (nombre niño) ++ "GH"

intensidadGenerada' :: Niño -> Int
intensidadGenerada' niño = div 20 (edad niño)

haceMojarLaCama' :: Niño -> Bool
haceMojarLaCama' niño = edad niño < 3
-- a la tupla que queremos devolver como vamos a operar con algun miembro de la tupla de un niño, se la pasamos a las funciones globales y las definimos mas abajo con las mismas cuentas

-- alternativa ofrecida en resolucion, usando funciones de forma local con "where" de forma que usemos solo estas funciones aca (porque no las reutilizaremos mas adelante se ve), donde solo llamamos a las funciones sin pasarle nada, y dentro de sus definiciones operamos a conveniencia con los miembros que querramos del niño
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

-- estas si las definimos de forma global, probablemente las utilicemos en otras funciones
esVocal :: Char -> Bool
esVocal caracter = flip elem "aeiouAEIOU" caracter
esVocal' :: Char -> Bool
esVocal' 'a' = True
-- ...
esVocal' 'U' = True
esVocal' _ = False

-- Aca hicimos lo mismo usando la forma que nos sugiere la resolucion usando forma local con "where" (lo cual si miramos tiene un tanto de sentido porque tienen los mismos nombres que son sullivan pero son distintas operaciones ambas, hacerlas de forma local mantiene esa estructura y cambia la operacion nomas)
-- recibimos una tupla "niño" y queremos devolver una tupla Grito, entonces 1) sabemos de forma constante que devuelve "¡Mamadera!" entonces primer miembro ahi constante, 
-- 2) a intensidadGenerada en este scope local queremos el nombre del niño, para luego filtrar dicho String con las vocales (esta si la definimos de forma global quizas la reutilicemos) la cual recibe caracteres y devuelve booleano, con el String que es [Char] nos quedamos con la cantidad de dicho String y 3) ponemos esos limites para la altura del niño que nos dara el Booleano que representa si se mea o no

-- Chuck Norris produce siempre un grito que dice todo el abecedario, con 1000 de nivel de intensidad y siempre hace que mojen la cama.
chuck :: Niño -> Grito
chuck niño = ("abcdefghijklmnopqrstuvwxyz", 1000, True)

-- Un Osito Cariñoso produce un grito “uf”, con un nivel de intensidad igual a la edad del niño y nunca moja la cama.
osito :: Niño -> Grito
osito niño = ("uf", edad niño, False)



