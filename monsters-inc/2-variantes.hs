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

type Niño = (String, Int, Double)

nombre (n, _, _) = n
edad (_, e, _) = e
altura(_, _, a) = a

-- variante 1: usando repeat en sullivan
-- repeat es una funcion definida en el Prelude, que simplemente devuelve una lista infinita de lo que sea que recibe como parametro, en este caso ['A',... entonces, podemos generarle el tope usando "take n" donde nuestro n sea la longitud del nombre del niño (accesor "basico" segundo miembro de la tupla niño), finalmente concatenamos con "GH"
sullivan :: Niño -> Grito
sullivan niño = (gritoGenerado, intensidadGenerada, haceMojarLaCama)
  where gritoGenerado      = take ((length . nombre) niño) (repeat 'A') ++ "GH"
        intensidadGenerada = div 20 (edad niño)
        haceMojarLaCama    = edad niño < 3

-- misma variante usando composicion (el tope sigue poniendoselo el take)
sullivan' :: Niño -> Grito
sullivan' niño = (gritoGenerado, intensidadGenerada, haceMojarLaCama)
  where gritoGenerado      = (take ((length . nombre) niño) . repeat) 'A' ++ "GH"
        intensidadGenerada = div 20 (edad niño)
        haceMojarLaCama    = edad niño < 3

-- donde el take esta aplicado parcialmente con la longitud del nombre del niño, y el repeat ira creando la lista infinita hasta el tope que le de el take (lo innovador es que la composicion la arrancamos de un char)

-- variante 2: usando una lista definida a partir de un rango infinito en sullivan
-- creando la lista potencialmente infinita en el rango 'A' a 'A'

sullivan'' :: Niño -> Grito
sullivan'' niño = (gritoGenerado, intensidadGenerada, haceMojarLaCama)
  where gritoGenerado = take ((length.nombre) niño) ['A', 'A'.. ] ++ "GH"  
        intensidadGenerada = div 20 (edad niño)
        haceMojarLaCama    = edad niño < 3

-- otra forma de hacer la lista infinita planteando los limites

-- variante 4: empleando un rango infinito en la lista de chuck
-- dado que el tipo Char es enumerable, osea pertenece a la familia "Enum" podemos emplearlo en una lista por comprension de la siguiente manera
chuck :: Niño -> Grito
chuck niño = (['a'..'z'], 1000, True)


















