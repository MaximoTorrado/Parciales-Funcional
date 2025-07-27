import GHC.IO.Handle (nativeNewlineMode)
-- 1) Obtener la energía que produce un grito, que se calcula como el nivel de terror por la intensidad al cuadrado, en caso de que moje la cama, si no, sólo es el triple del nivel de terror más la intensidad. El nivel de terror es equivalente al largo de la onomatopeya. 
type Grito = (String, Int, Bool)

onomatopeya :: (a, b, c) -> a
onomatopeya (o, _, _) = o

intensidad :: (a, b, c) -> b
intensidad (_,i,_) = i

mojoLaCama :: (a, b, c) -> c
mojoLaCama (_,_,m) = m
-- si quisieramos agregar un tipado

-- primer variante, recibiendo solo los miembros que considero con @
energiaDeGrito grito@(_, intensidad, mojoLaCama)
  | mojoLaCama = nivelTerror grito * intensidad ^ 2 
  | otherwise = (nivelTerror grito) * 3 + intensidad

-- segunda variante, recibiendo la tupla entera pero mediante los miembros (y usando where que crea parte una funcion de forma local, no es reutilizable)
energiaDeGrito' (onomatopeya, intensidad, mojoLaCama)
  | mojoLaCama = nivelTerror' * intensidad ^ 2
  | otherwise = nivelTerror' * 3 + intensidad
      where nivelTerror' = length onomatopeya

-- la contrapartida de esta solucion es que nos estamos acoplando mas fuertemente al formato de la tupla

-- se ve que lo que hace es nuevamente recibir de forma completa la tupla en grito, pero ahora usando "@" de ahi usa matcheo para tomar solo los miembros que quiera para alguna parte de la funcion, esto nos quita la necesidad de usar los accesors de cada miembro (tambien notar que "nivelTerror" trabaja con onomatopeya que en principio pareciera que no la esta recibiendo al usar @, pero ese llamado recibe "grito" como tupla entera, siempre las tuplas son pasadas de forma entera, dentro de las definiciones podemos usar matcheo)

nivelTerror grito = length (onomatopeya grito)
nivelTerror' grito = length . onomatopeya $ grito

nivelTerror'' grito = length . onomatopeya





