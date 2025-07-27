-- La reconocida empresa que transforma gritos de terror infantiles en energía limpia quiere organizar el trabajo de su monstruoso personal. 
-- 1) Obtener la energía que produce un grito, que se calcula como el nivel de terror por la intensidad al cuadrado, en caso de que moje la cama, si no, sólo es el triple del nivel de terror más la intensidad. El nivel de terror es equivalente al largo de la onomatopeya. 
-- De los gritos sabemos  gritos están representados por una tupla (Onomatopeya, Intensidad, mojóLaCama).
-- Ejemplo:
-- energiaDeGrito ("AAAAAHG", 10, True)
-- 700
-- energiaDeGrito ("uf", 2, False)
-- 8  

-- Este parcial nos plantea el modelo de dominio a medida que avanzamos con los puntos, en el primero nos muestra uno de estos elementos: el grito que es modelado como una tupla de 3 elementos (String, Int, Bool)

-- alternativamente para ganar en expresividad y tratar este tipo a mas alto nivel y en los terminos del dominio podriamos definir un type del grito (sinonimo)
type Grito = (String, Int, Bool)

-- finalmente, para abstraernos del orden de las componentes, definiremos funciones que accedan a los mismos, que mas adelante utilizaremos (osea los accesors "basicos", sin map para generar un cambio, si no solo para obtener los miembros de la tupla) 
onomatopeya (o, _, _) = o

intensidad (_,i,_) = i

mojoLaCama (_,_,m) = m

-- 1) Obtener la energía que produce un grito, que se calcula como el nivel de terror por la intensidad al cuadrado, en caso de que moje la cama, si no, sólo es el triple del nivel de terror más la intensidad. El nivel de terror es equivalente al largo de la onomatopeya. 
energiaDeGrito :: Grito -> Int
energiaDeGrito grito  
  | mojoLaCama grito = nivelTerror grito * (intensidad grito) ^ 2 
  | otherwise = (nivelTerror grito) * 3 + intensidad grito

nivelTerror grito = length (onomatopeya grito)
nivelTerror' grito = length . onomatopeya $ grito

nivelTerror'' grito = length . onomatopeya

-- Esta solucion presenta los siguientes elementos destacables: 1) Uso de guardas: el requerimiento del punto 1 nos plantea una funcion partida de dos ramas: si moja la cama y no moja la cama. 2) Delegacion del nivel de terror: el nivel de terror es un concepto del dominio, y por tanto deberia estar encapsulado en su propia funcion

-- recibe a la tupla de forma completa en "grito", en las tuplas usa la condicion booleana que ya viene contemplada en un miembro de la tupla que es si mojo la cama, dado el true conseguimos el "nivelTerror" de la tupla (que es la longitud del miembro onomatopeya o el primer miembro de la tupla) por la intensidad de la tupla (segundo miembro) al cuadrado, caso contrario otro calculo
-- lo que rescatamos es lo que dice como que "nivelTerror" es un concepto del dominio, no es una operacion mas, al tener titulo entonces es mejor delegarla 
