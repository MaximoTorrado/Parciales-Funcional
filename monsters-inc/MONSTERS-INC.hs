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

-- primer variante, recibiendo solo los miembros que considero con @
energiaDeGrito' grito@(_, intensidad, mojoLaCama)
  | mojoLaCama = nivelTerror grito * intensidad ^ 2 
  | otherwise = (nivelTerror grito) * 3 + intensidad

-- se ve que lo que hace es nuevamente recibir de forma completa la tupla en grito, pero ahora usando "@" de ahi usa matcheo para tomar solo los miembros que quiera para alguna parte de la funcion, esto nos quita la necesidad de usar los accesors de cada miembro (tambien notar que "nivelTerror" trabaja con onomatopeya que en principio pareciera que no la esta recibiendo al usar @, pero ese llamado recibe "grito" como tupla entera, siempre las tuplas son pasadas de forma entera, dentro de las definiciones podemos usar matcheo)

-- segunda variante, recibiendo la tupla entera pero mediante los miembros (y usando where que crea parte una funcion de forma local, no es reutilizable)
energiaDeGrito'' (onomatopeya, intensidad, mojoLaCama)
  | mojoLaCama = nivelTerror' * intensidad ^ 2
  | otherwise = nivelTerror' * 3 + intensidad
      where nivelTerror' = length onomatopeya

-- ERROR COMUN 1: Uso redundante de los valores booleanos (para las condiciones)
{- energiaDeGrito grito 
      | (mojoLaCama grito == True) = nivelTerror grito * (intensidad grito) ^ 2
      | otherwise = (nivelTerror grito) * 3 + intensidad grito -}


{- ERROR COMUN 2: No delegar apropiadamente
   energiaDeGrito grito
       |mojoLaCama grito = (length.onomatopeya) grito * (intensidad grito) ^ 2
       |otherwise    = 3 * (length.onomatopeya) grito + intensidad grito -}

-- ERROR COMUN 3: La solucion funcion mas como vimos anteriormente conceptualmente es incorrecta porque oculpa el "concepto propio del dominio" del enunciado que es "nivelTerror" osea al tener titulo tenemos que partirlo y definir

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
-- siendo que recibimos una tupla "Niño" y queremos devolver otra tupla "Grito" modelamos eso
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

-- variante 1: usando repeat en sullivan
-- repeat es una funcion definida en el Prelude, que simplemente devuelve una lista infinita de lo que sea que recibe como parametro, en este caso ['A',... entonces, podemos generarle el tope usando "take n" donde nuestro n sea la longitud del nombre del niño (accesor "basico" segundo miembro de la tupla niño), finalmente concatenamos con "GH"
sullivan''' :: Niño -> Grito
sullivan''' niño = (gritoGenerado, intensidadGenerada, haceMojarLaCama)
  where gritoGenerado      = take ((length . nombre) niño) (repeat 'A') ++ "GH"
        intensidadGenerada = div 20 (edad niño)
        haceMojarLaCama    = edad niño < 3

-- misma variante usando composicion (el tope sigue poniendoselo el take)
sullivan'''' :: Niño -> Grito
sullivan'''' niño = (gritoGenerado, intensidadGenerada, haceMojarLaCama)
  where gritoGenerado      = (take ((length . nombre) niño) . repeat) 'A' ++ "GH"
        intensidadGenerada = div 20 (edad niño)
        haceMojarLaCama    = edad niño < 3

-- donde el take esta aplicado parcialmente con la longitud del nombre del niño, y el repeat ira creando la lista infinita hasta el tope que le de el take (lo innovador es que la composicion la arrancamos de un char)

-- variante 2: usando una lista definida a partir de un rango infinito en sullivan
-- creando la lista potencialmente infinita en el rango 'A' a 'A'

sullivan''''' :: Niño -> Grito
sullivan''''' niño = (gritoGenerado, intensidadGenerada, haceMojarLaCama)
  where gritoGenerado = take ((length.nombre) niño) ['A', 'A'.. ] ++ "GH"  
        intensidadGenerada = div 20 (edad niño)
        haceMojarLaCama    = edad niño < 3

-- otra forma de hacer la lista infinita planteando los limites

-- variante 4: empleando un rango infinito en la lista de chuck
-- dado que el tipo Char es enumerable, osea pertenece a la familia "Enum" podemos emplearlo en una lista por comprension de la siguiente manera
chuck' :: Niño -> Grito
chuck' niño = (['a'..'z'], 1000, True)

{- 3) Hacer una función que reciba una lista de funciones y un elemento, y que devuelva la lista que resulta de aplicar cada función sobre el elemento. Definir dominio e imagen.
Por ejemplo:
pam [doble, triple, raizCuadrada] 9
[18, 27, 3]                                                   -}

-- primer variante map e incognita
pam :: [a -> b] -> a -> [b]
pam funciones valor = map (\x -> x valor) funciones 

-- como recibimos una lista de funciones y queremos que cada elemento se aplique al valor que recibimos (y ademas necesitamos que los resultados esten en una lista) la funcion secuencial unaria que usamos es map, que agarra los elementos de "funciones" con "x" y los aplica con el valor que le dimos 

-- segunda variante con $
pam' :: [a -> b] -> a -> [b]
pam' funciones valor = map ($ valor) funciones

{- 4) Los monstruos a veces trabajan en equipo, por lo que van varios a la casa de un niño y todos lo asustan. Obtener el conjunto de gritos que logra el equipo.
Por ejemplo:
gritos [sullivan, osito, chuck] (“kevin”, 2, 1.1)
[("AAAAAHG", 10, True), (“uf”,2,False), (“abcdefghijklmnopqrstuvwxyz”,1000,True)]  -}

gritos :: [Niño -> Grito] -> Niño -> [Grito]
gritos monstruos niño = pam monstruos niño 

-- vemos que gritos recibe una lista de funciones que reciben un niño y devuelven un grito, y un niño, entonces queremos todos los elementos de la lista sean aplicados en el ese niño, entonces es lo mismo que definimos en la funcion anterior (porque ademas pide devolver una lista)
-- el error comun seria volver a definir una funcion que haga eso sin llamar a "pam" que definimos anteriormente

{- 5) Saber la producción energética de un equipo de monstruos que va a un campamento y asusta a todos los niños que están durmiendo en las carpas. Asumir que los campamentos ya están definidos y son funciones constantes que devuelven una lista con niños.
Por ejemplo:
produccionEnergeticaGritos [sullivan, osito, chuck] campamentoDeExploradores
999999                                                                           -}

-- una forma de pensar esto es "la energia que los monstruos obtienen de un campamento es la sumatoria de las "energiaDeGrito" de los gritos obtenidos al asustarlo"
produccionEnergeticaGritos :: [Niño -> Grito] -> [Niño] -> Int
produccionEnergeticaGritos monstruos niños = (sumatoriaEnergia . asustarCampamento monstruos) niños

asustarCampamento :: [Niño -> Grito] -> [Niño] -> [Grito]
asustarCampamento monstruos niños = (aplanar . map (\x -> map x niños)) monstruos

sumatoriaEnergia :: [Grito] -> Int
sumatoriaEnergia gritos = foldl (\acu x -> acu + energiaDeGrito x) 0 gritos

aplanar :: [[a]] -> [a]
aplanar listaDeListas = foldl (++) [] listaDeListas

-- Recibimos unos "monstruos" que son una [Niño -> Grito], mas unos niños que son [Niño], nosotros queremos aplicar todos los elementos de [Niño -> Grito] a los elementos de [Niño] para conseguir [Grito], al cual querremos aplicar a todos sus elementos la funcion "energiaDeGrito" para obtener una sumatoria de la misma (foldl)
-- 1) para aplicar todos los elementos de [Niño -> Grito] a los elementos de [Niño] usamos "asustarCampamento", recibe ambos y usa un map que recibe una funcion incognita y [Niño -> Grito], la incognita va recibiendo los elementos de "monstruos" con "x", como cada x es "Niño -> Grito" y quiero aplicarlo a un [Niño] entonces no solo aplico x a los "niños" si no que tengo que usar una que sea secuencial, entonces "map x niños" aplica cada elemento de [Niño] a "Niño -> Grito", ahora este es solo un elemento de [Niño -> Grito] entonces al aplicar cada elemento de [Niño] a cada elemento de [Niño -> Grito] obtenemos una [[Grito]] = [ [(x,y,z),...,(x,y,z)],...,[(x,y,z),...,(x,y,z)] ], entonces nosotros queremos "aplanar" [[Grito]] a [Grito], que es como un concat pero creado por nosotros
-- 2) ya con [Grito] ahora solo queremos hacer la sumatoria, osea a cada elemento de [Grito] aplicarle "energiaDeGrito" y obtener un Int, entonces esto es un foldl que recibe una incognita una semilla y [Grito], la incognita por su sintaxis toma primero "acu" que es el valor de la semilla, que al ser una sumatoria sera 0, y los elementos de [Grito] en "x", entonces lo que haremos sera una SUMA ITERATIVA donde sumamos "acu" mas aplicar "energiaDeGrito" a cada elemento de [Grito], osea a cada grito

-- variante 1: usando concat y sum
produccionEnergeticaGritos' :: [Niño -> Grito] -> [Niño] -> Int
produccionEnergeticaGritos' monstruos niños = (sumatoriaEnergia . asustarCampamento monstruos) niños

asustarCampamento' :: [Niño -> Grito] -> [Niño] -> [Grito]
asustarCampamento' monstruos niños = (concat . map (\x -> map ($ x) monstruos)) niños

sumatoriaEnergia' :: [Grito] -> Int
sumatoriaEnergia' gritos = (sum . map energiaDeGrito) gritos

-- tenemos "monstruos" que son [Niño -> Grito] y "niños" que son [Niño], y queremos devolver un entero que represente aplicar "energiaDeGrito" a [Grito] que devolveria aplicar cada elemento de [Niño] a cada elemento de [Niño -> Grito], entonces
-- 1) con "asustarCampamento" recibimos [Niño -> Grito] y [Niño] entonces, si queremos aplicar cada elemento de [Niño] a [Niño -> Grito] para obtener un [Grito], uso un map que recibe una funcion incognita y [Niño], la incognita va recibiendo cada elemento de [Niño] en "x", que es un Niño, y queremos aplicarlo a [Niño -> Grito], ahora no podemos mandar "monstruos x" porque "monstruos" es una lista de funciones [Niño -> Grito] y "x" un valor solo, entonces tambien secuencialmente tenemos que aplicar "x" a cada elemento de [Niño -> Grito], lo hacemos con otro map que podria usar otra funcion incognita pero hacemos que reciba "map ($ x) monstruos" que significa aplica este valor "x" a cada elemento de [Niño -> Grito]. Como vimos anteriormente esto nos da [[Grito]] entonces tenemos que aplanarlo, usamos "concat" que ya existe y labura esto perfecto [[x]] = [x]
-- {la anterior es otra forma de aplicar lo que hicimos en la variante anterior, aca pensando en los elementos de "niño" y en la anterior pensando en los elementos de "monstruo"}
-- 2) teniendo la [Grito] que necesitabamos ahora solo queremos aplicar a cada elemento de [Grito] la funcion "energiaDeGrito" y sumarlo, entonces hacemos eso donde primero mapeamos con "energiaDeGrito" a la [Grito] obteniendo una lista de enteros que podemos sumar finalmente con "sum"

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

{- 7) Hacer una única función produccionEnergetica que reemplace a produccionEnergeticaRisas y a produccionEnergeticaGritos y que aplicada con los argumentos adecuados ermita obtener lo mismo que ellas, es decir, que reciba un conjunto de monstruos o un conjunto de comendiantes y devuelva la energía producida  -}

-- La solucion anterior es muy parecida a la solucion del ante-anterior, entonces buscamos ahora generalizar lo igual y parametrizar lo distinto, este punto entonces busca una funcion mas generica que resuelva el problema de la produccion energetiva ya sea de gritos o risas, osea que reciba monstruos o comediantes y ambos hagan lo mismo

{- produccionEnergeticaGritos :: [Niño -> Grito] -> [Niño] -> Int
produccionEnergeticaGritos monstruos niños = (sumatoriaEnergia . asustarCampamento monstruos) niños 

produccionEnergeticaRisas :: [Niño -> Risa] -> [Niño] -> Int
produccionEnergeticaRisas comediantes niños = (sumatoriaEnergiaRisas . hacerReirCampamento comediantes) niños 

asustarCampamento :: [Niño -> Grito] -> [Niño] -> [Grito]
asustarCampamento monstruos niños = (aplanar . map (\x -> map x niños)) monstruos  (esto como para denotar que puedo pensar en aplicar todos elementos de [x -> y] en [x] o al reves tambien todos [x] en [x -> y])

hacerReirCampamento' :: [Niño -> Risa] -> [Niño] -> [Risa]
hacerReirCampamento' comediantes niños = (concat . map (\x -> map ($ x) comediantes)) niños  

sumatoriaEnergiaRisas :: [Risa] -> Int
sumatoriaEnergiaRisas risas = (sum . map energiaRisa) risas                                                                                -}

-- Es decir, estas soluciones está acopladas en sus nombres de variables y la  función que aplican (energiaRisas o energíaGritos). Entonces, simplemente, parametricemos esas funciones, y de paso, utilicemos nombres más genéricos:

-- produccionEnergetica :: [Niño -> a] -> [Niño] -> Int
-- produccionEnergetica productores niños = (sumatoriaEnergia . extraerEnergiaCampamento productores) niños

-- sumatoriaEnergiaGeneral :: [a] -> Int
-- sumatoriaEnergiaGeneral producciones energiaProduccion = (sum . map energiaProduccion) producciones

-- extraerEnergiaCampamento :: [Niño -> a] -> [Niño] -> [a]
-- extraerEnergiaCampamento productores niños = (concat.map (\niño -> pam niño productores)) niños

-- bueno se ve que el tipado en algun punto esta mal, o directamente algo de la definicion, pero en general asi es la generalizacion de la misma
