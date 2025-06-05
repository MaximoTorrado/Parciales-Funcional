{- Modelar Personaje, estos tienen: nombre, una cantidad de poder y una lista de derrotas (léase, las veces que derrotó a alguien). Cada derrota tiene el nombre
de su oponente y el año en el que ocurrió. -}
-- Respuesta:

{- Podria ser un data, pero aca lo hacen con tuplas -}

data Personaje = Personaje {
    nombre :: String,
    poder :: Int,
    derrotas :: [Derrota],
    equipamiento :: [Equipamiento]
}

{- Este type podriamos hacerlo en tuplas, y tambien en otro data, pero el tema con datas es la funciones de acceso  -}
type Derrota = (String, Int)

{- 2) Modelar entrenamiento, el cual lo realizan un grupo de personajes y multiplica el poder de cada uno de ellos por la cantidad de personajes que están 
entrenando al mismo tiempo. -}
-- Respuesta:

{- Como vamos a simular cambios a los campos de un Personaje podemos crear las funciones de acceso "De Cambio" para evitar repetir logica -}

mapNombre :: (String -> String) -> Personaje -> Personaje
mapNombre funcionModificacion unPersonaje = unPersonaje { nombre = (funcionModificacion . nombre) unPersonaje}

mapPoder :: (Int -> Int) -> Personaje -> Personaje
mapPoder funcionModificacion unPersonaje = unPersonaje { poder = (funcionModificacion . poder) unPersonaje }

mapDerrotas :: ([Derrota] -> [Derrota]) -> Personaje -> Personaje
mapDerrotas funcionModificacion unPersonaje = unPersonaje { derrotas = (funcionModificacion . derrotas) unPersonaje }


{- Recibe una lista de Personajes y devuelve otra lista de Personajes pero con los campos poder multiplicados por la cantidad de personajes que recibe la funcion
(Ya que la cantidad de Personajes que recibe esta funcion son aquellos que estan "entrenando") -}

entrenamiento :: [Personaje] -> [Personaje]

{-  Esta parte lo que es capaz de hacer es multiplicar al campo poder segun la cantidad de personaes PARA UN PERSONAJE, osea de forma INDIVIDUAL

=> mapPoder ((*) . length unosPersonajes) unosPersonaje 

Luego como la anterior es la funcion INDIVIDUAL y queremos hacerlo a una LISTA DE PERSONAJES, usamos map para que reciba la funcion anterior la "funcion criterio"
para que se aplique ahora a la lista de personajes "unosPersonajes" 

=> map (mapPoder (* length unosPersonajes) unosPersonajes) -}

-- entrenamiento unosPersonajes = map (mapPoder ((*) . length unosPersonajes) unosPersonajes)

entrenamiento unosPersonajes = map (entrenar (length unosPersonajes)) unosPersonajes

{- Podemos hacer una abstraccion mas para no andar haciendo map(mapPoder), asi queda una funcion mas linda -}
entrenar :: Int -> Personaje -> Personaje
entrenar multiplicadorDePoder unPersonaje = mapPoder (* multiplicadorDePoder) unPersonaje

{- 3) Modelar rivalesDignos, que dado un grupo de personajes nos dice quienes son rivales para Thanos. Son dignos aquellos personajes que, luego de haber entrenado, 
tienen un poder mayor a 500 y además alguna de sus derrotas se llame "Hijo de Thanos". -}
-- Respuestas:

{- Recibe una lista de personajes y devuelve otra que nos dice cuales personajes son dignos despues de entrenar -}
rivalesDignos :: [Personaje] -> [Personaje]

{- Sabemos que "esDigno" hara la tarea de decirnos si es digno recibiendo una lista de personajes despues de haber entrenado (Que las recibe de "entrenamiento) 
segun si tienen un poder mayor a 500 y si alguna algun elemento de la lista de derrotas que tiene tenga un String "Hijo de Thanos"
Basicamente filter recibe la "funcion criterio" que es una composicion, que es capaz de recibir un personaje entrenarlo y fijarse si es digno o no, y una lista
luego entonces filter se encarga de filtrar esa lista segun la "funcion criterio"  -}

rivalesDignos unosPersonajes = filter esDigno . entrenamiento $  unosPersonajes

esDigno :: Personaje -> Bool

{- Podriamos tambien hacer de esta solucion una mayor abstraccion, por las dudas si mas adelante no queremos repetir logica, lo hacemos para "derrotoA" y tambien
podriamos hacer una que se llame "poderMayorA" que use esta misma logica -}

-- esDigno unPersonaje = ((>500) . poder) unPersonaje && derrotoA "Hijo de Thanos" unPersonaje

esDigno unPersonaje = poderMayorA 500 unPersonaje && derrotoA "Hijo de Thanos" unPersonaje

poderMayorA :: Int -> Personaje -> Bool
poderMayorA unPoder unPersonaje = ((> unPoder) . poder) unPersonaje
                                  --poder unPersonaje > unPoder

derrotoA :: String -> Personaje -> Bool
derrotoA rival unPersonaje = elem rival (oponentes unPersonaje)
                                        -- (derrota unPersonaje) esto esta mal porque no podemos ingresar a un campo tupla sin una funcion especifica para eso 

-- derrotoA rival personaje = elem rival . oponentes $ personaje

{- Esta seria la funcion que se encarga de recibir un personaje y devolver la lista de derrotas que tiene dicho personaje, pero lo hacemos usando funciones de
tuplas, osea esta seria una funcion de acceso de una tupla, que es un campo de un personaje -}
oponentes :: Personaje -> [String]
oponentes unPersonaje = map fst . derrotas $ unPersonaje

{- Modelar guerraCivil, la cual dado un año y dos conjuntos de personajes hace que cada personaje pelee con su contraparte de la otra lista y nos dice quienes son
los ganadores. Cuando dos personajes pelean, gana el que posee mayor poder y se le agregará la derrota del perdedor a su lista de derrotas con el año en el que 
ocurrió. Por ejemplo...

guerraCivil 2018 [scarletWitch, capitanAmerica, blackWidow] [vision, ironMan, spiderman] 

...hará que Scarlet Witch pelee contra Vision, el Capitán América contra Iron Man y Black Widow contra Spiderman   -}
-- Respuesta:

guerraCivil :: Int -> [Personaje] -> [Personaje] -> [Personaje]

{- Usando zipWith lo que hacemos es hacer que mediante una funcion que simule la pelea hagamos que peleen elemento con elemento de las 2 listas de personajes-}

guerraCivil unAño unosPersonajes otrosPersonajes = zipWith (pelea año) unosPersonajes otrosPersonajes

pelea :: Int -> Personaje -> Personaje -> Personaje
pelea unAño unPersonaje otroPersonaje | poderMayorA (poder otroPersonaje) unPersonaje = leGanaA unAño unPersonaje otroPersonaje 
                                      | otherwise = leGanaA unAño unPersonaje otroPersonaje

leGanaA :: Int -> Personaje -> Personaje -> Personaje
leGanaA unAño unGanador unPerdedor = mapDerrotas ((nombre unPerdedor, unAño) : ) unGanador
                                            -- (++ [(nombre unPerdedor, unAño)] )
                                            -- ((:) (nombre unPerdedor, unAño) )

{- zipWith (+) [1, 2, 3] [4, 5, 6] 
[5, 7, 9]  

zipWith $ [(>10), odd] [15, 2] 
[True, False] 

zip [1, 2, 3] ["Hola", "Chau", "Pdep"]
[(1, "Hola"), (2, "Chau"), (3, "Pdep")] -}


{- Ahora aparecen los equipamientos: estos son objetos de poder que dado un personaje modifican sus habilidades de manera extraordinaria. Asimismo, sabemos que los
personajes tienen varios equipamientos.

1. Escribir el tipo de un Equipamiento y modificar el modelo de Personaje para que acepte equipamientos. -}
-- Respuesta:

type Equipamiento = Personaje -> Personaje

-- Y se agrega el campo "equipamientos" que es una lista de equipamientos, osea una lista de funciones de personaje a personaje

{- 2) Tenemos equipamientos generales como por ejemplo escudo y trajeMecanizado. Modelar los siguientes equipamientos.

escudo: si tiene menos de 5 derrotas le suma 50 de poder, pero si tiene 5 o más le resta 100 de poder.  -}

escudo :: Equipamiento
escudo unPersonaje | cantidadDeDerrotas unPersonaje < 5 = mapPoder (+50) unPersonaje
                   | otherwise = mapPoder (substract 100) unPersonaje

cantidadDeDerrotas :: Personaje -> Int
cantidadDeDerrotas unPersonaje = length (derrotas unPersonaje)

{- trajeMecanizado: devuelve el personaje anteponiendo "Iron" al nombre del personaje y le agrega una versión dada al final del mismo. Por ejemplo:
Si el personaje se llama "Groot" y la versión del traje es 2 , su nombre quedaría "Iron Groot V2"  -}
-- Respuesta: 

trajeMecanizado :: Int -> Equipamiento

{- Aca usamos una expresion lambda (una funcion anonima) solamente para que sea mas "facil" y evitemos estar definiendo una funcion que haga lo mismo, es mas 
"comodo" y quizas pueda usarse cuando se necesita de una funcion que haga bastantes cosas para un mismo data (esto es opcional) -}

trajeMecanizado unAño unPersonaje = mapNombre (\nombre -> "Iron" ++ nombre ++ "V" ++ show version) unPersonaje

{- También tenemos equipamientos exclusivos que solo lo pueden usar determinados personajes, por ejemplo: stormBreaker solo lo puede usar Thor, guantelete del
Infinito sólo lo puede usar Thanos, gemaDelAlma sólo lo puede usar Thanos. Modelar:

a) stormBreaker: Le agrega "dios del trueno" al final del nombre y limpia su historial de derrotas ya que un dios es bondadoso. -}
-- Respuesta:

equipamientoExclusivo :: String -> Equipamiento -> Personaje  -> Personaje
equipamientoExclusivo unNombre unPersonaje unEquipamiento | unNombre == nombre unPersonaje = unEquipamiento unPersonaje 
                                                          | otherwise = unPersonaje 

stormBreaker :: Equipamiento
stormBreaker unPersonaje = equipamientoExclusivo "Thor" (agregarDios . diosBondadoso) unPersonaje

diosBondadoso :: Personaje -> Personaje
diosBondadoso unPersonaje = mapDerrotas (const []) unPersonaje 

-- falta agregarDios

{- b) gemaDelAlma: Añade a la lista de derrotas a todos los extras, y cada uno con un año diferente comenzando con el actual. Considerar que hay incontables extras.
 Por ejemplo: [("extra numero 1", 2018), ("extra numero 2", 2019), …]   -}
 -- Respuesta:

gemaDelAlma :: Equipamiento
gemaDelAlma unPersonaje = equipamientoExclusivo "Thanos" unPersonaje mapDerrotas (++ derrotasInfinitas) 

derrotasInfinitas :: [Derrota] 
derrotasInfinitas = zip extrasInfinitos [2025 ..]

extrasInfinitos :: [String]
extrasInfinitos = map ("Extra numeros" ++) . show [1..]
