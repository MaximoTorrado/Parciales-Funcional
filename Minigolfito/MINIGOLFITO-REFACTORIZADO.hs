import Text.Show.Functions  -- esto nos permite mostrar las funciones

-- Modelo inicial

{- Lisa Simpson se propuso a desarrollar un programa que le permita ayudar a su hermano a vencer a su vecino Todd en un torneo de minigolf. Para hacerlo mas interesante, los padres de los
niños hicieron una apuesta: el padre del niño que no gane debera cortar el cesped del otro usando un vestido de su esposa
De los participantes nos interesara el nombre del jugador, el de su padre y sus habilidades (fuerza y precision)  -}
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles

between n m x = elem x [n .. m]

maximoSegun' f = foldl1 (mayorSegun f)

mayorSegun' f a b
  | f a > f b = a
  | otherwise = b

putter :: Palo
putter habilidad = UnTiro {
    velocidad = 10, 
    precision = precisionJugador habilidad * 2, 
    altura = 0
    }
madera :: Palo
madera habilidad = UnTiro {
    velocidad = 100,
    precision = precisionJugador habilidad `div` 2,
    altura = 5
    }
hierro :: Int -> Palo      
hierro n habilidad = UnTiro {
    velocidad = fuerzaJugador habilidad * n,
    precision = precisionJugador habilidad `div` n,
    altura = (n - 3) `max` 0 -- dado el caso donde la resta "n - 3" sea negativa entonces el minimo es el 0
}

-- hierro 4 (habilidad bart)
-- UnTiro {velocidad = 100, precision = 15, altura = 1}

-- b. Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego
type Palo = Habilidad -> Tiro

palos :: [Palo]
palos = [putter, madera {- hierro 1,..., hierro 10, tenemos q zafar esto generalizando-} ] ++ map hierro [1..10]

-- 2.
-- Lo que hace golpe es a partir de un jugador aplicar la funcion palo que labura con la habilidad de un jugador
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

-- golpe bart putter
-- UnTiro {velocidad = 10, precision = 120, altura = 0}

golpe' :: Palo -> Jugador -> Tiro
golpe' palo = (palo . habilidad)

-- golpe' bart putter
-- UnTiro {velocidad = 10, precision = 120, altura = 0}


-- 3. Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. En principio necesitamos representar los siguientes obstáculos: 

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 3- 2) MODELO ALTERNATIVO GUARDANDO LAS FUNCIONES "GENERALES" EN UN DATA (cuando llegamos al punto 4 vemos que con el planteo anterior no podiamos aplicar, entonces tenemos que usar un REFACTOR, mirar, es muy interesante)

-- 3. Generalizacion de los obstaculos, esta funcion cumplia dos roles 1) el de crear los obstaculos mediante las condiciones de cada uno y los efectos luego de aplicarlos y 2) ver como quedaba el tiro luego de aplicar superar el obstaculo, era un 2 en 1, ahora al usar un REFACTOR la 1) tarea la tomara el constructor del data nuevo y la 2) en otra funcion donde del obstaculo que creamos se guarda su condicion y su efecto los campos del data, a los cuales llamaremos y evaluaremos

-- obstaculoSuperableSi :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
-- obstaculoSuperableSi condicion efecto tiroOriginal 
--  | condicion tiroOriginal = efecto tiroOriginal
--  | otherwise = tiroDetenido

-- type Obstaculo = Tiro -> Tiro

tiroDetenido :: Tiro 
tiroDetenido = UnTiro 0 0 0

-- aca tenemos que hacer un REFACTOR: Cambiar el diseño de una solucion sin cambiar la funcionalidad de la misma, donde replanteamos lo que es un obstaculo donde tendra la condicion y el efecto luego de superar, ahora la primera funcion de la funcion general anterior de crear un obstaculo la pasamos al constructor de este data
data Obstaculo = UnObstaculo {
    puedeSuperar :: (Tiro -> Bool),
    efectoLuegoDeSuperar :: (Tiro -> Tiro)
}

-- luego la segunda operacion de la funcion general anterior de ver si supera o no (lo que hacia en las guardas) lo hacemos en esta nueva
intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal 
  | (puedeSuperar obstaculo) tiroOriginal = (efectoLuegoDeSuperar obstaculo) tiroOriginal 
  | otherwise = tiroDetenido

-- a. entonces usando el REFACTOR anterior donde antes usabamos "obstaculoSuperableSi" para crear los obstaculos ahora usamos el constructor del data Obstaculo
tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita 

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

vaAlRasDelSuelo tiro = ((==0) . altura) tiro

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = UnTiro {
    velocidad = velocidad tiro * 2,
    precision = 100,
    altura = 0
    }

-- b. Lo mismo aca ahora creamos este obstaculo usando el constructor del nuevo data obstaculo
laguna :: Int -> Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo} 

-- c. Lo mismo aca y de paso para mostrar como seria sin point free 
hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo 

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && vaAlRasDelSuelo tiro

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido

-- intentarSuperarObstaculo tunelConRampita (UnTiro {velocidad = 80, precision = 95, altura = 0})
-- UnTiro {velocidad = 160, precision = 100, altura = 0}
-- intentarSuperarObstaculo tunelConRampita (UnTiro {velocidad = 80, precision = 55, altura = 0})
-- UnTiro {velocidad = 0, precision = 0, altura = 0}

{- 4a. Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.  -}

-- que "palos" significa una lista de palos que habiamos creado en el punto 1, "a partir de un conjunto de palos quiero conseguir  otro conjunto de palos segun cierto criterio" 

-- type Obstaculo ::  Tiro -> Tiro
-- golpe :: Jugador -> Palo -> Tiro
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

-- no tenemos como saber si le sirve o no el palo, ya que el obstaculo finalmente devuelve un tiro pero no un booleano, entonces tenemos que hacer un REFACTOR a como modelamos el Obstaculo (rediseñar sin generar cambios en la funcionalidad)

-- puedeSuperar quiere recibir el obstaculo y el tiro que viene del golpe para ver si es capaz de superarlo o no, pudimos hacerlo ahora usando el REFACTOR con el nuevo data del obstaculo entonces llamamos dicho campo y le pasamos lo que necesita
leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)

-- palosUtiles bart tunelConRampita
-- [<function>]
-- head (palosUtiles bart tunelConRampita) (habilidad bart)
-- UnTiro {velocidad = 10, precision = 120, altura = 0}

-- 4b. Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.
-- Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos de un hoyo, el resultado sería 2 ya que la velocidad al salir del segundo túnel es de  40, por ende no supera el hoyo.
-- BONUS: resolver este problema sin recursividad, teniendo en cuenta que existe una función takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.

-- La idea es aplicar un tiro a una lista de obstaculos, que dicho tiro tambien vaya sufriendo los cambios de cada obstaculo y finalmente devolver la cantidad de obstaculos que pudo superar, un primer error seria pensarlo con un filter viendo que obstaculos supera dicho tiro, pero no esta teniendo el efecto de los anteriores por eso es "consecutivo" 

-- Entonces a partir del tiro, la lista de obstaculos pero abriendolo con su logica (head : tail) planteamos la recursividad, suponiendo que en la lista de obstaculos ya estan creados (ya se hizo el paso 1) entonces podemos usar el paso 2) llamando a la condicion y el efecto de dicho obstaculo ya creado, donde se fija se puede superar el elemento de la lista (un obstaculo) si si suma 1 y se vuelve a llamar pero aplicando el efecto de dicho obstaculo y pasandole ahora una copia del tiro con el efecto aplicado, a ver si lo pasa, algo asi 1 + 1 + 0 ó 1 + 1 + 1 + 0 (caso base) etc

cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosConsecutivosSupera tiro [] = 0  -- caso base
cuantosObstaculosConsecutivosSupera tiro (obstaculo : obstaculos) -- aca abrimos la lista en su logica (head (elemento individual) : tail (resto de elementos))
  | puedeSuperar obstaculo tiro = 1 + cuantosObstaculosConsecutivosSupera (efectoLuegoDeSuperar obstaculo tiro) obstaculos 
  | otherwise = 0

-- cuantosObstaculosConsecutivosSupera (UnTiro {velocidad = 10, precision = 95, altura = 0}) [tunelConRampita, tunelConRampita] 
-- 2  

-- 4c. Definir paloMasUtil que recibe una persona y una lista de obstaculos y determina cual es el palo que le permite superar mas obstaculos con un solo tiro

-- Funciones que nos dan, queremos saber el tipo de ambas
-- recordamos que el tipo del foldl1 : foldl1 :: Foldable t => (a -> a -> a) -> [a] -> a 
-- maximoSegun :: ?? -> [a] -> a
-- luego f debe ser mirando "mayorSegun" una funcion que tome valores t y devuelva cualquiera x, pero como en la definicion de "mayorSegun" estamos comparando entonces debe ser de la familia "Ord"
maximoSegun :: Ord b => (t -> b) -> [t] -> t 
maximoSegun f = foldl1 (mayorSegun f)

-- mayorSegun :: ?? -> a -> a -> ??
-- sabemos que tomara entonces un valor a y devolvera un cualquiera x, ahora ese x como estamos comparando en las guardas debe ser de la familia "Ord", luego mirando que es lo que devuelve
-- vemos que son los valores que recibe, que son de tipo a tambien
mayorSegun :: Ord x => (a -> x) -> a -> a -> a  
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- esta funcion debe trabajar con la lista de palos que tengo y devolvernos aquel que pase mayor cantidad de obstaculos consecutivos, osea es un llamado a "maximoSegun" que sabemos que le pasaremos nuestra lista de palos ahora tenemos que pensar la funcion que le pasaremos a "maximoSegun" que deberia esperar un elemento de la lista de palos, osea un palo y devolver una cantidad de obstaculos que paso, asi lo hara con todos los elementos de la lista de palos y comparara cual es el mayor, la cual como vemos es la que definimos en el punto anterior 
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos =  maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos . golpe jugador) palos 

-- cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Int 
-- 1) tenemos la lista de obstaculos por parametro la cual podemos pasarle a la funcion tranquilamente  
-- 2) tenemos el jugador por parametro y para conseguir el tiro podemos usar la funcion golpe :: Jugador -> Palo -> Tiro, PERO como vemos aca primero recibe un jugador y un PALO, de forma que si respetamos el orden del tipado osea "(cuantosObstaculosConsecutivosSupera (golpe jugador) obstaculos) palos" la funcion "golpe" nunca recibe los elementos de la lista "palos" con los palos para obtener los tiros, entonces tenemos que cambiar el orden de los mismos usando flip (cuando lo usamos devuelve una funcion que ahora esperaria la otra parte, por eso usamos composicion)

-- (paloMasUtil bart [tunelConRampita, tunelConRampita, hoyo])           
-- <function>
-- golpe bart (paloMasUtil bart [tunelConRampita, tunelConRampita, hoyo])   
-- UnTiro {velocidad = 10, precision = 120, altura = 0}

-- 5. Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo, se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.
-- le pone estos nombres a las funciones "fst" y "snd" como para ser mas expresivos, digamos
jugadorDeTorneo = fst
puntosGanados = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = (map (padre . jugadorDeTorneo) . filter (not . gano puntosDeTorneo)) puntosDeTorneo

-- el ultimo mapeo es ideal porque toma de la lista de tuplas solo los miembros del jugador, que de tipo "Jugador" y tienen en su campo a "padre" cargado con un string con el nombre del padre, entonces va creando una lista con los strings de los padres de todos aquellos que no ganaron

-- dados todos los puntos del torneo (tupla de los jugadores y sus puntos) y una tupla de un jugador y sus puntos
gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador = (all ((< puntosGanados puntosDeUnJugador) . puntosGanados) . filter (/= puntosDeUnJugador)) puntosDeTorneo 

-- con el filter filtramos la lista de tuplas con todos aquellos que sean distinto a la tupla individual que tenemos, para tener todas las tuplas menos la individual luego con el all queremos ver si los puntos ganados
-- de la tupla individual se cumple que son mayores a todos los puntos de las demas tuplas de la lista, osea de los puntos de todos los demas jugadores menos este, si se cumple esto entonces es el ganador de la carrera

-- gano [(bart, 10), (todd, 7), (rafa, 8)]
-- <function>
-- gano [(bart, 10), (todd, 7), (rafa, 8)] (bart, 10)
-- True                                                   (tiene sentido, uno gano porque quitando dicha tupla tiene mas puntos que todos los demas)
-- gano [(bart, 10), (todd, 10), (rafa, 8)] (bart, 10) 
-- False                                                  (tiene sentido, nadie gano porque quitando dicha tupla ninguna tiene mas puntos que todos los demas)
-- pierdenLaApuesta [(bart, 10), (todd, 10), (rafa, 8)] 
-- ["Homero","Ned","Gorgory"]                             (tiene sentido, nadie gano "todos perdieron" porque ninguna tupla tiene mas puntos que todos los demas)
-- pierdenLaApuesta [(bart, 9), (todd, 10), (rafa, 8)]  
-- ["Homero","Gorgory"]
