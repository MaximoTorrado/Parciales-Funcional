import Distribution.Simple.LocalBuildInfo (ComponentLocalBuildInfo(componentExposedModules))
-- Parte A

-- Nuestro programa tendrá participantes que cuentan con nombre, trucos de cocina y un plato que es su especialidad. Los platos, a su vez, tienen una dificultad que va de 0 a 10 y un conjunto de componentes que nos indican sus ingredientes con sus respectivos pesos en gramos.

data Participante = Participante {
  nombre :: String, 
  trucos :: [Truco],   -- el type que define esto lo haremos luego de plantear los trucos
  especialidad :: Plato
}

data Plato = Plato {
  dificultad :: Int,   
  componentes :: [Componente] 
}

type Componente = (Ingrediente, Int)
type Ingrediente = String

--Algunos de los trucos más famosos son:
-- 1. endulzar: dada una cantidad de gramos de azúcar, le agrega ese componente a un plato.  

-- primero que nada mirando el primer truco es una modificacion a el atributo componentes de un plato, entonces para evitar la repeticion de logica planteamos un accesor de cambio al mismo
mapComponentes :: ([Componente] -> [Componente]) -> Plato -> Plato
mapComponentes f plato = plato { componentes = f . componentes $ plato }

endulzar :: Int -> Truco           -- Plato -> Plato
endulzar gramos plato = agregarComponente "azucar" gramos plato

-- notamos de una que necesitamos una funcion que sea capaz de ingresar desde un data plato, a su atributo [Componentes] que es [(Ingrediente, Int),..., (Ingrediente, Int) ], y alli especificamente de este punto agregar "azucar" y esos gramos en una tupla, para esto lo partimos en "agregarComponente"

agregarComponente :: String -> Int -> Truco                   -- Plato -> Plato
agregarComponente ingrediente gramos plato = mapComponentes (\x -> (ingrediente, gramos) : x) plato

-- recibe un string que hace referencia al primer miembro de la tupla para el "ingrediente", lo mismo segundo miembro para "gramo" y el plato, como es un llamado al "mapComponentes" le tenemos que pasar una funcion incognita que reciba los elementos de plato y al ser un llamado a "mapComponentes" se encarga de ir al atributo [Componente] de cada plato, entonces no recibe elementos de Plato si no que recibe realmente elementos de [Componente] que son tuplas, entonces del otro lado concatenamos al principio una tupla formada por ingrediente y gramo que recibimos 

-- salar: la vieja y confiable… dada una cantidad de gramos de sal y un plato, nos retorna el mismo con esa cantidad de sal para que quede flama.
salar :: Int -> Truco                  --  Plato -> Plato
salar gramos plato = agregarComponente "Sal" gramos plato

-- hacemos uso de "agregarComponente" definida anteriormente que recibe el ingrediente los gramos y el plato a agregar, entrando a [Componente] del plato y concatenando al principio una tupla formada por los 2 miembros que le pasamos

-- darSabor: dadas una cantidad de sal y una de azúcar sala y endulza un plato.
darSabor :: Int -> Int -> Truco                  -- Plato -> Plato
darSabor gramosSal gramosAzucar plato = endulzar gramosAzucar . salar gramosSal $ plato 

-- recibimos el segundo miembro gramos de ambos azucar y sal, entonces a dicho plato tenemos que salarlo y enduzarlo, esto justamente lo hacemos con las definidas anteriormente "endulzar" "salar", que justamente solo necesitan ese segundo miembro para usar "agregarComponente" asi agregan dicha tupla cuyo primer miembro para cada una es constante, una es "azucar" otra es "sal"

-- duplicarPorcion: se duplica la cantidad de cada componente de un plato… para más placer.
duplicarPorcion :: Truco                        -- Plato -> Plato
duplicarPorcion plato = mapComponentes (map duplicarCantidad) plato 


-- nuevamente como queremos hacer un cambio al atributo [Componente] de un plato entonces hacemos un llamado a "mapComponente", como este solo es capaz de entrar a [Componente] nosotros si queremos duplicar todas las tuplas tenemos que pasarle al mapC una funcion capaz de duplicar los elementos de [Componente] esto nos da una idea de un map, entonces mapeamos con una funcion que partimos que se encargue de eso en "duplicarComponente"

duplicarCantidad :: Componente -> Componente
duplicarCantidad (ingrediente, cantidad) = (ingrediente, cantidad * 2)

-- estando en [Componente] el map le pasamos esta funcion, que debe ser unaria entonces debe recibir los elementos de [Componente], entonces lo recibe por matcheo y del otro lado solo devolvemos una tupla con el mismo nombre del ingrediente pero duplicando si el segundo miembro cantidad 

-- simplificar: hay platos que son realmente un bardo. Es por ello que si un plato tiene más de 5 componentes y una dificultad mayor a 7 lo vamos a simplificar, sino lo dejamos igual. Simplificar  un plato es dejarlo con 5 de dificultad y quitarle aquellos componentes de los que hayamos agregado menos de 10 gramos. 
simplificar :: Truco                 -- Plato -> Plato
simplificar plato 
  | esComplejo plato = plato { dificultad = 5, componentes = filter ((>=10). snd) (componentes plato) } 
  | otherwise = id plato 

-- miramos primero si es complejo el plato como condicion, dado que sea el caso entonces como vamos a plantear una modificacion a un plato como data entonces podemos usar la segunda forma sin necesidad de usar los map de cada atributo, poniendo su atributo dificultad en 5, y a su atributo componentes le asignamos la [Componente] filtrara que devuelve el filtrar [Componente] del plato segun primero conseguir los segundos miembros de las tuplas que hacen referencia a los gramos, y quedarnos con aquellos mayores iguales a 10, esa [Componente] que devuelve se la asignamos a dicho atributo, caso que no se cumpla devolvemos el mismo plato

esComplejo :: Plato -> Bool
esComplejo plato = dificultad plato > 7 && ((>5) . length . componentes) plato

-- esta se encarga de recibir un plato, del mismo queremos su atributo [Componente] del cual queremos la cantidad de elementos y ver si es mayor o menor a 5

-- podriamos tambien plantear una manera usando el map y al mismo tiempo cambiando asignando un atributo especifico que seria una cosa asi

-- esComplejo unPlato = mapComponentes (filter hayMucho) $ unPlato { dificultad = 5 }
-- hayMucho (_, unosGramos) = unosGramos >= 10

-- despues de modelar los trucos entonces planteamos el type del truco
type Truco = Plato -> Plato

-- De los platos también nos interesa saber:
-- esVegano: si no tiene carne, huevos o alimentos lácteos.
esVegano :: Plato -> Bool
esVegano plato = not . any esProductoAnimal . componentes $ plato

-- recibimos un plato, para saber si tiene o no ciertos componentes entonces necesitamos de su [Componente], luego como nos dijo matos es mejor usar la afirmacion y luego negarla que otra cosa, entonces nos fijamos si de [Componente] hay "algun" de ciertos productos entonces usamos "any" que se fija secuencialmente de cada elemento de [Componente] si es o no, y lo partimos con "esProductoAnimal", luego lo negamos

esProductoAnimal :: Componente -> Bool
esProductoAnimal (ingrediente, _) = elem ingrediente recoveco 

-- gracias al any que hace las cosas de forma secuencial va recibiendo elementos de [Componente] osea Componente, es una tupla y como solo queremos ver el primer miembro "ingrediente" entonces por matcheo recibimos solo ese miembro y obviamos el segundo, ese miembro que llamamos "ingrediente" nos fijamos si es o no alguno de los productos que nos dijeron usando "elem" mas dicho valor y una lista con los productos animales en "recoveco"

recoveco :: [Ingrediente]
recoveco = ["Leche", "Carne", "Huevos", "Manteca"]

-- justamente hace referencia a todos los posibles productos que son animales, entonces con el elem se fija si cada ingrediente cumple alguno con ser uno de estos

-- esSinTacc: si no tiene harina.
esSinTacc :: Plato -> Bool 
esSinTacc plato = not . tiene "Harina" $ plato

-- primero recibimos un plato, del mismo directamente como nos dijo matos es mejor hacer la afirmacion y luego la negacion, entonces partimos con "tiene" y aplicado parcialmente "Harina" porque es el unico ingrediente a ver

tiene :: Ingrediente -> Plato -> Bool 
tiene ingrediente plato = elem ingrediente . ingredientes $ plato 

-- tiene recibe el ingrediente y el plato, del plato queremos conseguir de [Componente] = [(Ingrediente, Int),...,(Ingrediente, Int)] solamente [Ingrediente] para luego con un elem fijarnos si el ingrediente que nos pasaron, que es "Harina" esta dentro de esa [Ingrediente], podemos hacerlo todo ahi o directamente partirlo en otra funcion "ingredientes"

ingredientes :: Plato -> [Ingrediente]  -- [String]
ingredientes plato = map fst . componentes $ plato

-- recibe el plato, del mismo queremos [Componente] entonces usamos el atributo componentes, de ahi queremos [Componente] = [(Ingrediente, Int),...,(Ingrediente, Int)] que a cada elemento de [Componente] aplicarle fst, entonces mapeamos con "fst" consiguiendo finalmente [Ingrediente]

-- esComplejo: cuando tiene más de 5 componentes y una dificultad mayor a 7.
--  ya lo planteamos para "simplificar"

-- noAptoHipertension: si tiene más de 2 gramos de sal.
noAptoHipertension :: Plato -> Bool
noAptoHipertension plato = tiene "Sal" plato && cantidadDe "Sal" plato > 2

-- recibe un plato y queremos ver primero si tiene el ingrediente sal, que eso ya lo modelamos con la funcion anterior "tiene", en caso de tener significa que en [Componente] minimo habra una tupla que tiene como ingrediente sal y una cantidad n de gramos a ver, si cumple entonces vamos a partir en "cantidadDe" que se encargara de filtrar [Componente] segun cierto ingrediente para ver exclusivamente los gramos de la misma

cantidadDe :: Ingrediente -> Plato -> Int
cantidadDe ingrediente plato = (snd . filtrarComponentesSegun ingrediente) plato 

-- recibe el ingrediente que es un string y un plato, la idea es que a [Componente] del plato lo filtremos y devolvamos la unica tupla ("Sal", n) que tiene dicho ingrediente para mirar luego su cantidad en gramos con "snd"

filtrarComponentesSegun :: Ingrediente -> Plato -> Componente
filtrarComponentesSegun ingrediente plato = (head . filter ((== ingrediente). fst) . componentes) plato 

-- recibe el ingrediente que es el string mediante el cual vamos a identificar a la tupla dentro de [Componente] y el plato, del plato entonces queremos primero [Componente], luego queremos filtrarla segun todas aquellas tuplas cuyo primer miembro (Ingrediente, Int) es igual al "ingrediente" que recibimos, entonces para esto el filter debe recibir una funcion que pueda a cada elemento de [Componente] obtener su primer miembro de las tuplas esto con "fst" y luego mirar de la lista de [Ingrediente] que quedo sean iguales al "ingrediente" recibido, todo esto con "filter ((== ingrediente). fst)" 
-- [Componente] = [("Leche", 10),..., ("Sal", n)] => [("Sal", n)]
-- entonces finalmente de aquella lista con una unica tupla conseguimos dicha tupla usando "head" => ("Sal", n)

--Parte B
-- En la prueba piloto del programa va a estar participando Pepe Ronccino quien tiene unos trucazos bajo la manga como darle sabor a un plato con 2 gramos de sal y 5 de azúcar, simplificarlo y duplicar su porción. Su especialidad es un plato complejo y no apto para personas hipertensas.
--Modelar a Pepe y su plato. 

pepeRonccino :: Participante 
pepeRonccino = Participante { nombre = "Pepe Ronccino", trucos = [darSabor 2 5, simplificar, duplicarPorcion], especialidad = unPlatoComplejo }

unPlatoComplejo :: Plato 
unPlatoComplejo = Plato 10 [("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100)]

-- asi denotamos las 2 formas

-- Parte C
-- ¡Ahora sí! Manos a la obra… o manos en la masa… bueno, continuemos. Dado que este es un programa de concursos de cocina vamos a tener que modelar tres funcionalidades: 

-- cocinar: es el momento en el que la magia ocurre y vemos como queda finalmente el plato de un participante luego de aplicar todos sus trucos a su especialidad.
cocinar :: Participante -> Plato
cocinar participante =  aplicarTruco (especialidad participante) (trucos participante) 

-- recibe un participante y queremos ver el plato que devuelve despues de aplicar todos sus [Truco], que sale de su atributo trucos, a su plato que esta en su atributo especialidad, entonces queremos que el plato se aplique a cada elemento de [Truco], podriamos hacer todo esto es un solo foldl pero para que se vean bien la semilla y la lista lo partimos a "aplicarTruco", ya que no tenemos ninguna de las 2 a mano sin llamar a los atributos antes

aplicarTruco :: Plato -> [Truco] -> Plato 
aplicarTruco plato trucos = foldl (\acu x -> x acu) plato trucos

-- recibe un valor plato y [Truco], queremos aplicar el valor a todos los elementos de [Truco] y como pide devolver el plato luego de todo eso usamos foldl, que recibe una incognita una semilla y una lista, la semilla sera el valor plato que recibimos de arriba y la lista de trucos lo mismo, la incognita entonces por sintaxis de foldl recibe primero el valor de la semilla que es un plato en "acu" y luego los elementos de [Truco], como cada elemento es "Truco = Plato -> Plato" entonces no hay que aplicar parcialmente nada si no que directamente a cada elemento "x" de la lista le aplicamos el valor semilla en "acu"

-- esMejorQue: en esta contienda diremos que un plato es mejor que otro si tiene más dificultad pero la suma de los pesos de sus componentes es menor.
esMejorQue :: Plato -> Plato -> Bool
esMejorQue plato1 plato2 = esMasDificil plato1 plato2 && esMasLigero plato1 plato2 

-- partimos en otras dos ya que son dos condiciones a mirar

esMasDificil :: Plato -> Plato -> Bool
esMasDificil plato1 plato2 = dificultad plato1 > dificultad plato2

-- de aca miramos el atributo dificultad de cada plato para comparar

esMasLigero :: Plato -> Plato -> Bool
esMasLigero plato1 plato2 = peso plato1 < peso plato2

-- aca partimos en otra funcion "peso" para conseguirlo de cada plato, que sera la suma de los gramos de cada tupla de [Componente]

peso :: Plato -> Int
peso plato = sum . map snd . componentes $ plato

-- del plato queremos [Componente] de la misma, a cada elemento de la misma que son tuplas queremos los gramos entonces mapeamos con "snd", finalmente de la lista con los gramos sumamos todo con sum obteniendo el peso

-- participanteEstrella (este punto es el único en el que pueden usar recursividad si así lo desean): ¡se picó y no estamos hablando de la cebolla! Dada una lista de participantes, diremos que la estrella es quien luego de que todo el grupo cocine tiene el mejor plato.
participanteEstrella :: [Participante] -> Participante
participanteEstrella participantes = foldr1 mejorParticipante participantes

-- de [Participantes] queremos aquel que sea el mejor, aca viene el uso de foldr1 que "reduce una lista aplicando una funcion binaria" osea mediante una funcion que va comparando los elementos de [Participante] la reduce a un unico elemento Participante mientras compara, entonces para eso la partimos en "mejorParticipante"

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante participante1 participante2 
  | esMejorQue (cocinar participante1) (cocinar participante2) = participante1
  | otherwise = participante2 

-- esta funcion binaria que usa el foldr1 debe ir comparando de a 2 los elementos de [Participante] y devolver uno por cada comparacion, por lo tanto recibe 2 Participante (foldr1 se encarga de hacerlo secuencial), la comaparacion sera usando las funciones creadas anteriormente donde 
-- 1) usando "cocinar" vimos que aplica todos los trucos al plato del participante y devuelve su plato final
-- 2) usando "esMejorQue" vimos que recibe 2 platos, en este caso seran los 2 platos finales de los 2 participantes que estamos comparando para finalmente devolver cual es mejor, asi de forma iterativa hasta devolver el mejor 

participanteEstrella' :: [Participante] -> Participante
participanteEstrella' [participante] = participante
participanteEstrella' (head : tail) = mejorParticipante head (participanteEstrella' tail)

-- esta es la variante con recursividad, planteamos el caso base dado que la lista tenga un unico participante devolvemos dicho participante, luego recibimos [Participante] por su logica de (head : tail) donde usamos la que partimos en la otra variante "mejorParticipante", que se encarga de mirar los 2 platos finales de a 2 participantes, a la cual le pasamos "head", que seria un participante el elemento actual digamos, mas recursivamente esta funcion pero con los demas elementos con "tail" asi hasta llegar al caso base

-- Parte D
--Para finalizar vamos a modelar el plato definitivo, el platinum. Este plato tiene de especial que tiene infinitos componentes misteriosos con cantidades incrementales y dificultad 10:

-- > componentes platinum
-- [("Ingrediente 1", 1), ("Ingrediente 2", 2), ("Ingrediente 3", 3),..]

-- > dificultad platinum
-- 10

platinum :: Plato
platinum = Plato 10 componentesMisteriosos

componentesMisteriosos :: [Componente]
componentesMisteriosos = map (\x -> ("Ingrediente" ++ show x, x)) [1..]

-- usamos un map para devolver [("Ingrediente 1", 1), ("Ingrediente 2", 2), ("Ingrediente 3", 3),..], que recibe una incognita y una lista infinita desde 1 en adelante, la incognita va recibiendo elementos de la lista infinita con "x" y por cada elemento devuelve una tupla donde el primer elemento concatena "Ingrediente" mas el "show" de dicho elemento "x" (esto creo toma el elemento que en principio es un Int y lo hace String), y como segundo miembro dicho elemento "x"

-- Luego de modelar el platinum respondé las siguientes preguntas justificando tus respuestas:
-- ¿Qué sucede si aplicamos cada uno de los trucos modelados en la Parte A al platinum?

-- para "endulzar" no tendria problema porque agrega una tupla con ("azucar", gramos) al inicio del atributo componentes, que es una lista infinita pero al ser al principio no necesita evaluar toda la lista, lo mismo para "salar", lo mismo para "darSabor" ya que aplica 2 tuplas pero al inicio
-- ahora para "duplicarPorcion" no podra hacerse porque querra duplicar todas las tuplas y nunca terminara de evaluar los infinitos elementos de [Componente]
-- y para "simplificar" como en una de las condiciones mira el "length componentes > 5" ya por el solo hecho de que "length componentes" intente conseguir la longitud de una lista infinita no termina de evaluar 

-- ¿Cuáles de las preguntas de la Parte A (esVegano, esSinTacc, etc.) se pueden responder sobre el platinum? 

-- para "esVegano" y "esSinTacc" el uso del any implica que quierra recorrer toda la lista infinita de componentes por lo tanto depende de dos cosas, si estan dichos elementos en la lista infinita podra devolver un True, en caso de no estar no podra terminar de evaluar nunca
-- para "esComplejo" lo mismo que para "simplicar" quiere conseguir un length de una lista infinita no podra terminar de evaluar esa expresion especifica
-- para "noAptoParaHipertensos" solo tendra respuesta si hay sal, o falso o si hay sal con menos de 2 gramos, todos los demas casos donde no haya sal entonces nunca termina de evaluar

-- ¿Se puede saber si el platinum es mejor que otro plato?

-- no, el problema que encuentra es que en un momento quiere conseguir el peso, la dificultad no tendra problema porque no implica la lista infinita, pero el peso querra sumar todos los segundos miembros de las tuplas de [Componente] y no terminara de evaluar

