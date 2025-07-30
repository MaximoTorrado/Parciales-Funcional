-- Parte A
-- Tendremos comediantes de los que conocemos su nombre, su apodo, los años de experiencia en el rubro y su repertorio de chistes. Sobre cada comediante necesitamos lo siguiente:

data Comediante = Comediante { 
  nombre :: String,
  apodo :: String, 
  experiencia :: Int,
  repertorio :: [Chiste]  -- el type o el data que define los chistes los definiremos cuando modelemos los chistes
} -- deriving Show

-- tieneCancha: tiene cancha si está hace más de 10 años en el rubro o si su apodo es “Risitas de oro”.
tieneCancha :: Comediante -> Bool
tieneCancha comediante = haceMasDe 10 comediante || tieneApodo "Risitas de oro" comediante

haceMasDe :: Int -> Comediante -> Bool
haceMasDe años comediante = (> años) . experiencia $ comediante

tieneApodo :: String -> Comediante -> Bool
tieneApodo apodito comediante = (== apodito) . apodo $ comediante 

-- aprenderChiste: cuando aprende un chiste nuevo, éste se suma a su repertorio y aumenta sus años de experiencia en 1.
aprenderChiste :: Chiste -> Comediante -> Comediante
aprenderChiste chiste comediante = mapExperiencia (+ 1) . mapRepertorio (chiste :) $ comediante

-- como aca tenemos que hacer un cambio a 2 atributos del data de un Comediante, mirando el item siguiente tambien se vuelve a pedir cambios a los mismos atributos planteamos los accesors de los mismos para no repetir logica, agregando un año en ese caso a la experiencia, y agregando en la [Chiste] al principio otra lista cargada con el chiste recibido

mapExperiencia :: (Int -> Int) -> Comediante -> Comediante
mapExperiencia f comediante = comediante { experiencia = f . experiencia $ comediante }

mapRepertorio :: ([Chiste] -> [Chiste]) -> Comediante -> Comediante
mapRepertorio f comediante = comediante { repertorio = f . repertorio $ comediante }

-- si bien hasta ahora no modelamos el chiste ya lo tenemos en caso de tener que generar un cambio a este atributo

-- cancelar: cuando se pasa de la raya repetidas veces, puede terminar siendo cancelado. Cuando esto pasa, sus años de experiencia se reducen a 0, pierde todo su repertorio y su apodo pasa a ser “Cancelator”.
cancelar :: Comediante -> Comediante 
cancelar comediante = mapApodo (const "Cancelator") . mapRepertorio (const []) . mapExperiencia (const 0) $ comediante 

-- aca hacemos uso de los map creados y los const cuando queremos que tome cierto valor constante, lo bueno es que vemos que funciona hasta para aplicar un String costante, para eso creamos "mapApodo"

mapApodo :: (String -> String) -> Comediante -> Comediante 
mapApodo f comediante = comediante { apodo = f . apodo $ comediante }

-- Modelar: Los 3 items mencionados. A Pipi, una comediante que apodan “31416”, tiene 42 años de experiencia y sabe un chiste corto, un chiste largo que dura 8 minutos y una anécdota que pasó en Bariloche.

pipi :: Comediante
pipi = Comediante { nombre = "pipi", apodo = "31416", experiencia = 42, repertorio = [chisteCorto, chisteLargo 8, anecdota "Bariloche"]}

-- este comediante nos da una idea de como seran modelados los chistes mas adelante, donde tendremos que usar aplicacion parcial de algunos para que sean chistes 

-- Parte B
-- Pero acá no nos olvidamos que para el show hacen falta un público y chistes. De cada persona del público sabemos el nombre, la edad, el índice de entretenimiento y su tipo de humor. Y de los chistes lo siguiente:
data Persona = Persona {
  nombrePersona :: String,
  edad :: Int,
  entretenimiento :: Int,
  humor :: String
}

type Publico = [Persona]

-- chisteCorto: aumenta el entretenimiento de una persona en 10 y cambia su tipo de humor a “básico”. 
chisteCorto :: Chiste       -- Persona -> Persona 
chisteCorto persona = mapEntretenimiento (+ 10) . mapHumor (const "basico") $ persona 

mapEntretenimiento :: (Int -> Int) -> Persona -> Persona 
mapEntretenimiento f persona = persona { entretenimiento = f . entretenimiento $ persona }

mapHumor :: (String -> String) -> Persona -> Persona 
mapHumor f persona = persona { humor = f . humor $ persona }

-- ya viendo los chistes siguientes vemos que tenemos que hacer cambios a atributos en comun, para no repetir logica usamos accesors

-- anecdota: es un chiste muuuy largo que sólo entretiene a la persona si tiene humor “paciente”, aumentando su entretenimiento en 2 veces la cantidad de letras que tenga el lugar donde ocurrió la anécdota. En caso contrario, se aburre disminuyendo su entretenimiento a la mitad.
anecdota :: String -> Persona -> Persona 
anecdota ubicacion persona  
  | tieneHumor "paciente" persona = mapEntretenimiento (+ 2 * (length ubicacion)) persona
  | otherwise = mapEntretenimiento (`div` 2) persona

-- recibe un string "ubicacion" y una persona, entonces la condicion sera si tiene dicho humor que le pasamos, como lo tendremos que usar en el chiste que viene entonces lo partimos en "tieneHumor" asi no repetimos logica, si se cumple entonces usamos el accesor del entretenimiento pasandole una suma del doble de las letras del string de "ubicacion", que al ser un string direcamente con un length obtenemos la cantidad de letras  y le mandamos dicha ubicacion  
-- notamos que anecdota de arranque no sabemos si es un chiste o no (por eso no lo ponemos en su tipado), en caso de serlo para que pueda serlo deberia ser aplicado parcialmente con un String

tieneHumor :: String -> Persona -> Bool 
tieneHumor tipo persona = (== tipo) . humor $ persona

-- recibe el posible tipo de humor y la persona, de la persona queremos conseguir su humor de dicho atributo y finalmente comparamos si es igual al posible tipo que recibimos 

-- chisteLargo: si el humor de la persona es “básico”, le quita un año de edad según la cantidad de minutos que dure el chiste. Si no, pasa sin pena ni gloria.
chisteLargo :: Int -> Chiste    -- Persona -> Persona 
chisteLargo duracion persona 
  | tieneHumor "basico" persona = persona { edad = edad persona - duracion }
  | otherwise = id persona 

-- recibe un entero que hace referencia a la duracion del chiste y una persona, la condicion usamos "tieneHumor" definida anteriormente dado sea el caso como es la unica vez donde vamos a generar un cambio a un atributo de una Persona podemos usar la segunda forma sin necesidad de crear un accesor, caso contrario devuelve la misma persona como vino

-- Modelar los chistes.

-- Habiendo modelado los chistes podemos ahora plantear el data o el type que define a un chiste
type Chiste = Persona -> Persona 

--Parte C
-- Llegó el momento… ¡hora del show! Al darUnShow, un o una comediante le cuenta todo su repertorio de chistes al público. Es importante saber leer al público que va a presenciar el show de stand up, por lo que nos interesa saber algunas métricas pre y post show:
darUnShow :: Comediante -> [Persona] -> [Persona]  --  si bien aca deberiamos usar el type Publico usamos asi para ser mas explicitos
darUnShow comediante personas = map (aplicarShow comediante) personas

-- pensamos el publico como [Persona] entonces queremos a cada elemento de [Persona] osea a cada persona queremos aplicarle los chistes de este comediante, y devolver [Persona] entonces mapeamos con una funcion capaz de recibir los elementos de [Persona] e individualmente aplicar los chistes del comediante, lo partimos en "aplicarShow" que aplicamos parcialmente con el comediante asi conseguimos sus [Chiste]

aplicarShow :: Comediante -> Persona -> Persona
aplicarShow comediante persona = foldl (\acu x -> x acu) persona (repertorio comediante)

-- esta gracias al map que se encarga de hacer las cosas secuencialmente recibe los elementos de "personas" osea una persona individual, y como queremos devolver como queda la persona despues de aplicarle los chistes pensamos en un foldl, como queremos aplicar la persona a todos los elementos de [Chiste] entonces la lista que recibe foldl sera la que sale del atributo repertorio del comediante, la semilla sera la persona y por sintaxis del foldl hacemos que cada chiste se aplique con la persona esta devolviendo finalmente una persona que paso por todos los chistes

{- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Diferencia con monsters inc: 
en "asustarCampamento" tenemos [Niño -> Grito] como lista de funciones y [Niño] como lista de valores, ¿porque uno necesita concat y el otro no? la diferencia es querer aplicar "todas las funciones a todos los valores" por eso usamos un doble mapeo "asustarCampamento monstruos niños = concat (map (\x -> map x niños) monstruos)" 
En los comediantes "cada persona escucha todos los chistes del comediante" y "no queremos todos los resultados intermedios, solo como queda cada persona despues de todos los chistes"

Monstruos  => Cada funcion se aplica a toda la lista de valores  => [[Grito]] => necesita de concat (porque hay listas intermedias, una por cada funcion)

Comediante => A cada valor se aplicas toda la lista de funciones => [Persona] => no necesita de concat (cada valor genera un unico resultado)

considero fuertemente que las 2 estan bien nomas que una da "pasos intermedios" que podriamos aplanar o no, y la otra sin "pasos intermedios" si no que el estado final solamente
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  -}
darUnShow' :: Comediante -> [Persona] -> [Persona]
darUnShow' comediante personas = aplicarShow' (repertorio comediante) personas

aplicarShow' :: [Chiste] -> [Persona] -> [Persona] 
aplicarShow' chistes personas = concat (map (\chiste -> map chiste personas) chistes)

-- vaASerUnExito: dado un line up de comediantes y un público, el show va a funcionar si todos los comediantes tienen cancha o es un público experimentado (la suma de todas las edades del público es mayor a 150).
vaASerUnExito :: [Comediante] -> [Persona] -> Bool
vaASerUnExito comediantes personas = all tieneCancha comediantes || esPublicoExperimentado personas 

esPublicoExperimentado :: [Persona] -> Bool
esPublicoExperimentado personas = (> 150) . sum . map edad $ personas

-- seVanATentar: cuando al menos 3 personas del público tienen humor “fácil”.
seVanATentar :: [Persona] -> Bool
seVanATentar personas = (>= 3) . length . filter (tieneHumor "facil") $ personas

--nos viene como anillo al dedo porque justamente "tieneHumor" es booleana

--registroPostShow: necesitamos saber cómo queda el público después de que un o una comediante da un show. Para mantener los datos anónimos, queremos un registro que sólo tenga el índice de entretenimiento y el tipo de humor de cada persona. 
registroPostShow :: Comediante -> [Persona] -> [Datos]
registroPostShow comediante personas = devolverDatosAnonimos . darUnShow comediante $ personas

type Datos = (Int, String)

-- si queremos ver como queda el publico despues de dar un show, entonces recibimos un comediante y una persona para obtener [Persona] que queda despues de dar el show, ahora de esta no queremos la lista con todos los datos si no que queremos especificamente solo los atributos entretenimiento y humor de cada persona, entonces creamos el type con Datos para que se [Persona] hacerla [(Int, String)] partiendola con "devolverDatosAnonimos"

devolverDatosAnonimos :: [Persona] -> [Datos]
devolverDatosAnonimos personas = map (\x -> (entretenimiento x, humor x)) personas

-- recibe [Persona] despues de dar un show, ahora como queremos a partir de [Persona] devolver [Datos] a partir de cada persona ya pensamos en un map, que recibe una incognita y personas, ahora la incognita va a ir recibiendo los elementos de [Persona] que son personas, del otro lado del igual entonces devolvemos en una tupla el atributo entretenimiento de "x" para el primer miembro y el atributo humor de "x" para el segundo

-- me enamore de esta solucion, osea mientras recibamos los elementos de una lista de cualquier cosa con una incognita nosotros podemos operar con dichos elementos y devolver otra estructura 

-- Modelar lo requerido para poder dar un show y obtener las métricas.

-- Parte D

-- Imaginate un público infinito… ¿al menos así habría alguien que se ría de mis chistes? 😭 Respondé y justificá las siguientes preguntas teniendo en cuenta un público infinito:

-- ¿Qué pasaría si queremos saber si el público se va a tentar?
-- Considero que no podria hacerlo ya que en "seVanATentar" busca primero filtrar, nunca podra terminar de filtrar, segundo busca el length y ya el length de una lista infinita nunca terminara de evaluar

-- ¿Qué pasaría si queremos saber si va a ser un éxito?
-- Solo por el orden de como esta presentado "all tieneCancha comediantes || esPublicoExperimentado personas" es posible en el caso que sea verdadero la primera condicion, en el caso que sea falso ya al ir a la evaluacion de la lista infinita como quiere hacer la suma de todas las edades nunca termina de evaluar 
-- bonus dado el caso que plantearamos las cosas de forma "invertida" osea "esPublicoExperimentado personas || all tieneCancha comediantes" ya directamente no termina de evaluar porque hasta no tener respuesta del lado izquierdo no pasa a la otra condicion

-- ¿Qué pasaría si queremos obtener el registro post show?
-- como es un publico infinito nunca terminara de evaluar las infinitas posibilidades donde le aplicamos el chiste, si las arrancaria a hacer quizas si hacemos un "take n" podriamos obtener algunas pero lo mismo






