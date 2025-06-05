{- Buscar departamentos por los medios tradicionales es una tarea compleja, ya que requiere mucho tiempo de investigación buscando en los clasificados de los diarios
y recorriendo inmobiliarias. Es por eso que hoy en día cada vez son más las personas que dejaron eso atrás dejando que internet se encargue de buscar las supuestas
mejores alternativas para sus necesidades.

Por eso surge una nueva página para buscar departamentos que permita al usuario personalizar sus propias búsquedas y de paso eventualmente mandarle mails con las 
nuevas ofertas inmobiliarias que podrían ser de su interés a ver si agarra viaje.

Tenemos los departamentos modelados de la siguiente forma: -}

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool  --Otra manera de llamarle a esta funcion, aquella que recibe un Departamento y devuelve un booleano
type Busqueda = [Requisito]  -- Una lista de funciones que reciben un dpto y devuelven un booleano

data Depto = Depto {
    ambientes :: Int,
    superficie :: Int, 
    precio :: Int, 
    barrio :: Barrio
} deriving (Show, Eq)

{- Notar que aca no estamos usando deriving, es por un campo que tiene esta data porque no hay una forma "sencilla" de comparar  -}

{- EXTRA!! 
Y en consola para construir una persona seria algo asi
> Persona "fulano@gmail.com" [[(\ depto -> ambientes dpto > 2)]]    (La cual esta bien implementada pero no podremos mostrar, y creo que ni usamos)

para confirmar esto podriamos intentar que nos muestre el mail y habra respuesta
> mail (Persona "fulano@gmail.com" [[(\ depto -> ambientes dpto > 2)]])
"fulano@gmail.com"   -}

data Persona = Persona {
    mail :: Mail, 
    busquedas :: [Busqueda] --Dijimos que Busqueda es una lista de Requisitos, entonces una persona tiene una lista de listas de funciones (Requisito)
}

{- EXTRA!! Como ejercicio de inferencia queremos tipar estas funciones para saber de donde a donde van

ordenarSegun :: ? -> [??] -> [??]
ordenarSegun :: ? -> [a] -> [a]  (Esto lo noto porque "obvia" la primer parte que no sabemos todavia que es, luego recibe la lista vacia y devuelve la lista vacia)
ordenarSegun :: (a -> ? -> Bool) -> [a] -> [a]  (notamos que "criterio" siendo compuesta con not, osea devuelve booleano, pero anteriormente esta parcialmente 
aplicada con x y como vimos anteriormente x es de tipo a) 
ordenarSegun :: (a -> a -> Bool)  (Finalmente nos damos cuenta que recibe otro parametro de tipo a porque a esa composicion luego le pasamos xs, otro elemento de la
lista que es de tipo a) -}

{- Logica: Recibe una funcion que espera 2 parametros que son 2 elementos de la lista, y una lista, entonces ordena  -}

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

{- between ::? -> ? -> ? -> ? 
   between ::? -> ? -> ? > Bool (La operacion principal es && que se reduce a un booleano)
   between ::? -> ? -> ? -> Bool 
   between ::Ord a => a -> a -> a -> Bool  (Aca notamos que minimamente los parametros tendran que ser el mismo tipo, a, y como estamos comparando, entonces todos
deben ser de la misma familia Ord)  -}

{- Logica: Recibe 3 parametros , y me dice si el segundo esta en el medio entre el primero y el tercero, osea si estan ordenados -}

between ::Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [Depto 3 80 7500 "Palermo", Depto 1 45 3500 "Villa Urquiza", Depto 2 50 5000 "Palermo", Depto 1 45 5500 "Recoleta"]

{- Se pide desarrollar las siguientes funciones y consultas de modo que se aprovechen tanto como sea posible los conceptos de orden superior, aplicacion parcial y
composicion 

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
Ejercicios
---------------------------------------------------------------------------------------------------------------------------------------------------------------------

1) a) Definir las funciones mayor y menor que reciban una funcion y dos valores, y retorna true si el resultado de evaluar esa funcion sobre el primer valor es 
mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente  -}

-- Respuesta 

{- Primero que nada la logica de esta funcion es tener en cuenta que anteriormente debe existir una funcion que se encargue de recibir un parametro y devuelva un 
valor que sea comparable, luego 2 parametros que sean del mismo tipo del primero que recibe la funcion anterior, entonces a esta funcion le pasamos dicha funcion
para que sea aplicada en los otros 2 parametros y nos diga si es mayor (En la primera funcion que nosotros hacemos) que aplicarla en el primer parametro que en el
segundo y viceversa en la otra

mayor :: (?) -> ? -> ? -> Bool  (Ambas sabiendo que reciben una funcion y 2 parametros, y devuelve un booleano)
menor :: (?) -> ? -> ? -> Bool

mayor :: (a -> b) -> a -> a -> Bool 
menor :: (a -> b) -> a -> a -> Bool (Los 2 parametros que reciben tienen que ser del mismo tipo para que la funcion pueda trabajar con ellos, por consiguiente la 
el primer parametro de la funcion que recibe la funcion principal debe ser del mismo tipo que los otros 2, osea, a)

mayor :: Ord b => (a -> b) -> a -> a -> Bool 
menor :: Ord b => (a -> b) -> a -> a -> Bool (Finalmente el resultado de la funcion, b, que recibe debe devolver un valor que se pueda comparar ya que la funcion 
principal se basa enteramente en ese resultado) -}

mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor funcion parametro parametro2 = funcion parametro > funcion parametro2

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor funcion parametro parametro2 = funcion parametro < funcion parametro2

{- Una salida (Si llego a pasar este parcial a papel podria dibujar el paso a paso)
ghci> mayor (*(-1)) (-3) 2
True
ghci> menor (*(-1)) (-3) 2
False -}

-- b) Mostrar un ejemplo de como se usaria una de estas funciones para ordenar una lista de strings en base a su longitud usando "OrdenarSegun"

-- Respuesta

{- Basicamente nos esta pidiendo que hagamos un ejemplo de una funcion que del otro lado del igual use "ordenarSegun" y tambien alguna de las otras dos, como esta
marca el camino de como debe recibir los parametros  ( ordenarSegun :: (a -> a -> Bool) -> [a] -> [a] )

-- ejemploDeOrdenarSegun = ordenarSegun (__ __) ["1", "esteVaASerElTercero", "dos"]

Osea no tenemos que estar rompiendonos la cabeza en entender como funciona "ordenarSegun" solamente enfocarnos en pasarle bien los otros 2 parametros que necesita,
ya que hasta ahora cumplimos con pasarle bien una lista ( ordenarSegun :: (a -> a -> Bool) -> ✓ -> ✓ ) Como pidio que se use una funcion creada anteriormente
entonces cumplimos con eso

-- ejemploDeOrdenarSegun = ordenarSegun (mayor __) ["1", "esteVaASerElTercero", "dos"]

Finalmente como estamos queriendo ordenar segun "longitud" lo mas probable es pasarle como segundo parametro la funcion length (lo cual tiene SENTIDO porque la 
funcion "mayor" ademas necesita una funcion criterio que termina dando un resultado "b" que se puede ordenar, en este caso sera un entero con la cantidad de letras)

-- ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["1", "esteVaASerElTercero", "dos"]

Si intentamos entender toda la logica se vuelve medio pesadazo porque hay mucha delegacion de parametros, pero basicamente lo que hace es ordenar esa lista segun
mayor cantidad de letras por elemento de la lista, si usaramos "menor" seria lo mismo pero con la menor cantidad de letras  -}

{- IMPORTANTE!!
Lo fundamental de este punto no es ponerse a intentar entender toda la logica detras, porque estamos jugando con funciones ya creadas del enunciado, pero si ver 
que parametros en un orden logico pasarlos -}
                                    
ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["1", "esteVaASerElTercero", "dos"]

{- 2) a) Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra en alguno de los barrios de la lista. -}

-- Respuesta

-- Ya nos estan dando el tipado a partir de el enunciado
ubicadoEn :: [Barrio] -> Depto -> Bool

{- Hasta ahora entonces lo que nos esta pidiendo basicamente es, recibir una lista de Strings que representan una lista con los nombres de los barrios, luego recibe
un data de tipo Depto, el uno de sus campos o funcion de acceso es "barrio", entonces nosotros tenemos que verificar la funcion de acceso de ese data que nos pasaron
esta contenida en la lista de barrios  -}

{- Podriamos entonces usar la funcion elem -}

ubicadoEn barrios departamento = elem (barrio departamento) barrios

{- EXTRA!!
Si quisiera usar una notacion point free podria hacer la siguiente composicion

ubicadoEn barrios departamento = (`elem` barrios) . barrio departamento => ubicadoEn barrios = (`elem` barrios) . barrio

Donde claro la funcion de acceso "barrio" recibe el departamento, que eso se reduce a un String que luego recibe elem pero "flipeado" ya que en su definicion recibe
primero el elemento y luego la lista, entonces aplicado parcialmente con la lista de barrios, y entonces recibe elem el String y se fija si esta en la lista de 
Strings (Pareciera que asi es mas de guapo)  -}


{- 2) b) cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función al ser aplicada con el departamento se encuentra entre 
los dos valores indicados.  -}

-- Respuesta

{- 1) Leyendo bien el enunciado "funcion al ser aplicada en departamento" osea la funcion que recibe esta funcion debera recibir primero un parametro de tipo Depto
osea sera una transformacion de un parametro de tipo Depto, LO QUE HACE Y COMO NO NOS INTERESA, solo nos quedamos con que ESO DEVUELVE UN VALOR COMPARABLE
2) "Se encuentra entre los valores indicados" ya con eso notamos que la funcion que transforma el Depto debera devolver un tipo de dato que primero sea del mismo
tipo que los otros dos parametros que le pasamos, luego tambien deberan ser ordenables por que los vamos a comparar CON LO QUE DEVUELVE LA FUNCION ANTERIOR
3) Finalmente un parametro de tipo Depto al cual le mandare a iniciar toda la comparacion (Vemos que tenemos una funcion que ya recibe un Depto, y como probablemente
necesitemos componer necesitamos un parametro de ese tipo que le pasaremos para que arranque todo) -}

{- IMPORTANTE!!
Es bastante raro y abstracto este enunciado, pero pareciera que varios enunciados son de esta manera, como solo buscamos tipar y definir una funcion que haga cierta
cosa quizas hay que dejar de lado el "como" y solo quedarnos con algo que intente cumplir eso que nos dice  -}

cumpleRango :: Ord a => (Depto -> a) -> a -> a -> Depto -> Bool

{- Necesitamos una funcion que nos indique que un valor esta "en medio" de otros dos, justamente nos dan una ya tipada que cumple esto, recibe 3 parametros comparables
y se encarga de hacer eso, entonces podemos aplicarla parcialmente con las 2 cotas que nos dieron -}
{- Luego sabemos que hay una funcion que recibe un Depto (Lo que haga con ella y como lo haga nos importa un chori) entonces pensamos en componer con lo anterior
que esta aplicado parcialmente para ya recibir aquello que devuelve esta funcion que recibe el Depto, entonces componemos -}

cumpleRango funcion cotaInferior cotaSuperior departamento = (between cotaInferior cotaSuperior . funcion) depto

{- Finalmente teniendo esto que cumple con el prototipo podemos plantear alternativas con point free (No es obligatorio)
 cumpleRango funcion cotaInferior cotaSuperior = between cotaInferior cotaSuperior . funcion    -}

{- 3) a) Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.  -}

{- Basicamente me van a dar una sola busqueda (Que es una lista de requisitos, una lista de funciones que reciben un depto y devuelve un booleano) y un depto
entonces lo que pide es "si cumple que TODOS los REQUISITOS de una BUSQUEDA se VERIFICAN para un DEPTO DADO" osea, usaremos la funcion all para que en la lista de
funciones que significa una busqueda se evaluen todas en el departamento que le pasemos     -}

cumpleBusqueda :: Depto -> Busqueda -> Bool

{- IMPORTANTE!!
¿Cuando usar ($ a) funcionAplicar? Cuando queremos usar ese parametro para esa funcion especifica, y si hablamos de una lista de funciones que reciben todos como
primer parametro ese tipo a y queremos hacer que se apliquen ahi de manera secuencial (Anidado con una funcion que haga eso ademas como un "all" o "any") podemos 
hacerlo ya nos aseguramos que se use ese parametro para esa funcion

FUNDAMENTAL!! 
Ademas cuando pensamos en una funcion que recibe una lista de funciones que recien un parametro a y devuelven algo (Potencialmente un Bool), y un parametro de tipo 
a, la mejor manera de apuntar a que con ese parametro se efectue de manera secuencial en toda la lista de funciones seria con un all o un any mas 1) Expresion
lambda o 2) Usando ($ a) que me parece mucho mas linda porque es "hacemos que se haga estas funciones listo" (Esto porque ingresar a una lista de funciones no es 
del todo regalado) -}

cumpleBusqueda departamento busqueda = all ($ departamento) busqueda
-- cumpleBusqueda' departamento = all ($ departamento)

{- Con expresion lambda
 
-- cumpleBusqueda'' departamento busqueda = (\requisito -> ...) 

Siendo que busqueda es una lista de requisitos, un elemento de esa lista es UN requisito, ahora viendo el tipo de requisito vemos que da de Depto a Bool, entonces
quiero ver que ese requisito al aplicarse al "departamento" se cumple o no

-- cumpleBusqueda'' departamento busqueda = (\requisito -> requisito departamento)

Finalmente hasta ahora tengo como se aplicar individualmente para un departamento ,como quiero que se haga secuencialmente para toda una lista de requisitos entonces
uso all y le paso la lista donde estan todas las funciones que queremos que haga eso ("busqueda")

-- cumpleBusqueda'' departamento busqueda = all (\requisito -> requisito departamento) busqueda

A eso tambien podemos aplicar point free pero estariamos  -}

{- EXTRA!!
Una forma que nos da este profesor para intentar pensar las funciones para directamente hacerlas mas cancheras y con point free es pensar la salida como una funcion
gracias a la currificacion

cumpleBusqueda :: Depto -> (Busqueda -> Bool)

Pero como todo es opcional, directamente podriamos pensar la solucion que consideremos y luego reescribirla si queremos, o no -}












