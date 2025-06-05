{- En epoca de cuarentena, los sitios de e-commerce son muy importante para poder abastecer a la poblacion sin que salga de sus hogares. Uno de los sitios mas 
famosos de este estilo, nos pidio ayuda para finalizar algunas funcionalidades. Todo muy bien hasta aca, pero hay un pequeño detalle: el listado de funciones a
desarrollar esta desordenado. Esto significa que algunas que aparecen al final son necesarias para realizar otras del principio. Vas a tener que pensar cuales
desarrollar primero. ¡Vamos! -}

-- Todas las funciones pedidas deben estar tipadas (Debemos hacer la definicion de la misma para saber de que tipo son)

{- precioTotal: Dado un precio unitario, una cantidad, un descuento y un costo de envío calcular el precio total. Para eso, hay que calcular el precio unitario con
descuento y multiplicarlo por la cantidad. ¡No te olvides de agregar el precio del envío! LISTO!!!!!

esProductoDeElite: Un producto es de elite si es de lujo, codiciado y no es un producto corriente. LISTO!!!!

aplicarDescuento: Dado un precio y un descuento, obtener el precio final con el descuento aplicado. LISTO!!!!!

entregaSencilla: Una entrega es sencilla, si se hace en un día sencillo. Los días sencillos son lo que tienen una cantidad de letras par en el nombre. Ejemplo de un
día: “20 de Abril de 2020”.  LISTO!!!!!

descodiciarProducto: Dado el nombre de un producto, generar uno que no sea codiciado. Para esto le vamos a sacar las últimas letras hasta que la cantidad de letras
en el nombre quede igual a 10 (ó menor a 10 en productos con nombres cortos)  LISTO!!!!!

esProductoDeLujo: Dado el nombre de un producto, saber si es de lujo. Un producto es de lujo cuando contiene una “x” o “z” en su nombre. LISTO!!!!

aplicarCostoDeEnvio: Dado un precio y un costo de envío, obtener el precio final una vez sumado el costo de envío.  LISTO!!!!!

esProductoCodiciado: Dado el nombre de un producto, saber si es un producto codiciado. Un producto es codiciado cuando la cantidad de letras en su nombre es mayor
a 10. LISTO!!!!!!!!!

esProductoCorriente: Dado el nombre de un producto, saber si es un producto corriente. Un producto es corriente si la primera letra de su nombre es una vocal. 
LISTO!!!

productoXL: Dado un producto, conseguir su versión XL. Esta se consigue agregando ‘XL’ al final del nombre. LISTO!!!!!

versionBarata: Dado el nombre de un producto conseguir su versión barata. La misma es el producto descodiciado y con su nombre dado vuelta. LISTO!!!! -}

{- Funciones que podemos utilizar y debemos tipar las usemos o no
take ::                 drop ::                  head ::               elem ::                   reverse ::
>> take 2 “Buenas!!”    >> drop 2 “Buenas!!”     >> head “Buenas!!”    >> elem ‘a’ “Buenas!!”    >> reverse “Buenas!!”
“Bu”                    “enas!!”                 ‘B’                   True                      "!!saneuB"
>> take 5 “Saludos”     >> drop 3 “Saludos”      >> head “Nos vemos”   >> elem ‘y’ “Buenas!!”    
“Salud”                 “udos”                   ‘N’                   False                                                -}

{- Respuesta:
Bueno arrancamos tipando las funciones estas, la definicion no la haremos (Aunque la mayoria las vi en las grabadas), pero bueno el tipado podemos deducirlo de solo
ver los ejemplos que nos dan -}
{- En vez de tirarnos de cabeza a que solo reciben String en el tipado mirando el ejemplo, si mal no me acuerdo las funciones que operan con cambiar algo de un 
String no usan ese tipo, si no que las pensamos como un "vector de caracteres" y bueno en este caso usaremos listas de caracteres, y ademas para generalizarlo mas
usamos listas de una familia de datos para no encapsularlo solo a strings, porque podemos usar estas funciones tambien para listas de numeros, float, etc 
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
head :: [a] -> [a]
elem :: a -> [String] -> Bool
reverse :: [a] -> [a]                          -}

{- Para las primeras funciones arrancando desde "precioTotal" siendo que dentro tambien se pide aplicar el descuento y el costo de envio, vamos a primero crear estas
2 funciones para luego delegarlas a la funcion "precioTotal", tambien usamos un type para ahorrar el definir 2 veces el mismo movimiento de funciones -}

{- IMPORTANTE!! 
Siempre que usemos type, al alias que usemos tiene que ARRANCAR CON MAYUSCULA, y ADEMAS SOLO USAR EN DONDE SE RESPETE UNICAMENTE ESTOS PARAMETROS-}

type Aplicar = Float -> Float -> Float

aplicarDescuento :: Aplicar
aplicarDescuento descuento precioUnitario = precioUnitario * (1 - descuento / 100) 

aplicarCostoDeEnvio :: Aplicar
aplicarCostoDeEnvio costoDeEnvio precioUnitario = precioUnitario + costoDeEnvio

{- FUNDAMENTAL!!
Cuando "delegar" parametros y cuando una funcion debe recibir todos los parametros (Igual no demasiada importancia a esto, si bien del todo no esta tan mal puede haber
algun error, lo dejamos por ahora en "Teoria")
1) Cuando "delegamos" parametros en una funcion principal, osea que esta misma operara con otras funciones creadas anteriormente y no necesariamente tiene que RECIBIR
(Del lado izquierdo del igual) los parametros con los que operara la otra funcion, si no que del otro lado del igual al llamar la funcion esta se encargara de pedir los
parametros que necesite, TENER EN CUENTA que al hacer esto SIEMPRE ES MEJOR DELEGAR CON FUNCIONES QUE HACEN UNA UNICA OPERACION, porque si queremos que opere con 
distintos parametros deberiamos cambiar toda la funcion o hacer una funcion conjunta

2) Cuando componemos funciones del otro lado del igual DEBEMOS RECIBIR (Del lado izquierdo del igual) TODOS LOS PARAMETROS, justamente porque en las composiciones 
nosotros le tendremos que pasar al menos un valor para que arranque, y tambien para hacer que funcione podemos aplicar parcialmente asi tiene valores que aquella con la
que esta compuesta no devuelve pero aplicando parcialmente ya los tiene (Obvio aplicando parcialmente con los valores que recibe de una la funcion principal)
  -}

{- IMPORTANTE!! 
Aca NO podemos usar el TYPE "Aplicar" porque justamente estamos AGREGANDO PARAMETROS entonces no funcionara, solo van donde son UNICAMENTE los parametros que pide type -}

--Notar que aca para que funcionen la composicion tenemos que hacer algunas aplicaciones parciales, por eso ademas recibimos todos los parametros

{- IMPORTANTE!!
Justamente aca denotar un parentesis ((composicion) parametro) * cantidad es REDUNDANTE, se ve que por precendencia de valores, ver despues bien   -}

precioTotal :: Float -> Float -> Float -> Float -> Float
precioTotal precioUnitario costoDeEnvio descuento cantidad = (aplicarCostoDeEnvio costoDeEnvio . aplicarDescuento descuento) precioUnitario * cantidad

{- Bueno ahora para "versionBarata" es facil solamente tenemos que usar reverse, pero anteriormente tenemos que estar asegurados que "descodiciamos" el producto primero
entonces tenemos que hacer primero esa funcion para luego usarla en esta -}

{- Basicamente lo que hace es recibir un nombre, lo pensamos como un vector de caracteres porque vamos a operar con el mismo, luego de alguna manera tenemos que sacarle
3 letras y luego fijarnos si es menor a 10, si no es menor volver a sacar 3 letras y asi hasta que tenga 10 o menos de 10 letras -}

{- ERROR PARA REPASAR!
descodiciarProducto :: [Char] -> [Char]  
descodiciarProducto nombre | ((>10) . length . drop 3) nombre == False = descodiciarProducto nombre --Aca
                           | otherwise = nombre 

Lo que podemos mejorar es que si miramos bien lo que hace esta funcion es con el drop (Que va sacando letras pero del inicio) PERO EL TEMA ESTA EN LA DERECHA DEL IGUAL
= descodiciarProducto nombre, basicamente lo que estamos haciendo aca es VOLVER A LLAMAR A LA FUNCION PERO sin SIMULAR NINGUN CAMBIO, para QUE TENGA SENTIDO deberiamos
pasarle EL NOMBRE PERO con el CAMBIO QUE SUFRIO   -}

{- Vamos a reintentar esto pero ahora sacando desde lo ultimo de a 3 letras, y luego del otro lado del igual si volviendo a llamar a la funcion pero pasandole como
parametro el nombre habiendo sufrido un cambio  
1) Primero que nada vemos si la longitud del nombre es mayor a 10, hacemos esto pasandole como parametro el nombre a la funcion length, la cual reduce todo a un entero
y podemos comparar 
2) Si se cumple que ES MAYOR QUE 10 entonces aca tenemos en cuenta la RECURSIVIDAD ¿Por que hacemos esto? Porque justamente dado el caso donde sea un nombre mayor a 10
tenemos que ir haciendo esto tantas veces sea necesario hasta que se cumpla que sea menor a 10.
Entonces DIRECTAMENTE en la RECURSIVIDAD le PASAMOS el parametro NOMBRE SIMULANDO EL CAMBIO QUE SUFRIRIA (Que es restarle 3) ¿Como haremos esto? 
3) Usaremos length que le pasaremos como parametro el nombre para que esta la convierta en un Int con la cantidad de numeros y a eso le restaremos 3, luego usaremos take
que su tipado recibe un entero (Que nace de length) y una lista de char, entonces le pasamos lo que nace de la operacion esa mas el nombre, todo eso se reducira a que
el take devuelva la TOTALIDAD de la cadena (De la operacion que nace de length) a partir del nombre principal sin cambios, esto lo recibe "descodiciarProducto" las veces
que sea necesaria hasta que sea menor que 10
4) Una vez no se cumpla que es mayor que 10 entonces devolvemos la cadena  -}

descodiciarProducto :: [Char] -> [Char]
descodiciarProducto nombre | length nombre > 10 = descodiciarProducto (take (length nombre - 3) nombre)
                           | otherwise = nombre

{- IMPORTANTE!! 
Tiene todo el sentido del mundo porque por cada vez que llamamos de vuelta la funcion estamos simulando una vez mas el cambio que le hacemos, entonces ahora le pasamos
a descodiciarProducto (La MISMA FUNCION que va a DEVOLVER un parametro) un parametro pero SIMULANDO el cambio (Que es ir restandole 3 letras) entonces devolver ese 
parametro que le fuimos restando 3 letras es valido  -}

{- Ahora si vamos por "versionBarata" -}

versionBarata :: [Char] -> [Char]
versionBarata nombre = reverse (descodiciarProducto nombre)

{- Bueno para "esProductoDeElite" se necesita anteriormente saber si el producto es de lujo, codiciado y ademas no es producto corriente, lo cual las 3 formas hay 
funciones para eso -}

{- IMPORTANTE!! 
Aca como es una funcion que devuelve un Booleano, NO hay que USAR "GUARDAS BOOLEANAS", una manera que voy a usar para evitar las guardas booleanas es una composicion 
donde le entra a length un nombre (de tipo [Char]) la cual devuelve un Int con la cantidad de letras y luego la recibe (>10) y devuelve un booleano  -}

{- IMPORTANTE!!
Aca tambien por default Haskell nos dice que podriamos usar notacion point free corte canchero asi de guapo   -}

esProductoCodiciado :: [Char] -> Bool
esProductoCodiciado nombre = ((>10) . length) nombre

esProductoDeLujo :: [Char] -> Bool
esProductoDeLujo nombre =  elem 'x' nombre || elem 'z' nombre 

{- ERROR PARA REPASAR
esProductoCorriente :: [Char] -> [Char]
esProductoCorriente nombre | elem (take 1 nombre) nombre == 'a' || 'A'

Basicamente aca estamos aplicando mal la funcion elem porque esta no devuelve ningun char si no que devuelve un booleano, estamos comparando mal las cosas -}

{- Anteriormente estaba intentando acomodar todo para que devuelva [Char] asi poder componer todo pero al final es mejor si uso que devuelva Booleano para que en la
funcion que anexa a las 3 directamente sea una sola linea donde se tenga que cumplir las 3 para ser cierto   -}

{- Aca lo que puedo hacer es usar la funcion elem que puede recibir una cadena de la siguiente manera "asdf" mas una lista y se fija si esa lista tiene alguno de los
elementos estos, ademas a eso le agregaremos la funcion head para que especificamente sea la primera letra (Aca estamos teniendo en cuenta que las cadenas de los nombres
son solo en minuscula, caso contrario deberiamos usar tolower) -}

esProductoCorriente :: [Char] -> Bool
esProductoCorriente nombre = elem (head nombre) "aeiou"

esProductoDeElite :: [Char] -> Bool
esProductoDeElite nombre = esProductoCodiciado nombre && esProductoDeLujo nombre && not (esProductoCorriente nombre)

{- Bueno para "productoXL" basicamente recibiendo un nombre, osea un parametro de tipo [Char] tenemos que agregar al final un "XL" -}

{- IMPORTANTE!! 
Para "agregar al final" de un String o un vector de caracteres (Lista en este caso, las 3 son sinonimos) podemos solamente usar el operador ++ y entre comillas aquello
que queremos agregar -}

productoXL :: [Char] -> [Char]
productoXL nombre = nombre ++ " " ++ "XL"

{- Finalmente para "entregaSencilla" basicamente dado un dia de forma "20 de Abril de 2020" por ejemplo, es una entrega sencilla si esa cadena tiene una cantidad par de
caracteres, entonces lo que podemos hacer es recibir la lista de caracteres que representa el dia, luego componer lenght con dicho dia asi se traduce en la cantidad de
letras que tiene y a esas componerla con la funcion even, que directamente toma un numero y devuelve si es o no es par (En booleano)
Como al final la even va a devolver un Booleano entonces el tipado de la funcion principal tiene que devolver un booleano -}

entregaSencilla :: [Char] -> Bool
entregaSencilla dia = (even . length) dia











