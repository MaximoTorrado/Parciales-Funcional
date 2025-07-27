{- Una persona muy muy muy fanática de esta gran historia, nos pidió crear el mundo de Naruto Uzumaki en el Paradigma Funcional. Así que, por favor, aplica tus conocimientos y ¡concedele el deseo! 🙏

Parte 1
En este mundo conocemos el nombre, las herramientas, los jutsus y el rango de cada ninja. El rango es un número que comienza en 0 y no puede ser negativo. Las herramientas ninjas son de mucha ayuda para realizar misiones. 
De cada una conocemos el nombre y la cantidad disponible. Algunos ejemplos son: bombas de humo, kunais, shurikens y sellos explosivos. Para poder utilizarlas se pide modelar: -}

data Ninja = Ninja {
  nombre :: String, 
  herramientas :: [Herramienta], 
  -- jutsus :: [Jutsu],       nosotros aca planteamos el atributo jutsus pero todavia no sabemos que son hasta que los modelemos
  rango :: Int
}

-- esto lo deducimos de que sabemos de cada Herramienta su nombre y su cantidad disponible
type Herramienta = (String, Int)

-- que pida que el atributo "rango" no sea negativo significa que en cualquier caso que querramos hacer algun cambio a dicho atributo su maximo sea 0, osea agregamos la misma condicion que parcial anterior al accesor
mapRango :: (Int -> Int) -> Ninja -> Ninja
mapRango f ninja = ninja { rango = max 0 . f . rango $ ninja }

-- luego leyendo los puntos que le siguen vemos que tratan sobre al menos al principio agregar herramientas, entonces creamos su accesor
mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramientas f ninja = ninja { herramientas = f . herramientas $ ninja }

-- si leemos mas a fondo vemos "cada ninja puede obtener una cantidad especifica de una herramienta" osea que en este punto no es solamente agregar una tupla "Herramienta = (String, Int)" si no que especificamente agregar
-- al segundo miembro de la tupla que representa la cantidad, entonces para eso y no repetir logica creamos su accesor a un miembro de una tupla de la siguiente manera
mapCantidad :: (Int -> Int) -> Herramienta -> Herramienta 
mapCantidad f (nombre, cantidad) = (nombre, f cantidad)

-- a. obtenerHerramienta: cada ninja debe poder obtener una cantidad específica de una herramienta en particular teniendo en cuenta que:
-- i. si la suma de todas sus herramientas más la cantidad a obtener es menor o igual a 100, puede hacerlo sin problemas;
-- ii. en caso contrario, obtiene la cantidad que pueda sin excederse de 100 herramientas.  

obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta herramienta ninja = mapHerramientas((mapCantidad . min . cuantasHerramientasPuedeObtener) ninja herramienta :) ninja
-- la idea es que esta funcion agregue una tupla a [Herramienta] de un ninja, esto lo haremos con "mapHerramienta" que le pasamos una funcion y a dicho ninja
-- 1) 
cuantasHerramientasPuedeObtener :: Ninja -> Int
cuantasHerramientasPuedeObtener ninja = (100 -) . cantidadDeHerramientas $ ninja

cantidadDeHerramientas :: Ninja -> Int
cantidadDeHerramientas ninja = sum . map snd . herramientas $ ninja

-- b. usarHerramienta: cuando un ninja usa alguna de sus herramientas no mide cuántas utiliza, por lo que se queda sin ella y no debe figurar más entre sus pertenencias.
usarHerramienta :: String -> Ninja -> Ninja
usarHerramienta otroNombreHerramienta ninja = mapHerramientas (filter ((/=) otroNombreHerramienta . nombreHerramienta)) $ ninja

nombreHerramienta :: Herramienta -> String
nombreHerramienta = fst

-- la idea es de la [Herramienta] quitar aquella herramienta que le pasamos, sabemos entonces que usaremos el "mapHerramienta" para hacer el cambio y le pasaremos un filter para quitar aquella tupla de [Herramienta] y a dicho 
-- ninja, al ser una tupla tenemos que tener alguno de los miembros para identificarla entonces, a todas las tuplas nos quedamos solo con el primer miembro nombre con "nombreHerramienta" que es un llamado a fst, luego con la
-- lista que nos quedo todas con el string del primer miembro de las tuplas las filtramos con todas aquellas que no tengan el nombre que recibimos por parametro, y listo 




