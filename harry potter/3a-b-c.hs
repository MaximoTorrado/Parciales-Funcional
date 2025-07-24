{-  Se pide desarrollar un programa Haskell que ayude a regular el consumo de las pociones que se enseñan a los alumnos del colegio Hogwarts de Magia y Hechicería.
Las pociones, producidas al combinar ingredientes exóticos que causan efectos diversos, pueden ser consumidas con el fin de alterar los niveles de suerte, inteligencia y fuerza de quien las bebe.
Para representar este modelo contamos con las siguientes definiciones:   -}

data Persona = Persona {
  nombrePersona :: String,
  suerte :: Int,
  inteligencia :: Int,
  fuerza :: Int
} deriving (Show, Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

-- Codigo Base

nombresDeIngredientesProhibidos = [
 "sangre de unicornio",
 "veneno de basilisco",
 "patas de cabra",
 "efedrina"]

-- inferimos esta funcion
-- vemos que recibe 2 parametros : -> -> 
-- el primer parametro vemos que debe ser una funcion que es aplicado a los elementos de una lista con la logica (head : tail) : (a -> x) -> [a] -> 
-- vemos que el resultado de aplicar las funciones las estamos comparando con desigualdades, entonces las englobamos en la familia "Ord" : Ord x => (a -> x) -> [a] ->
-- por ultimo vemos que devuelve uno de los elementos de la lista, que es de tipo a, entonce devuelve ese tipo : 

maximoSegun :: Ord x => (a -> x) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)

-- 1. Dada una persona definir las siguientes funciones para cuantificar sus niveles de suerte, inteligencia y fuerza sin repetir código:
niveles ::Persona -> [Int]
niveles persona = [fuerza persona, inteligencia persona, suerte persona]

-- 1.a sumaDeNiveles que suma todos sus niveles.
-- ahora es mas facil porque recibiendo la persona llamando a la funcion anterior tenemos una lista con sus niveles a la cual podemos sumar
sumaDeNiveles :: Persona -> Int
sumaDeNiveles persona = (sum . niveles) persona 

-- 1.b diferenciaDeNiveles es la diferencia entre el nivel más alto y más bajo.
diferenciaDeNiveles :: Persona -> Int
diferenciaDeNiveles persona = maximoNivel persona - minimoNivel persona

maximoNivel :: Persona -> Int
maximoNivel persona = (maximum . niveles) persona

minimoNivel :: Persona -> Int
minimoNivel persona = (minimum . niveles) persona

--1.c nivelesMayoresA n, que indica la cantidad de niveles de la persona que están por encima del valor dado.
nivelesMayoresA :: Int -> Persona -> Int
nivelesMayoresA n persona = length . filter (>n) . niveles $ persona

-- 2. Definir la función efectosDePocion que dada una poción devuelve una lista con los efectos de todos sus ingredientes.
efectosDePocion :: Pocion -> [Efecto]
efectosDePocion pocion = (concat . map efectos . ingredientes) pocion

-- 3. Dada una lista de pociones, consultar:
--  a. Los nombres de las pociones hardcore, que son las que tienen al menos 4 efectos.
pocionesHardcore :: [Pocion] -> [String]
pocionesHardcore pociones = (map nombrePocion . filter ((>=4) . length . efectosDePocion)) pociones

-- de la lista de pociones si la filtramos la funcion que recibe filter debe ser para UNA pocion capaz de decir si es hardcore o no, y para el map de la lista filtrada decirnos
-- los "nombresPocion" de cada una, justamente al map usamos esa funcion definida del data 
-- al filter le pasamos una composicion de "efectosDePocion" "length" y "(>=4)" donde toma los elementos de la lista de pociones, osea una pocion y devuelve su lista de efectos
-- luego mira la cantidad de efectos y se fija si es mayor igual a 4, asi filtra la lista de pociones

--  b. La cantidad de pociones prohibidas, que son aquellas que tienen algún ingrediente cuyo nombre figura en la lista de ingredientes prohibidos.
cantidadPocionesProhibidad :: [Pocion] -> Int
cantidadPocionesProhibidad pociones = length . filter esProhibida $ pociones 

-- a partir de una lista de pociones filtramos pasando una funcion al filter que sea capaz de decir si UNA pocion es prohibida o no, luego vemos la cantidad que quedo filtrada
esProhibida :: Pocion -> Bool
esProhibida pocion = any (flip elem nombresDeIngredientesProhibidos . nombreIngrediente) . ingredientes $ pocion

-- de una pocion componemos para conseguir su [Ingrediente] con "ingredientes" luego lo recibe "any" aplicado parcialmente de manera que de la [Ingrediente] consigamos primero 
-- los "nombreIngrediente" de cada uno y nos fijemos si tiene algun elemento de "nombresDeIngredientesProhibidos" usando "elem"
-- el tema con "elem" es que primero recibe es el elemento, que es lo que nos da "nombreIngrediente" entonces lo flipeamos para cambiar el orden de los parametros

--  c. Si son todas dulces, lo cual ocurre cuando todas las pociones de la lista tienen algún ingrediente llamado “azúcar”.
todasDulces :: [Pocion] -> Bool
todasDulces pociones = all (any (("azucar" ==) . nombreIngrediente) . ingredientes) pociones


-- quiero ver si todos los elementos de la lista de pociones cumplen con tener en su [Ingrediente] ALGUN elemento "azucar", todos y algun, entonces el que engloba es un "all" que
-- recibe una composicion donde primero obtenemos la [Ingrediente] de cada pocion con "ingredientes" luego nos fijamos si se cumple que dicha lista los "nombreIngrediente" alguno
-- usando "any" cumple con ser igual a "azucar" esto si se cumple para todos entonces es todasDulce si no, no



