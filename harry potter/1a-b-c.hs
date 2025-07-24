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
--  a. sumaDeNiveles que suma todos sus niveles.

-- agarra todos los atributos de una persona y suma dichos campos (menos el nombre)
-- sumaDeNiveles :: Persona -> Int
-- sumaDeNiveles persona = fuerza persona +  inteligencia persona + suerte persona

--  b. diferenciaDeNiveles es la diferencia entre el nivel más alto y más bajo.
-- tengo que encontrar el mas grande entre los 3 atributos y el menor de los 3 atributos, para luego restar el primero con el segundo

-- diferenciaDeNiveles :: Persona -> Int
-- diferenciaDeNiveles persona = maximoNivel persona - minimoNivel persona

-- delegamos en 2 funciones que hagan esto, pero ahora notamos entre la funcion anterior como la idea si bien es distinta es muy parecida nomas cambiando los operadores 
-- maximoNivel :: Persona -> Int
-- maximoNivel persona = fuerza persona `max` inteligencia persona `max` suerte persona

-- minimoNivel :: Persona -> Int
-- minimoNivel persona = fuerza persona `min`  inteligencia persona `min` suerte persona

-- para evitar esa repeticion de logica hacemos un REFACTOR y pensamos esta funcion, que recibe una persona y a partir de dicha agarra sus niveles y los junta en una lista, devolviendo dicha lista
niveles ::Persona -> [Int]
niveles persona = [fuerza persona, inteligencia persona, suerte persona]

-- 1.a 
-- ahora es mas facil porque recibiendo la persona llamando a la funcion anterior tenemos una lista con sus niveles a la cual podemos sumar
sumaDeNiveles :: Persona -> Int
sumaDeNiveles persona = (sum . niveles) persona 

-- 1.b 
-- lo mismo para esta ya que podemos delegar en las 2 funciones que de ellas dos usar la funcion "maximum" y "minimum"
diferenciaDeNiveles :: Persona -> Int
diferenciaDeNiveles persona = maximoNivel persona - minimoNivel persona

maximoNivel :: Persona -> Int
maximoNivel persona = (maximum . niveles) persona

minimoNivel :: Persona -> Int
minimoNivel persona = (minimum . niveles) persona


--  c. nivelesMayoresA n, que indica la cantidad de niveles de la persona que están por encima del valor dado.
nivelesMayoresA :: Int -> Persona -> Int
nivelesMayoresA n persona = length . filter (>n) . niveles $ persona

-- recibe un entero y la persona, de dicha persona juntamos sus niveles en una lista llamando a esa funcion "niveles" luego la filtramos por aquellos elementos, osea aquellos niveles que son
-- mayores al n que le pasamos, y finalmente vemos la cantidad de niveles mayores a ese (cantidad = length, suma = sum)



