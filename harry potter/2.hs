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

-- de la pocion sacamos sus ingredientes que nos devuelve una [Ingrediente, ... Ingrediente], luego si queremos mapear los elementos de esta lista con los efectos estaria bien PERO el tema es que por cada
-- elemento de la lista "efectos" devuelve [Efecto], entonces terminaria devolviendo [[Efecto], ..., [Efecto]]
-- para zafar concat [[x]] => [x]