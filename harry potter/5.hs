
{- Se pide desarrollar un programa Haskell que ayude a regular el consumo de las pociones que se enseñan a los alumnos del colegio Hogwarts de Magia y Hechicería.
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

--  b. La cantidad de pociones prohibidas, que son aquellas que tienen algún ingrediente cuyo nombre figura en la lista de ingredientes prohibidos.
cantidadPocionesProhibidad :: [Pocion] -> Int
cantidadPocionesProhibidad pociones = length . filter esProhibida $ pociones 

-- a partir de una lista de pociones filtramos pasando una funcion al filter que sea capaz de decir si UNA pocion es prohibida o no, luego vemos la cantidad que quedo filtrada
esProhibida :: Pocion -> Bool
esProhibida pocion = any (flip elem nombresDeIngredientesProhibidos . nombreIngrediente) . ingredientes $ pocion

--  c. Si son todas dulces, lo cual ocurre cuando todas las pociones de la lista tienen algún ingrediente llamado “azúcar”.
todasDulces :: [Pocion] -> Bool
todasDulces pociones = all (any (("azucar" ==) . nombreIngrediente) . ingredientes) pociones

-- 4. Definir la función tomarPocion que recibe una poción y una persona, y devuelve como quedaría la persona después de tomar la poción. Cuando una persona toma una poción, 
-- se aplican todos los efectos de esta última, en orden.
tomarPocion :: Pocion -> Persona -> Persona
tomarPocion pocion personaInicial = foldl (\acu x -> x acu ) personaInicial (efectosDePocion pocion)

-- 5. Definir la función esAntidotoDe que recibe dos pociones y una persona, y dice si tomar la segunda poción revierte los cambios que se producen en la persona al tomar la 
-- primera.

esAntidotoDe :: Pocion -> Pocion -> Persona -> Bool
esAntidotoDe pocion antidoto persona = (== persona) . tomarPocion antidoto. tomarPocion pocion $ persona 

-- basicamente no existe un "antidoto" son 2 pociones, lo que queremos hacer es a una persona hacerle tomar una pocion, con todo lo que eso implica que es aplicar dicha persona a
-- [Efecto] de la pocion, pero lo hicimos en "tomarPocion" para luego hacerle tomar la otra pocion ahora "antidoto" y ver si esta igual a como entro, osea le revirtio el efecto final

-- 6. Definir la función personaMasAfectada que recibe una poción, una función cuantificadora (es decir, una función que dada una persona retorna un número) y una lista de personas,
-- y devuelve a la persona de la lista que hace máxima el valor del cuantificador. Mostrar un ejemplo de uso utilizando los cuantificadores definidos en el punto 1.

personaMasAfectada :: Pocion -> (Persona -> Int) -> [Persona] -> Persona
personaMasAfectada pocion criterio personas = maximoSegun (criterio . tomarPocion pocion) personas  

-- recibimos una "pocion" una funcion "criterio" y la lista de "personas" usamos "maximoSegun" que espera una funcion que ira aplicando recursivamente a los elementos de una lista y comparara cada resultado, la funcion 
-- que le mandamos podria ser "criterio" sola pero como nos dieron una pocion ANTES que use el criterio queremos que tome dicha pocion, entonces usamos "tomarPocion pocion" mas el criterio para que todos los elementos de
-- la lista de personas primero tomen la pocion y luego evaluen segun el criterio que reciba (alguna funcion como "sumarNiveles") y "maximoSegun" se encargar de aplicar recursivamente y devolver el mayor segun el criterio
