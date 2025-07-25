module Lib where

-- type Peso = Int
type Tiempo = Int
-- type Grados = Int

-- 1. Modelar a los Gimnastas y las operaciones necesarias para hacerlos ganar tonificación y quemar calorías considerando que por cada 500 calorías quemadas se baja 1 kg de peso.

data Gimnasta = Gimnasta {
  peso :: Int, 
  tonificacion :: Int
} deriving (Show, Eq)

-- usamos la segunda forma de modelar un cambio en un atributo de un data creando una copia con un atributo cambiado, podriamos usar el constructor devolviendo otro data pero deberiamos poner uno por uno todos los atributos
tonificar :: Int -> Gimnasta -> Gimnasta 
tonificar n gimnasta = gimnasta { tonificacion = tonificacion gimnasta + n }

quemarCalorias :: Int -> Gimnasta -> Gimnasta
quemarCalorias kcal gimnasta = gimnasta { peso = peso gimnasta - kcal `div` 500 }


data Rutina = Rutina {
  nombre :: String, 
  duracionTotal :: Tiempo
  -- ejercicios :: [Ejercicio]  esto esta bien pero deberiamos primero plantear los ejercicios del punto 2 para recien ahi plantear un type "Ejercicio"
} deriving (Show, Eq)