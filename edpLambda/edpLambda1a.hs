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

data Persona = Persona {
    mail :: Mail, 
    busquedas :: [Busqueda] --Dijimos que Busqueda es una lista de Requisitos, entonces una persona tiene una lista de listas de funciones (Requisito)
}

{---------------------------------------------------------------------------------------------------------------------------------------------------------------------
Ejercicios
----------------------------------------------------------------------------------------------------------------------------------------------------------------------

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