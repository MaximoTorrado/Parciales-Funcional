{- Tenes un perrito y te toca ir a la oficina durante el dia? P de Perritos, la guarderia perruna, abrio sus puertas para que puedas dejar a tu fiel compañero en sus buenas manos
¡Ayudala a dar un excelente servicio con tus conocimientos del paradigma funcional!  -}

{- Hablemos de los protagonitas: los perritos. De cada uno conocemos: su raza, sus juguetes favoritos, el tiempo que va a permanencer en la guarderia, y la energia que tiene   
De las guarderias sabemos que tienen un nombre y una rutina para entretener a los pichichos. La rutina es un conjunto de actividades, compuestas por el ejercicio y el tiempo que 
este dura en minutos. Algunos de estos ejercicios son

jugar: disminuye en 10 unidades la energia del perrito ¡No puede quedar un valor negativo! -}
-- Respuesta: 

--Arrancamos modelando los perritos
data Perro = Perro {
    raza :: String,
    juguetesFavoritos :: [String],
    tiempoDePermanencia :: Int,
    energia :: Int 
} -- deriving Show

{- Tambien las guarderias: Sabemos que la guarderia tiene un campo rutina, que esta compuesta por una lista de actividades, que una actividad esta compuesta por un ejercicio y un 
entero (un tiempo que dura este ejercicio) -}
type Actividad = (Ejercicio, Int)

{- IMPORTANTE!!
Como tenemos una dupla nosotros podemos plantear por si acaso necesitemos ingresar a alguno de sus elementos, "funciones de acceso" o "getters" para las tuplas, en caso de necesitar
alguna de las 2 especifica usaremos estas funciones -}
ejercicio :: Actividad -> Ejercicio
ejercicio = fst

duracionActividad :: Actividad -> Int
duracionActividad = snd

--Luego si miramos los ejercicios (las funciones a definir) basicamente reciben un perro y devuelven un perro que se le simulo un cambio a un campo del mismo, entonces
type Ejercicio = Perro -> Perro

data Guarderia = Guarderia {
    nombre :: String,
    rutina :: [Actividad]
}--  deriving Show


