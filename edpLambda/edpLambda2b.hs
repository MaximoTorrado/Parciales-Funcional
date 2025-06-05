type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
    ambientes :: Int,
    superficie :: Int, 
    precio :: Int, 
    barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail, 
    busquedas :: [Busqueda]
}

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [Depto 3 80 7500 "Palermo", Depto 1 45 3500 "Villa Urquiza", Depto 2 50 5000 "Palermo", Depto 1 45 5500 "Recoleta"]

-- 1) a)

mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor funcion parametro parametro2 = funcion parametro > funcion parametro2

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor funcion parametro parametro2 = funcion parametro < funcion parametro2

-- 1) b) 

ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["1", "esteVaASerElTercero", "dos"]

-- 2) a)

ubicadoEn :: [Barrio] -> Depto -> Bool
ubicadoEn barrios departamento = elem (barrio departamento) barrios

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