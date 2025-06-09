{--Parte A

Nuestro programa tendrá participantes que cuentan con nombre, trucos de cocina y 
un plato que es su especialidad. Los platos, a su vez, tienen una dificultad que va de 0 a 10 y 
un conjunto de componentes que nos indican sus ingredientes con sus respectivos pesos en gramos.--}

data Participante = Participante {
    nombre :: String,
    trucos :: [Truco],
    especialidad :: Plato
} deriving (Show)

data Plato = Plato {
    dificultad :: Int,
    componentes :: [Componente]
} deriving (Show)

type Truco = Plato -> Plato
type Componente = (Ingrediente, Peso)
type Ingrediente = String
type Peso = Int

{--endulzar: dada una cantidad de gramos de azúcar, le agrega ese componente a un plato.  
salar: la vieja y confiable… dada una cantidad de gramos de sal y un plato, nos retorna el mismo
 con esa cantidad de sal para que quede flama.
darSabor: dadas una cantidad de sal y una de azúcar sala y endulza un plato.
duplicarPorcion: se duplica la cantidad de cada componente de un plato… para más placer.
simplificar: hay platos que son realmente un bardo. Es por ello que si un plato tiene más 
de 5 componentes y una dificultad mayor a 7 lo vamos a simplificar, sino lo dejamos igual. 
Simplificar un plato es dejarlo con 5 de dificultad y quitarle aquellos componentes de los
que hayamos agregado menos de 10 gramos.--}
    
endulzar :: Int -> Truco
endulzar unosGramos unPlato = agregarComponente "Azucar" unosGramos unPlato

salar :: Int -> Truco
--salar = agregarComponente "Sal"
salar unosGramos unPlato = agregarComponente "Sal" unosGramos unPlato

agregarComponente :: String -> Int -> Plato -> Plato
--agregarComponente unNombre unosGramos = modificarComponentes (\unosComponentes -> (unNombre, unosGramos) : unosComponentes)
agregarComponente unNombre unosGramos unPlato = modificarComponentes ((unNombre, unosGramos) :) unPlato

modificarComponentes :: ([Componente] -> [Componente]) -> Plato -> Plato
modificarComponentes unaFuncion unPlato = unPlato { componentes = unaFuncion . componentes $ unPlato }
--modificarComponentes unaFuncion unPlato = unPlato { componentes =  unaFuncion (componentes  unPlato) }

darSabor :: Int -> Int -> Truco
darSabor unosGramosDeSal unosGramosDeAzucar = endulzar unosGramosDeAzucar . salar unosGramosDeSal
--darSabor unosGramosDeSal unosGramosDeAzucar unPlato = (endulzar unosGramosDeAzucar) . (salar unosGramosDeSal) $ unPlato

duplicarPorcion :: Truco
duplicarPorcion = modificarComponentes $ map duplicarCantidad
--duplicarPorcion unPlato = modificarComponentes (map duplicarCantidad) unPlato

duplicarCantidad :: Componente -> Componente
duplicarCantidad (ingrediente, cantidad) = (ingrediente, cantidad * 2)

{--simplificar: hay platos que son realmente un bardo. Es por ello que si un plato tiene más 
de 5 componentes y una dificultad mayor a 7 lo vamos a simplificar, sino lo dejamos igual. 
Simplificar un plato es dejarlo con 5 de dificultad y quitarle aquellos componentes de los
 que hayamos agregado menos de 10 gramos.--}

simplificar :: Truco
simplificar unPlato
  | esComplejo unPlato = modificarComponentes (filter hayMucho) $ unPlato { dificultad = 5 }
 -- | esComplejo unPlato = unPlato { dificultad = 5, componentes = filter ((>= 10). snd) (componentes unPlato) }
  | otherwise         = unPlato

esComplejo :: Plato -> Bool
esComplejo unPlato = dificultad unPlato > 7 && (not . null . drop 5 . componentes) unPlato
--esComplejo unPlato = dificultad unPlato > 7 && ((> 5) . cantidadDeComponentes) unPlato
--f (g (h x))

cantidadDeComponentes :: Plato -> Int
cantidadDeComponentes unPlato = length . componentes $ unPlato

hayMucho :: Componente -> Bool
hayMucho unComponente = snd unComponente >= 10
--hayMucho (_, unosGramos) = unosGramos >= 10

{--De los platos también nos interesa saber:
esVegano: si no tiene carne, huevos o alimentos lácteos.
esSinTacc: si no tiene harina.
esComplejo: cuando tiene más de 5 componentes y una dificultad mayor a 7.
noAptoHipertension: si tiene más de 2 gramos de sal.--}

esVegano :: Plato -> Bool
esVegano unPlato = not . any esProductoAnimal . componentes $ unPlato

esProductoAnimal :: Componente -> Bool
esProductoAnimal (ingrediente, _) = elem ingrediente productosAnimales

productosAnimales :: [Ingrediente]
productosAnimales = ["Leche", "Carne", "Huevo", "Manteca"]

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not . tiene "Harina" $ unPlato

tiene :: Ingrediente -> Plato -> Bool
tiene unIngrediente unPlato = elem unIngrediente . ingredientes $ unPlato

ingredientes :: Plato -> [Ingrediente]
ingredientes unPlato = map fst . componentes $ unPlato

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = tiene "Sal" unPlato && cantidadDe "Sal" unPlato > 2

cantidadDe :: Ingrediente -> Plato -> Int
cantidadDe unIngrediente unPlato = cantidad . conseguirComponente unIngrediente $ unPlato

cantidad :: Componente -> Int
cantidad unComponente = snd unComponente

conseguirComponente :: Ingrediente -> Plato -> Componente
conseguirComponente unIngrediente unPlato = head . filter (esDe unIngrediente) . componentes $ unPlato

esDe :: Ingrediente -> Componente -> Bool
esDe unIngrediente (ingredienteDelComponente, _) = unIngrediente == ingredienteDelComponente

{--Parte B
En la prueba piloto del programa va a estar participando Pepe Ronccino 
quien tiene unos trucazos bajo la manga como darle sabor a un plato con 2 gramos de sal y 5 de azúcar, 
simplificarlo y duplicar su porción. Su especialidad es un plato complejo y no apto para personas hipertensas.
Modelar a Pepe y su plato. --}

pepeRonccino :: Participante
pepeRonccino = Participante "Pepe Ronccino" [darSabor 2 5, simplificar, duplicarPorcion] unPlatoComplejo

unPlatoComplejo :: Plato
unPlatoComplejo = Plato 10 [("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100)]

{--Parte C 
cocinar: es el momento en el que la magia ocurre y vemos como queda finalmente el plato 
 de un participante luego de aplicar todos sus trucos a su especialidad. 
esMejorQue: en esta contienda diremos que un plato es mejor que otro si tiene más dificultad 
pero la suma de los pesos de sus componentes es menor.--}
cocinar :: Participante -> Plato
cocinar unParticipante = aplicarTrucos (trucos unParticipante) (especialidad unParticipante)

-- foldl unaFuncion unaSemilla unaLista
-- foldr
aplicarTrucos :: [Truco] -> Plato -> Plato
aplicarTrucos unosTrucos unPlato = foldr aplicarTruco unPlato unosTrucos
--aplicarTrucos unosTrucos unPlato = foldr ($) unPlato unosTrucos
--aplicarTrucos unosTrucos unPlato = foldr (.) id unosTrucos $ unPlato

aplicarTruco :: Truco -> Plato -> Plato
aplicarTruco unTruco unPlato = unTruco unPlato

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = esMasDificil unPlato otroPlato && esMasLigero unPlato otroPlato

esMasDificil :: Plato -> Plato -> Bool
esMasDificil unPlato otroPlato = dificultad unPlato > dificultad otroPlato

esMasLigero :: Plato -> Plato -> Bool
esMasLigero unPlato otroPlato = peso unPlato < peso otroPlato

peso :: Plato -> Int
peso unPlato = sum . map cantidad . componentes $ unPlato

{--participanteEstrella  (usar recursividad si así lo desean): Dada una lista de participantes, diremos que la estrella 
es quien luego de que todo el grupo cocine tiene el mejor plato.--}

participanteEstrella :: [Participante] -> Participante
participanteEstrella unosParticipantes = foldr1 mejorParticipante unosParticipantes
--foldr1 unaFuncion (cabeza : cola) = foldr unaFuncion cabeza cola
--participanteEstrellaFoldr (p:ps) = foldr mejorParticipante p ps

foldr :: (a -> b -> b) -> b -> [a] -> b  necesita valor inicial o semilla o acumulador
foldr f z [x1, x2, x3] = f x1 (f x2 (f x3 z)) 
el foldr1 no necesita valor inicial, toma el primer elemento de la lista como semilla
--foldr1 f [x1, x2, x3] = f x1 (f x2 x3)

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante unParticipante otroParticipante
  | esMejorQue (cocinar unParticipante) (cocinar otroParticipante) = unParticipante
  | otherwise                                                      = otroParticipante

participanteEstrella' :: [Participante] -> Participante
participanteEstrella' [unParticipante] = unParticipante
--participanteEstrella' [unParticipante, otroParticipante] = mejorParticipante unParticipante otroParticipante
--caso especial si son 2 en la lista, ya lo cubre el caso recursivo, no darle bola
participanteEstrella' (unParticipante : otrosParticipantes) =
  mejorParticipante unParticipante (participanteEstrella' otrosParticipantes)

{-- Parte D 

Para finalizar vamos a modelar el plato definitivo, el platinum. Este plato tiene de especial
que tiene infinitos componentes misteriosos con cantidades incrementales y dificultad 10:
> componentes platinum
[("Ingrediente 1", 1), ("Ingrediente 2", 2), ("Ingrediente 3", 3),..]
> dificultad platinum
10
Luego de modelar el platinum respondé las siguientes preguntas justificando tus respuestas:
¿Qué sucede si aplicamos cada uno de los trucos modelados en la Parte A al platinum?
¿Cuáles de las preguntas de la Parte A (esVegano, esSinTacc, etc.) se pueden responder sobre el platinum? 
¿Se puede saber si el platinum es mejor que otro plato?
--}

platinum :: Plato
platinum = Plato 10 unaListaDecomponentesRara

unaListaDecomponentesRara :: [Componente]
unaListaDecomponentesRara =
  map (\unNumero -> ("Ingrediente " ++ show unNumero, unNumero)) [1..]
{--otra forma
platinum' :: Plato
platinum' = Plato {
    dificultad = 10,
    componentes = listaDecomponentes
}
listaDecomponentes :: [Componente]
listaDecomponentes = zip ingredientesMisteriosos [1..]

ingredientesMisteriosos :: [Ingrediente]   
ingredientesMisteriosos = map (("Ingrediente " ++) . show) [1..] --}