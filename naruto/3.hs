{- Una persona muy muy muy fanática de esta gran historia, nos pidió crear el mundo de Naruto Uzumaki en el Paradigma Funcional. Así que, por favor, aplica tus conocimientos y ¡concedele el deseo! 🙏

Parte 1
En este mundo conocemos el nombre, las herramientas, los jutsus y el rango de cada ninja. El rango es un número que comienza en 0 y no puede ser negativo. Las herramientas ninjas son de mucha ayuda para realizar misiones. 
De cada una conocemos el nombre y la cantidad disponible. Algunos ejemplos son: bombas de humo, kunais, shurikens y sellos explosivos. Para poder utilizarlas se pide modelar: -}

data Ninja = Ninja {
  nombre :: String, 
  herramientas :: [Herramienta], 
  jutsus :: [Jutsu],  -- jutsus :: [Jutsu],       nosotros aca planteamos el atributo jutsus pero todavia no sabemos que son hasta que los modelemos
  rango :: Int
} -- deriving Show

type Herramienta = (String, Int)

mapRango :: (Int -> Int) -> Ninja -> Ninja
mapRango f ninja = ninja { rango = max 0 . f . rango $ ninja }

mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramientas f ninja = ninja { herramientas = f . herramientas $ ninja }

mapCantidad :: (Int -> Int) -> Herramienta -> Herramienta 
mapCantidad f (nombre, cantidad) = (nombre, f cantidad)

-- a. obtenerHerramienta: cada ninja debe poder obtener una cantidad específica de una herramienta en particular teniendo en cuenta que:
-- i. si la suma de todas sus herramientas más la cantidad a obtener es menor o igual a 100, puede hacerlo sin problemas;
-- ii. en caso contrario, obtiene la cantidad que pueda sin excederse de 100 herramientas.  
obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta herramienta ninja = mapHerramientas((mapCantidad . min . cuantasHerramientasPuedeObtener) ninja herramienta :) ninja

cuantasHerramientasPuedeObtener :: Ninja -> Int
cuantasHerramientasPuedeObtener ninja = (100 -) . cantidadDeHerramientas $ ninja

cantidadDeHerramientas :: Ninja -> Int
cantidadDeHerramientas ninja = sum . map snd . herramientas $ ninja

-- b. usarHerramienta: cuando un ninja usa alguna de sus herramientas no mide cuántas utiliza, por lo que se queda sin ella y no debe figurar más entre sus pertenencias.
usarHerramienta :: String -> Ninja -> Ninja
usarHerramienta otroNombreHerramienta ninja = mapHerramientas (filter ((/=) otroNombreHerramienta . nombreHerramienta)) $ ninja

nombreHerramienta :: Herramienta -> String
nombreHerramienta = fst

{- Parte 2
En su vida cotidiana, cada ninja tiene que cumplir misiones. Por suerte no es un trabajo solitario, ¡trabajan en equipos! De cada misión se especifica qué cantidad de ninjas requiere, el rango recomendable para realizarla, qué
ninjas enemigos hay que derrotar y la herramienta (¡obviamente con su cantidad!) de recompensa en caso de cumplirla con éxito. Se pide modelar: -}

data Mision = Mision {
  cantidadDeNinjas :: Int,  
  rangoRecomendado :: Int, 
  ninjasEnemigos :: [Ninja],   
  recompensa :: Herramienta
} -- deriving Show


-- a. esDesafiante: dado un equipo de ninjas, una misión es desafiante cuando al menos alguien del equipo tiene menor rango que el recomendado y hay que derrotar al menos 2 enemigos.
esDesafiante :: [Ninja] -> Mision -> Bool
esDesafiante ninjas mision = any (not . estaCalificado mision) ninjas && ((>=2) . length . ninjasEnemigos) mision

estaCalificado :: Mision -> Ninja -> Bool
estaCalificado mision ninja = rango ninja >= rangoRecomendado mision

-- b. esCopada: esto pasa cuando la recompensa de la misión son 3 bombas de humo, 5 shurikens o 14 kunais.
esCopada :: Mision -> Bool
esCopada mision = esCopadita (recompensa mision)

esCopadita :: Herramienta -> Bool
esCopadita ("bombas de humo", 3) = True
esCopadita ("shuriken", 5) = True
esCopadita ("kunai", 14) = True
esCopadita _ = False

esCopada' :: Mision -> Bool
esCopada' mision = esCopadita' (recompensa mision)

recoveco :: [Herramienta]
recoveco = [("bombas de humo", 3), ("shuriken", 5), ("kunai", 14)]

esCopadita' :: Herramienta -> Bool
esCopadita' herramienta = elem herramienta recoveco

-- c. esFactible: para que una misión sea factible no tiene que ser desafiante y además el grupo debe contar con la cantidad de ninjas necesaria o la suma total de herramientas del equipo debe ser superior a 500.
esFactible :: [Ninja] -> Mision -> Bool
esFactible ninjas mision = (not . esDesafiante ninjas) mision && ((>=) (cantidadDeNinjas mision) . length) ninjas || sumaHerramientasMayorA500 ninjas                 

sumaHerramientasMayorA500 :: [Ninja] -> Bool
sumaHerramientasMayorA500 ninjas = ((>= 500) . sum . map cantidadDeHerramientas) ninjas

-- Las misiones se pueden completar con éxito o no:
-- d. fallarMision: la vida no siempre es fácil... ni en nuestro mundo ni en el mundo ninja. Cuando una misión falla sólo quedan en el equipo quienes tengan el rango recomendado o superior. Quienes queden sufrirán la vergüenza de ver
-- su rango disminuido en 2 unidades. ¡Por el resto del equipo no te preocupes! Te prometemos que están bien. 😝
fallarMision :: [Ninja] -> Mision -> [Ninja]
fallarMision ninjas mision = (sacar2Rangos . filter ( (>=) (rangoRecomendado mision) . rango)) ninjas 

sacar2Rangos :: [Ninja] -> [Ninja]
sacar2Rangos ninjas = map (mapRango (subtract 2)) ninjas 

-- e. cumplirMision: si todo sale bien, se promociona de rango a cada miembro del equipo. Además obtendrán la recompensa teniendo en cuenta la restricción del máximo de herramientas.
cumplirMision :: [Ninja] -> Mision -> [Ninja]
cumplirMision ninjas mision = map (obtenerHerramienta (recompensa mision) . promover) ninjas  -- mapRango (+1)

promover :: Ninja -> Ninja
promover ninja = mapRango (+1) ninja

-- ¡Todavía no hablamos de los jutsus! Técnicas especiales que nacen de la energía interior de cada ninja. Es como un superpoder que hace que las misiones sean más simples 😅.
-- Algunas de las que conocemos son:
-- f. clonesDeSombra: reduce la cantidad de ninjas que se necesitan para una misión en el mismo número que los clones de sombra creados. ¡El tamaño del equipo no puede ser menor a 1!
clonesDeSombra :: Int -> Mision -> Mision
clonesDeSombra clones mision = mision { cantidadDeNinjas = max 1 (cantidadDeNinjas mision - clones) }

mapCantidadDeNinjas :: (Int -> Int) -> Mision -> Mision
mapCantidadDeNinjas f mision = mision { cantidadDeNinjas = f . cantidadDeNinjas $ mision }

clonesDeSombra' :: Int -> Jutsu -- Mision -> Mision
clonesDeSombra' clones mision = mapCantidadDeNinjas (subtract clones) mision

fuerzaDeUnCentenar :: Jutsu -- Mision -> Mision
fuerzaDeUnCentenar mision = mision { ninjasEnemigos = filter ((<5) . rango) (ninjasEnemigos mision) }


mapNinjasEnemigos :: ([Ninja] -> [Ninja]) -> Mision -> Mision
mapNinjasEnemigos f mision = mision { ninjasEnemigos = f . ninjasEnemigos $ mision }

fuerzaDeUnCentenar' :: Mision -> Mision
fuerzaDeUnCentenar' mision = mapNinjasEnemigos (filter ((<5) . rango)) mision


-- notamos ahora lo que tiene en comun todos los Jutsus
type Jutsu = Mision -> Mision

{- Parte 2

Existe la Gran Guerra Ninja, una misión de rango 100 que necesita al menos 100000 ninjas para completarse, tiene infinitos enemigos y su recompensa es el abanico de Madara Uchiha.
Se pide modelar la granGuerraNinja sabiendo, además, que tiene infinitos villanos y son Zetsu 1, Zetsu 2, Zetsu 3... Zetsu N, el rango de todos es de 600 y no tienen jutsus ni herramientas. -}

granGuerraNinja :: Mision
granGuerraNinja = Mision { 
  cantidadDeNinjas = 100000,
  rangoRecomendado = 100, 
  ninjasEnemigos = infinitosZetsus,
  recompensa = ("Abanico", 1)
}

-- si queremos hacer un "Zetsu 1, Zetsu 2, Zetsu 3... Zetsu N"
infinitosZetsus :: [Ninja]
infinitosZetsus = map zetsu [1..]

zetsu :: Int -> Ninja
zetsu n = Ninja {
  nombre = "Zetsu" ++ show n,
  rango = 600, 
  jutsus = [],
  herramientas = []
} 


-- Sabiendo esto y teniendo en cuenta un equipo de ninjas finitos, responder qué devuelve y por qué en las siguientes funciones: 
-- a. esDesafiante
-- b. esCopada
-- c. fuerzaDeUnCentenar       



