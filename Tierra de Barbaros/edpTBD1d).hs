type Objeto = Barbaro -> Barbaro  -- Este type representa una funcion que recibe un barbaro y devuelve otro barbaro

data Barbaro = Barbaro {
    nombreDeBarbaro :: String,
    fuerzaDeBarbaro :: Int,
    habilidadesDeBarbaro :: [String],
    objetosDeBarbaro :: [Objeto]         -- Entonces aca sera una lista de funciones que van de barbaro a barbaro
} -- deriving Show

-- Accesors "De cambio"
mapNombreDeBarbaro :: (String -> String) -> Barbaro -> Barbaro
mapNombreDeBarbaro funcionModificacion unBarbaro = unBarbaro { nombreDeBarbaro = (funcionModificacion . nombreDeBarbaro) unBarbaro }

mapFuerzaDeBarbaro :: (Int -> Int) -> Barbaro -> Barbaro
mapFuerzaDeBarbaro funcionModificacion unBarbaro = unBarbaro { fuerzaDeBarbaro = (funcionModificacion . fuerzaDeBarbaro) unBarbaro }

mapHabilidadDeBarbaro :: ([String] -> [String]) -> Barbaro -> Barbaro
mapHabilidadDeBarbaro funcionModificacion unBarbaro = unBarbaro { habilidadesDeBarbaro = (funcionModificacion . habilidadesDeBarbaro) unBarbaro }

mapObjetosDeBarbaro :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
mapObjetosDeBarbaro funcionModificacion unBarbaro = unBarbaro { objetosDeBarbaro = (funcionModificacion . objetosDeBarbaro) unBarbaro }

--a) Las espadas aumentan la fuerza de los barbaros en 2 unidades por cada kilogramo de peso
espadas :: Int -> Objeto
espadas pesoDeEspada unBarbaro = mapFuerzaDeBarbaro ((+) (pesoDeEspada * 2)) unBarbaro 

--b) Los "amuletosMisticos" puerco-marranos otorgan una habilidad dada a un barbaro
amuletosMisticos :: String -> Objeto
amuletosMisticos unaHabilidad unBarbaro = mapHabilidadDeBarbaro (++ [unaHabilidad]) unBarbaro

--c) Las "varitasDefectuosas" añaden la habilidad de hacer magia, pero desaparecen todos los demas objetos del barbaro 
varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = (mapHabilidadDeBarbaro (++ ["Hacer Magia"]) . mapObjetosDeBarbaro (const [])) unBarbaro

varitasDefectuosas' :: Objeto
varitasDefectuosas' unBarbaro = unBarbaro { habilidadesDeBarbaro = habilidadesDeBarbaro unBarbaro ++ ["Hacer Magia"], objetosDeBarbaro = [] }

varitasDefectuosas'' :: Objeto
varitasDefectuosas'' unBarbaro = (mapHabilidades (++ ["Hacer Magia"]) . desaparecerObjetos) unBarbaro

desaparecerObjetos :: Objeto
desaparecerObjetos unBarbaro = unBarbaro { objetosDeBarbaro = [varitaDefectuosa] }

--d) Una ardilla, que no hace nada

-- Respuesta:

{- Basicamente sigue siendo una funcion pero al no hacer nada podemos modelarla con la funcion "id" -}

ardilla :: Objeto
ardilla = id