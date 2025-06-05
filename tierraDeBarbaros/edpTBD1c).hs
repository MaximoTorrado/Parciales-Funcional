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

-- c) Las "varitasDefectuosas" añaden la habilidad de hacer magia, pero desaparecen todos los demas objetos del barbaro 

-- Respuesta:

{- Basicamente tenemos que hacer 2 cosas:

1) Lo mismo que la funcion anterior que es agregar al final de una lista de habilidades (una lista de String) una habilidad (un String) pero que es constante, 
siempre sera el String "Hacer Magia", podemos usar la misma logica que la funcion anterior donde le pasamos a "mapHabilidades" la funcion "++" para concatenar al
final, y le pasamos una lista ya cargada con el String, osea ++ ["Hacer Magia"], esto seria de forma constante tambien podriamos usar el mismo codigo y el String 
"Hacer Magia" pasarselo cuando llamemos a la funcion (Pero estariamos repitiendo logica)

De aca ya notamos entonces que el tipado va a ser de tipo "Objeto" porque solo necesitamos el barbaro y devolvemos otro barbaro, no necesitamos que nos pasen el 
String "Hacer Magia" porque estariamos repitiendo logica de la funcion anterior

2) Hacer "desaparecer" la lista de objetos (lista de funciones) de dicho barbaro, para eso podriamos hacer una funcion (Para luego componer con la de arriba) que :

  a) Reciba un barbaro y esta usando la funcion "const" reciba parcialmente la lista vacia "[]" mas la lista de objetos (que obtendriamos llamando a la funcion de
  acceso "objetos") y asi haciendo que dicha lista se le asigne la vacia

  b) Interpretando un poco mas el enunciado y pensando que llamar esta funcion "varitasDefectuosas" significa al mismo tiempo asignar al campo "objetos" del barbaro
  ESTA FUNCION SOLAMENTE asi SOLO DEJAMOS CON ESTA FUNCION y las anteriores las obviamos      -}

{- ------------------------------------------------------------------------------------------------------------------------------------------------------
   INTENTOS:

   varitasDefectuosas :: Objeto
   varitasDefectuosas unBarbaro = (mapHabilidadesDeBarbaro (++ ["Hacer Magia"]) . desaparecerObjetos) unBarbaro

   desaparecerObjetos :: Barbaro -> [Objeto]
   desaparecerObjetos unBarbaro = (const [] . objetosDeBarbaro) unBarbaro
   
   Este intento estaba bueno pero el tema con "desaparecerObjetos" es que termina devolviendo una lista de Objetos, y la composicion con "mapHabilidades
   DeBarbaro" quiere recibir un barbaro luego de recibir una funcion [String -> String] que ya esta aplicada con (++ ["Hacer Magia"]) 
-----------------------------------------------------------------------------------------------------------------------------------------------------}
-- a) 

{- Esta funcion esta muy buena porque "mapObjetosDeBarbaro" le aplicamos parcialmente "const []" que esta esta esperando otra lista para asignarle [] a la segunda
lista, entonces recibe un barbaro y en la definicion de "mapObjetosDeBarbaro" se llama a la funcion de acceso "objetosDeBarbaro" y entonces se reduce a la lista de
objetos del barbaro, que despues con "const []" que es la "funcionModificacion" le asigna []

Luego como sabemos "mapObjetosDeBarbaro" devuelve un barbaro con el campo "objetos" modificado, entonces este lo recibe "mapHabilidadDeBarbaro" para que con lo 
aplicado parcialmente, osea (++ ["Hacer Magia"]) pueda generar ese cambio ahora en el campo "habilidadesDeBarbaro"  mas el cambio anterior que le hizo 
"mapObjetosDeBarbaro" 

Y asi con esta composicion agregamos una habilidad y hacemos que la lista de objetos sea la vacia -}

varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = (mapHabilidadDeBarbaro (++ ["Hacer Magia"]) . mapObjetosDeBarbaro (const [])) unBarbaro

{- Una alternativa, pero que no deberiamos tener en cuenta porque si no no estariamos dandole uso a las funciones "mapX" seria directamente generando dichos cambios
mediante la sintaxis de simular cambios con "Record Syntax", pero haciendo esto seguramente repitamos logica mas adelante  -}

varitasDefectuosas' :: Objeto
varitasDefectuosas' unBarbaro = unBarbaro { habilidadesDeBarbaro = habilidadesDeBarbaro unBarbaro ++ ["Hacer Magia"], objetosDeBarbaro = [] }

--b) 

{- Despues ya por ultimo esta opcion seria interpretar que llamar a esta funcion "varitasDefectuosas" para un barbaro, le significa ademas de agregar al final de la
lista de habilidades una habilidad, SUPLANTAR TODOS LOS OBJETOS POR ESTE MISMO OBJETO, obviando todos los demas -}

varitasDefectuosas'' :: Objeto
varitasDefectuosas'' unBarbaro = (mapHabilidades (++ ["Hacer Magia"]) . desaparecerObjetos) unBarbaro

{- Aca directamente esta funcion, que recibe un barbaro y devuelve otro barbaro pero con la lista de objetos vacia, lo hacemos directamente usando la sintaxis de
simular un cambio en el campo de un data pero con "Record Syntax", para que luego lo compongamos y lo reciba "mapHabilidadeDeBarbaro", esta es la solucion que 
propone el video-}

desaparecerObjetos :: Objeto
desaparecerObjetos unBarbaro = unBarbaro { objetosDeBarbaro = [varitaDefectuosa] }



