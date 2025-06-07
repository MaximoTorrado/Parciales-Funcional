{-Los vigilantes (watchmen en inglés) son personas que luchan por la justicia en la ciudad de NiuShork. Cada uno tiene un set de habilidades que le sirven para realizar sus tareas de
 protectores. 

algunosVigilantes = [ ("El Comediante", ["Fuerza"], 1942), ("Buho Nocturno", ["Lucha", "Ingenierismo"], 1963), ("Rorschach", ["Perseverancia", "Deduccion", "Sigilo"], 1964), 
                    ("Espectro de Seda", ["Lucha", "Sigilo", "Fuerza"], 1962), ("Ozimandias", ["Inteligencia", "Más Inteligencia Aún"], 1968), 
                    ("Buho Nocturno", ["Lucha", "Inteligencia", "Fuerza"], 1939), ("Espectro de Seda", ["Lucha", "Sigilo"], 1940)]

Existen héroes sucesores (es por eso que aparecen dos héroes con el mismo nombre). El año indica el momento de aparición del héroe.

Ahhhh ojalá todo fuera tan sencillo.... La gente no siempre está de acuerdo con que estos enmascarados justicieros sirvan de protectores para ciudad. En las aventuras que viven los vigilantes suceden diferentes eventos que afectan a nuestros marginados héroes. Un evento siempre le ocurre a un conjunto de vigilantes, y este evento afecta de diversas maneras a dicho conjunto.

Eventos:
a) destrucción de NiuShork: muere Rorschach y se retira el Dr Manhattan
b) muerte de un vigilante (es un "retiro" forzoso, digamos... Deja de pertenecer a la agrupación) (mueren/se retiran todos los que se llamen de igual manera)
c) guerra de Vietnam: a los vigilantes que además son agentes del gobierno les agrega Cinismo como habilidad, a los restantes no.       -}
-- Respuesta

type Nombre = String
type Habilidad = String
type Año = Int
type Vigilante = (Nombre, [Habilidad], Año)

-- getters
nombre :: Vigilante -> Nombre
nombre = fst 

habilidad :: Vigilante -> [Habilidad]
habilidad = snd 

año :: Vigilante -> Año
año = trd 

type Evento = [Vigilante] -> [Vigilante]

muerteORetiro :: Nombre -> Evento
muerteORetiro unNombre unosVigilantes = filter (not . (== unNombre) . nombre) unosVigilantes      

-- Otra alternativa al not
muerteORetiro' :: Nombre -> Evento
muerteORetiro' unNombre unosVigilantes = filter ((/= unNombre) . nombre) unosVigilantes  

destruccionNiuShork :: Evento 
destruccionNiuShork unosVigilantes = (muerteORetiro "Dr Manhattan" . muerteORetiro "Rorschach") unosVigilantes


{- c) guerra de Vietnam: a los vigilantes que además son agentes del gobierno les agrega Cinismo como habilidad, a los restantes no.  Además, se conocen los agentes de gobierno:
agentesDelGobierno = [("Jack Bauer","24"), ("El Comediante", "Watchmen"), ("Dr. Manhattan", "Watchmen"), ("Liam Neeson", "Taken")] -}

-- Respuesta: 

type Agente = (String, String)

nombreAgente :: Agente -> Nombre 
nombreAgente = fst

type AgentesDeGobierno = [Agente]

-- No devuelve una lista de agentes porque se le agrega una habilidad, y eso es un campo de las duplas de un vigilante y no de un agente de gobierno
guerraDeVietnam :: AgentesDeGobierno -> Evento






























