{- Desde una importante cadena televisiva nos pidieron modelar un programa (cuak) para ver si sus originales ideas tienen sentido. Si bien el formato nos resultó un poco trillado nos dijeron que la idea es totalmente nueva.

Parte A

Nuestro programa tendrá participantes que cuentan con nombre, trucos de cocina y un plato que es su especialidad. Los platos, a su vez, tienen una dificultad que va de 0 a 10 y un conjunto de componentes que nos indican sus ingredientes con sus respectivos pesos en gramos.

Algunos de los trucos más famosos son:

endulzar: dada una cantidad de gramos de azúcar, le agrega ese componente a un plato.  
salar: la vieja y confiable… dada una cantidad de gramos de sal y un plato, nos retorna el mismo con esa cantidad de sal para que quede flama.
darSabor: dadas una cantidad de sal y una de azúcar sala y endulza un plato.
duplicarPorcion: se duplica la cantidad de cada componente de un plato… para más placer.
simplificar: hay platos que son realmente un bardo. Es por ello que si un plato tiene más de 5 componentes y una dificultad mayor a 7 lo vamos a simplificar, sino lo dejamos igual. Simplificar un plato es dejarlo con 5 de dificultad y quitarle aquellos componentes de los que hayamos agregado menos de 10 gramos.

De los platos también nos interesa saber:

esVegano: si no tiene carne, huevos o alimentos lácteos.
esSinTacc: si no tiene harina.
esComplejo: cuando tiene más de 5 componentes y una dificultad mayor a 7.
noAptoHipertension: si tiene más de 2 gramos de sal.

Parte B

En la prueba piloto del programa va a estar participando Pepe Ronccino quien tiene unos trucazos bajo la manga como darle sabor a un plato con 2 gramos de sal y 5 de azúcar, simplificarlo y duplicar su porción. Su especialidad es un plato complejo y no apto para personas hipertensas.
 
Modelar a Pepe y su plato. 

Parte C

¡Ahora sí! Manos a la obra… o manos en la masa… bueno, continuemos. 

Dado que este es un programa de concursos de cocina vamos a tener que modelar tres funcionalidades: 

cocinar: es el momento en el que la magia ocurre y vemos como queda finalmente el plato de un participante luego de aplicar todos sus trucos a su especialidad.
esMejorQue: en esta contienda diremos que un plato es mejor que otro si tiene más dificultad pero la suma de los pesos de sus componentes es menor.
participanteEstrella (este punto es el único en el que pueden usar recursividad si así lo desean): ¡se picó y no estamos hablando de la cebolla! Dada una lista de participantes, diremos que la estrella es quien luego de que todo el grupo cocine tiene el mejor plato.

Parte D

Para finalizar vamos a modelar el plato definitivo, el platinum. Este plato tiene de especial que tiene infinitos componentes misteriosos con cantidades incrementales y dificultad 10:

> componentes platinum
[("Ingrediente 1", 1), ("Ingrediente 2", 2), ("Ingrediente 3", 3),..]

> dificultad platinum
10

Luego de modelar el platinum respondé las siguientes preguntas justificando tus respuestas:
 
¿Qué sucede si aplicamos cada uno de los trucos modelados en la Parte A al platinum?
¿Cuáles de las preguntas de la Parte A (esVegano, esSinTacc, etc.) se pueden responder sobre el platinum? 
¿Se puede saber si el platinum es mejor que otro plato?


Aclaraciones

 Todas las funciones deberán estar tipadas.
 NO repetir lógica.
 Usar composición siempre que sea posible. -}
