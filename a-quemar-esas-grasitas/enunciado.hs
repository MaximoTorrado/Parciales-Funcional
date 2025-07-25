{- Se desea desarrollar un sistema para un popular gimnasio que permita calcular el efecto de las rutinas de ejercicios que realizan sus socios.

De cada gimnasta nos interesa saber su peso y su coeficiente de tonificación.

Los profesionales del gimnasio preparan rutinas de ejercicios pensadas para las necesidades de cada gimnasta. Una rutina es una lista de ejercicios que el gimnasta realiza durante unos minutos para quemar calorías y 
tonificar sus músculos.


Se pide:

1. Modelar a los Gimnastas y las operaciones necesarias para hacerlos ganar tonificación y quemar calorías considerando que por cada 500 calorías quemadas se baja 1 kg de peso.
2. Modelar los siguientes ejercicios del gimnasio:
  a. La cinta es una de las máquinas más populares entre los socios que quieren perder peso. Los gimnastas simplemente corren sobre la cinta y queman calorías en función de la velocidad promedio alcanzada (quemando 10 
  calorías por la velocidad promedio por minuto).
  La cinta puede utilizarse para realizar dos ejercicios diferentes:
   i. La caminata es un ejercicio en cinta con velocidad constante de 5 km/h.
   ii. El pique arranca en 20 km/h y cada minuto incrementa la velocidad en 1 km/h, con lo cual la velocidad promedio depende de los minutos de entrenamiento.
  b. Las pesas son el equipo preferido de los que no quieren perder peso, sino ganar musculatura. Una sesión de levantamiento de pesas de más de 10 minutos hace que el gimnasta gane una tonificación equivalente a los kilos 
  levantados. Por otro lado, una sesión de menos de 10 minutos es demasiado corta, y no causa ningún efecto en el gimnasta.
  c. La colina es un ejercicio que consiste en ascender y descender sobre una superficie inclinada y quema 2 calorías por minuto multiplicado por la inclinación con la que se haya montado la superficie.

Los gimnastas más experimentados suelen preferir otra versión de este ejercicio: la montaña, que consiste en 2 colinas sucesivas (asignando a cada una la mitad del tiempo total), donde la segunda colina se configura con 
una inclinación de 5 grados más que la inclinación de la primera. Además de la pérdida de peso por las calorías quemadas en las colinas, la montaña incrementa en 3 unidades la tonificación del gimnasta.

  3. Dado un gimnasta y una Rutina de Ejercicios, representada con la siguiente estructura:

data Rutina = Rutina {
 nombre :: String,
 duracionTotal :: Int,
 ejercicios :: [Ejercicio]
}

 Implementar una función realizarRutina, que dada una rutina y un gimnasta retorna el gimnasta resultante de realizar todos los ejercicios de la rutina, repartiendo el tiempo total de la rutina en partes iguales. Mostrar 
un ejemplo de uso con una rutina que incluya todos los ejercicios del punto anterior.

4. Definir las operaciones necesarias para hacer las siguientes consultas a partir de una lista de rutinas:
  a. ¿Qué cantidad de ejercicios tiene la rutina con más ejercicios?
  b. ¿Cuáles son los nombres de las rutinas que hacen que un gimnasta dado gane tonificación?
  c. ¿Hay alguna rutina peligrosa para cierto gimnasta? Decimos que una rutina es peligrosa para alguien si lo hace perder más de la mitad de su peso. -}