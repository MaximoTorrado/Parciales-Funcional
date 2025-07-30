{- Stand App
¿A quién no le gusta jijearse? Este año tenemos la suerte de poder reírnos mientras codeamos… ¡porque vamos a hacer una app de stand up! 🥁
Para ver qué tan bien le irían a los shows de un stand up, nos pidieron tu ayuda por tus conocimientos del paradigma funcional. 🙏

Parte A

Tendremos comediantes de los que conocemos su nombre, su apodo, los años de experiencia en el rubro y su repertorio de chistes.
Sobre cada comediante necesitamos lo siguiente:
tieneCancha: tiene cancha si está hace más de 10 años en el rubro o si su apodo es “Risitas de oro”.
aprenderChiste: cuando aprende un chiste nuevo, éste se suma a su repertorio y aumenta sus años de experiencia en 1.
cancelar: cuando se pasa de la raya repetidas veces, puede terminar siendo cancelado. Cuando esto pasa, sus años de experiencia se reducen a 0, pierde todo su repertorio y su apodo pasa a ser “Cancelator”.

Modelar:
Los 3 items mencionados.
A Pipi, una comediante que apodan “31416”, tiene 42 años de experiencia y sabe un chiste corto, un chiste largo que dura 8 minutos y una anécdota que pasó en Bariloche.

Parte B

+ El público estaba listo.
- ¿Y el chiste?
+ Me lo olvidé en casa.

Pero acá no nos olvidamos que para el show hacen falta un público y chistes. De cada persona del público sabemos el nombre, la edad, el índice de entretenimiento y su tipo de humor. Y de los chistes lo siguiente:
chisteCorto: aumenta el entretenimiento de una persona en 10 y cambia su tipo de humor a “básico”. 
anecdota: es un chiste muuuy largo que sólo entretiene a la persona si tiene humor “paciente”, aumentando su entretenimiento en 2 veces la cantidad de letras que tenga el lugar donde ocurrió la anécdota. En caso contrario, se aburre disminuyendo su entretenimiento a la mitad.
chisteLargo: si el humor de la persona es “básico”, le quita un año de edad según la cantidad de minutos que dure el chiste. Si no, pasa sin pena ni gloria.

Modelar los chistes.


Parte C

Llegó el momento… ¡hora del show! Al darUnShow, un o una comediante le cuenta todo su repertorio de chistes al público.
Es importante saber leer al público que va a presenciar el show de stand up, por lo que nos interesa saber algunas métricas pre y post show:
vaASerUnExito: dado un line up de comediantes y un público, el show va a funcionar si todos los comediantes tienen cancha o es un público experimentado (la suma de todas las edades del público es mayor a 150).
seVanATentar: cuando al menos 3 personas del público tienen humor “fácil”.
registroPostShow: necesitamos saber cómo queda el público después de que un o una comediante da un show. Para mantener los datos anónimos, queremos un registro que sólo tenga el índice de entretenimiento y el tipo de humor de cada persona. 

Modelar lo requerido para poder dar un show y obtener las métricas.

Parte D

Imaginate un público infinito… ¿al menos así habría alguien que se ría de mis chistes? 😭
Respondé y justificá las siguientes preguntas teniendo en cuenta un público infinito:
¿Qué pasaría si queremos saber si el público se va a tentar?
¿Qué pasaría si queremos saber si va a ser un éxito?
¿Qué pasaría si queremos obtener el registro post show?

Aclaraciones
Todas las funciones deberán estar tipadas.
NO repetir lógica.
Usar composición siempre que sea posible.
Poner número y nombre a todas las hojas.
 -}