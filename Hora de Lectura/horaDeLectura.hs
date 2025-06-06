{- Leer es una actividad que solemos dejar de lado por el tiempo que lleva hacerlo. Pero es un lindo hábito que está bueno mantener, ¿no? 📖
En PdeP tenemos una biblioteca con nuestros libros favoritos; te contamos algunos 📚. El visitante de Stephen King que tiene 592 páginas, Shingeki no Kyojin
capítulo 1, 3 y 127 que tienen 40 páginas cada uno y los escribió Hajime Isayama, Fundación de Isaac Asimov con 230 páginas, los capítulos  5, 10 y 12 de Sandman
de Neil Gaiman con 35 páginas cada uno y, por último, la saga de Eragon: Eragon (544 páginas), Eldest (704 páginas), Brisignr (700 páginas) y Legado (811 páginas)
de Christopher Paolini. 
¡Un montón! Nos gustaría organizarlos un poco 😅. Estas son cosas que nos gustarían saber:  -}

{- El promedioDeHojas de los libros de nuestra biblioteca. 
Qué lectura es una lecturaObligatoria, esto es así cuando es de Stephen King o de la saga de Eragon o es el ejemplar de Fundación de 230 páginas de Isaac Asimov
(¡ningún otro!).
Si la biblioteca es fantasiosa, es decir, si tiene algún libro de Christopher Paolini o de Neil Gaiman.
El nombreDeLaBiblioteca, que es el nombre de todos los títulos juntos, sacándole las vocales.
Si tenemos una bibliotecaLigera, o sea, si todas sus lecturas tienen 40 páginas o menos. 

Definí las funciones y valores necesarios para poder consultar lo listado anteriormente. Y sí, todas las funciones que crees deberán estar tipadas. ¡Manos al
teclado! 👩‍💻👨‍💻  -} 

{- Respuesta:  -}

{- Notamos del enunciado primero que nada que para poder representar un Libro con lo visto hasta esta clase (Que fueron tuplas, tambien podriamos y creo que esta mas
bueno usar data) necesitamos, 1) Titulo 2) Autor 3) CantidadDePaginas, entonces estas 3 cosas las podremos usar en una TUPLA
Para hacerse aun mas el guapo podriamos usar un type para las 4 cosas asi ya dejamos un type para la tupla que representara un libro, donde dentro tambien sus 
elementos tienen un type para cada parte -}

type Titulo = String
type Autor = String
type CantidadDePaginas = Int
type Libro = (Titulo, Autor, CantidadDePaginas)
 
{- IMPORTANTE!!
Si bien notamos que tenemos casi todo lo que necesitamos de informacion de los libros, no se bien porque en la resolucion opta por tener toda esa informacion en
distintas funciones "etiqueta", especificamente en funciones "etiqueta" pero a partir de la dupla que planteamos que representaria un libr A SIMPLE VISTA PARECIERA QUE 
ES UNA BUENA IMPLEMENTACION SI ESTAMOS TRABAJANDO CON DUPLAS (Mas que nada para lo visual y lo declarativo que otra cosa) -}

{- Ahi confirme, los type alias son por chiche y por mejor visual mas que otra cosa -}

elVisitante :: Libro
elVisitante = ("El Visitante", "Stephen King", 592)

-- Notar que tambien aca es lindo marcar los capitulos, porque en la vida real cada capitulo es un libro 
shingekiNoKyojinC1 :: Libro
shingekiNoKyojinC1 = ("Shingeki no Kyojin 1", "Hajime Isayama", 40)

shingekiNoKyojinC3 :: Libro
shingekiNoKyojinC3 = ("Shingeki no Kyojin 3", "Hajime Isayama", 40)

shingekiNoKyojinC27 :: Libro
shingekiNoKyojinC27 = ("Shingeki no Kyojin 3", "Hajime Isayama", 40)

fundacion :: Libro
fundacion = ("Fundacion", "Isaac Asimov", 230)

sandmanC5 :: Libro
sandman5 = ("sandman5", "Neil Gaiman", 35)

sandmanC10 :: Libro
sandmanC10 = ("sandman10", "Neil Gaiman", 35)

sandmanC12 :: Libro
sandman12 = ("sandman12", "Neil Gaiman", 35)

eragon :: Libro
eragon = ("eragon", "Christopher Paolini", 544)

eldest :: Libro
eldest = ("eldest", "Christopher Paolini", 704)

brisignr :: Libro
brisignr = ("brisignr", "Christopher Paolini", 700)

legado :: Libro
legado = ("legado", "Christopher Paolini", 811)

{- IMPORTANTE!! 
Luego notar que se habla de una biblioteca, osea nosotros tenemos que tener una estructura donde seamos capaces de poner guardar libros, que en si son tuplas (En este
caso) de tipo (String, String Int) Pero como notamos esto ya lo tenemos en un alias, entonces aprovechemos eso y creemos una funcion etiqueta que sera una lista de 
tuplas donde guardara estos datos (Es etiqueta porque es constante)   -}

{- Notar que a lo siguiente esta de diez:
biblioteca :: [Libro]
biblioteca = [elVisitante, shingekiNoKyojinC1, shingekiNoKyojinC3, shingekiNoKyojinC27, fundacion, sandmanC5, sandmanC10, sandmanC12, eragon, eldest, brisignr, legado] 


  -}






















