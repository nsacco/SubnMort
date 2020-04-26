### Fork de Nico.


########### LOS RESULTADOS Y PLOTS ESTAN EN LA CARPETA ANALYSIS  
########### Asuntos PENDIENTES
### ANALIZAR RESULTADOS. LO DE CONCORDIA ES RELEVANTE
### USAR POBLACION AJUSTADA DE INDEC POR DPTOS, por cambian jerarquias
### TESTEAR HIPOTESIS DE HETEROGENEIDAD A MENOR ESPERANZA DE VIDA



El domingo ganamos! 

Vamos Boca!


This repository provides code and report for the Project "Mortality Estimates for Small Areas in Argentina (1980-2015)", elaborated by Nicolás Sacco, Iván Williams and Bernardo L. Queiroz with the bookdown package. 

Poner wd en la raíz del proyecto setwd("~/Arg_DeptMort")

## Requirements

To set up the software, you will need to install the `bookdown` package and its dependencies as follows:

```r
install.packages('bookdown')
```

You will also need LaTeX installed. If you don't already have LaTeX, one convenient approach is to install it via R:

```r
install.packages('tinytex')
tinytex::install_tinytex()
```

# Arg_DeptMort
# 
# En el reporte general estarán todos los pasos seguidos. Se hará un artículo de acuerdo al formato de revista que elijamos. 
# 
# TO DO LIST
# 
# Entender el código de Iván, organizar carpetas, etc.
# 
# Notas de Iván: Hola Nico, como andaaas
Todavia estoy sin la compu, pero leyendo un poco, y considerando que no hay antecedentes en la temática (por lo menos buscando no encontré), creo que sería bueno hacer el trabajo en dos fases:
1) 
a) Busqueda de estudios de evaluación de cobertura de defunciones en provincias seleccionadas.
b) poblacion (denominador) y defunciones (numerador) por edades, para las tres provincias y sus departamentos. diagnstico de tasas y necesidad de ajuste
c) Evaluación y ajuste de num y denom
c) Estimación de tasas y necesidad de tratamiento estadístico
2)
a) seleccion de metodos de estimación por departamento
b) estimacion e intervalos de confianza
c) tablas de mortalidad y esperanzas de vida 
d) conclusiones sobre heterogeneidad entre departamentos
# 
#  Pero la fase 1 nos va a ayudar a tratar la data y mostrar la necesidad de usar modelos ante tasas que son 0 en muchos grupos de edad por ejemplo.
Al final nos quedamos con Prov de BsAs, Neuquen y Formosa? Lo dejo a tu elección
Creo que podemos arrancar con población y muertes. Si te parece puedo agarrar defunciones que me interesa ver los metodos DDM. 


Ya tengo las defunciones de las tres provincias (DEIS), agrupadas por dpto, edad simple y sexo. Digo que las tengo en R porque las procesé desde la base DEIS, y lo tengo en código (o sea que si cambiamos de idea de las provincias es nada más cambiar la provincia en el código). Los subí al drive.
Si queres hacer población adelante. Como idea se me ocurre tomar las estimaciones INDEC a 1/7/2010, y aplicarle la estructura por edad del Censo (acá hay dos opciones: o usar la estructura del básico (los que durmieron esa noche en el dpto), o la del ampliado (los que residen)). Decime vos que te parece mejor de estas dos opciones, o si se te ocurre otra forma.
Respecto a las correcciones de defunciones, tengo que leer algo de biblio, pero por lo que dijo Gonzaga en clase no hay métodos de corrección a nivel tan bajo. O sea que quizás tenga que usar los de DDM a nivel provincial y distribuir internamente.
Respecto a las correcciones de población por dpto, usando Estimaciones INDEC por ahi se soluciona, pero aplicando estructura censo creo que sería como distribuir la omisión proporcionalmente por edad y sexo. Puede ser algo que aclaremos y listo.# 

Es como nos dijo Marcos: "We cannot apply DDM to small areas in Brazil because several assumptions of the method are not met, especially closed population"
Por lo que veo corrigen para la microrregion (son agrupaciones de municipalidades al parecer), y luego aplican un metodo para distribuir esa omisión de registro de muertes entre las pequeñas áreas.
Para aplicar DDM a las provincias requiero la población en 2001 y 2010 por edades y sexo, y las muertes por edades y sexo en el período intercensal. Ya tengo todo supuestamente. Para entender los métodos estoy usando la web de la iussp. Los de CEDEPLAR tienen un paquete en R que los hace (se llama DDM)
Vos te pones con población? Estoy de acuerdo en usar hecho!

Yo arranque a leer DDM. Esta semana arranco los cálculos. Lo voy a hacer a nivel provincial c las poblaciones 2001 y 2010 conciliadas por indec al 30/6. Después decidimos que hacer hacia abajo. Si distribuir la omisión de .uertes de manera proporcional u otra cosa.

Acordemonos de usar siempre los codigos indec (prov/dpto/partido)
Puse en el drive el archivo de codigos

Mañan lo veo a Carlos para afinar cosas de la tesis.
Ya tengo los insumos (población y defunciones de las tres prov) para aplicar los DDM a nivel provincial: población por grupos quinquenales y sexo en 2001 y 2010 (por proyecciones), y las defunciones entre 2001 y 2010 sexo y edad.

que haces nico
todo ok, yo tambien estuve colgueti
como segui trabajando en mi pc, cree otra carpeta y ahora se me dificulta mas pasar todo que crear una nueva. Ahi te la compartí de vuelta
Ahi tenes las bases 2009 a 2011 de deis, que me las habia pasado Cristina. Esas usé para computar las defunciones
Empece a recopilar algo de poblacion, pero es para revisar
ahi esta el pdf tambien con algunos de los graficos pro dpto


http://bonecave.schmert.net/fit_TOPALS.html

Nicooo, felices pascuas.
Algunas cosas respecto al objetivo 1:
+ Adjunto un diagnóstico de missings de edad y sexo por dpto de las provincias que habíamos hablado: BsAs, Formosa, Neuquen. 
Respecto a edad, el porcentaje nunca es mayor a 5% (y la mayoría es menor a 2%). respecto a sexo, hay casos como el dpto 427 de BsAs que tiene un 24%, lo cual parece mucho como para redistribuir. Este diagnostico quizás lo podemos hacer con todas las prov, como apartado del trabajo.
+ Para estudiar las tasas necesitamos población. En la publicación de población base de dptos de INDEC aparece el mensaje que copio abajo*. Esto nos plantea si elegir otras provincias (en ese caso dejo a tu criterio sociológico cuales).
+ Respecto al análisis de cobertura de defunciones por dpto, sería bueno preguntarle a Bernardo qué hacer: no tenemos un estudio por dpto como ellos, y los DDM me parecen demasiado restrictivos (migracion nula, igual omisión entre censos 2001 y 2010, y redistribución hacia dentro de los dptos ya que los indicadores serían provinciales). Estaría bueno ver qué opina para guiar lo que queda.
¿Qué opinas?
Abrazo





*Nota (octubre 2016): la evaluación de cobertura del Censo Nacional de Población, Hogares y Viviendas 2010 (CENSO 2010), llevada a cabo hasta el momento por la Dirección de Estadísticas Poblacionales y la Dirección Nacional de Estadísticas Sociales y de Población del INDEC, ha detectado registros de población replicados en la base de datos provincial, distribuidos de manera heterogénea según departamento/partido/comuna. A partir de ese análisis se determinó que las estimaciones departamentales de las siguientes provincias deben ser consideradas con reservas: Ciudad Autónoma de Buenos Aires, Provincia de Buenos Aires, Corrientes, Formosa, Jujuy, Mendoza, Salta, Santa Cruz, Santiago del Estero y Tierra del Fuego, Antártida e Islas del Atlántico Sur. 

Ver más de Nicolás Sacco

Hi Nico,

it is a lot of missing information for Buenos Aires. We did not have a problem like that, specially for a area that I would expect much better data. But I have a doubt. Buenos Aires is one province, right? And then you have 427 departments within it? 

I am guessing one could use a larger area and then a empirical bayes model to estimate the departments. It is like a indirect standardization and works ok. And we can expect the larger area to have more robust estimates and to be less sensitive to the model. 

Best

nicooo
BsAs tiene 134 dptos . no se por qué se refiere a 427.
Por ahi nos puede pasar algun paper modelo de aplicación de esa metodología bayesiana.
Puede ser interesante tambien, en vez de tomar la provincia como área mayor, contruir áreas mayores agrupando de a 2 o 3 provincias. Para esto podemos probar algún metodo de segmentación que considere toda la estructura por edades y agrupe según prefiles. Tambien le podemos preguntar sobre esa idea.
Q t parece
Abrazo

Marshall, R. J. (1991). Mapping disease and mortality rates using empirical Bayes estimators. Applied Statistics, 283-294.

Manton, K. G., Woodbury, M. A., Stallard, E., Riggan, W. B., Creason, J. P., & Pellom, A. C. (1989). Empirical Bayes procedures for stabilizing maps of US cancer mortality rates. Journal of the American Statistical Association, 84(407), 637-650.

Assunção, R. M., Schmertmann, C. P., Potter, J. E., & Cavenaghi, S. M. (2005). Empirical Bayes estimation of demographic schedules for small areas. Demography, 42(3), 537-558.

Pinheiro, P. C., & Queiroz, B. L. (2017). Análise espacial da mortalidade e das internações hospitalares por acidentes de motocicleta no Brasil. Anais, 1-26.

Justino, J. R., de Araújo Freire, F. H. M., & Lucio, P. S. (2013). Estimação de sub-registros de óbitos em pequenas áreas com os métodos bayesiano empírico e algoritmo EM. Revista Brasileira de Estudos de População, 29(1), 87-100.

http://schmert.net/topals-mortality/

y con respecto a la nota al pie del INDEC q mencionás, se me ocurre que dividamos en las provincias que no están en esa nota y después agarremos y veamos q pasa. Igual, esa nota no ayuda en nada!

Dale, que dias estas por aca.
Yo estoy terminando de entender el bayesiano empírico, me costó mas de lo que esperaba.
Por ahí podemos hacer esos tres métodos: estand, topals y bayesiano empírico. Habría que pensar con que criterios creerle mas a una q el otro (si dan diferentes)

Ya lo tengo en código al BE, y creo entender una parte importante, aún algunas cosas que se me escapan. Respecto a topals, creo q es más fácil teóricamente y el código esta disponible. vos te ocupás del otro?
Sugiero no distribuir las defunciones de departamento y provincia desconocida, amplificaríamos el error me parece (esto se lo puedo preg a Bernanrdo). Sí las defunciones de sexo y edad desconocida, dentro de cada dpto.
Para seleccionar las provincias q usemos podemos considerar tanto las advertencias de indec, como estimaciones indirectas de omisión (ddm).

Nico, apliqué los tres metodos de DDM a las provincias de Arg usando el paquete.
Despues usé la suavización de Schmertmatn para calcular los expuestos en tres años (y no multiplicar el dato censal por 3), a nivel de dpto.
Después estuve estimando por EB, pero le falta un poco.
Desoes t comento mejor donde esta cada codigo

Buenas tardes, espero que se encuentren bien.
Perdón por el retraso en la comunicación de las primeras estimaciones. 
Estuve tratando de aprender los métodos, y no anduve con mucho tiempo para estudiar.
En las últimas semanas estuve probando dos cuestiones:
1) la estimación de expuestos al riesgo en el período 2009-2011, usando una técnica de Carl Schmertmann, donde usa una tabla modelo para computar años vividos en cada edad simple, a partir de stock censal. Los resultados creo que son buenos, mejora la estimación de exposición respecto a multiplicar por tres el stock censal a cada edad (aún corriendo el censo a fecha media del intervalo). Adjunto unos gráficos.
2) Usando lo anterior y las defunciones por departamento (sin imputar desconocidos de edad y departamento), ensayé la estimación bayesiana usando la aroximación por contracción (Assuncao, 2005) por edades quinquenales. Los resultados en algunos departamentos son buenos, aunque en otros no estoy del todo convencido. Las tasas que son 0 en algunas edades no son lo suficientemente suavizadas por el área mayor (quedan por debajo). Adjunto ejemplos de Catamarca (provincia nro 10), una provincia relativamente pequeña en Argentina. Por ejemplo en el dpto 14, que representa el 0.6% del área mayor. En general las dudas las tengo en las edades tempranas 5-14.
Hasta aquí los comentarios. Ustedes me dirán si quieren que les pase el código y las fuentes, y cómo.
Tengo unas preguntas, por ahí me pueden orientar: 
*¿es posible incorporar intervalos de confianza a esta metodología? Creo que solo de esta manera podríamos ver diferencias significativas del nivel.
*¿Qué otra metodología consideran podemos ir aplicando para conocer mortalidad a nivel de áreas menores, no conociendo el grado de omisión a ese nivel?
Saludos!
Iván

https://biblioteca.ibge.gov.br/index.php/biblioteca-catalogo?view=detalhes&id=2101597
============================================================
Session Info
============================================================  
# R version 3.5.1 (2018-07-02)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 7 x64 (build 7601) Service Pack 1

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252 
[2] LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices
[4] utils     datasets  methods  
[7] base     

loaded via a namespace (and not attached):
 [1] bookdown_0.7    Rcpp_0.12.18   
 [3] digest_0.6.17   rprojroot_1.3-2
 [5] backports_1.1.2 magrittr_1.5   
 [7] evaluate_0.11   stringi_1.1.7  
 [9] rmarkdown_1.10  tools_3.5.1    
[11] stringr_1.3.1   xfun_0.3       
[13] yaml_2.2.0      compiler_3.5.1 
[15] htmltools_0.3.6 knitr_1.20  
