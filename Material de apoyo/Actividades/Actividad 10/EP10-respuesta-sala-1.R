library(ggpubr)
library(pwr)
library(ggplot2)
library(tidyverse)

#===================
#==== PREGUNA 1 ====
#===================
#En trabajo de t�tulo de un estudiante del DIINF, se reportan los siguientes tiempos de
#ejecuci�n ('Tpo' en milisegundos) medidos para dos versiones de un algoritmo gen�tico 
#(A6 y B12) para resolver instancias del problema del vendedor viajero disponibles en 
#repositorios p�blicos. �Es uno de los algoritmos m�s r�pido que el otro?

# informaci�n dada por enunciado
texto <- ("
Instancia 'Tpo A6' 'Tpo B12'
'rat575' 33349 32444
'u724' 55026 64019
'd657' 43352 52696
'rat783' 65076 76857
'u574' 112326 123456
'pr1002' 136262 162808
'fl1577' 3234574 3192222
'nrw1379' 335608 393213
'd1291' 268964 335566
'u1432' 398653 472597
'pcb1173' 303634 234658
'fl1400' 337977 430748
'u2152' 3073534 3253423
'rl1323' 243679 132654
'rl1304' 342321 231254
'u1817' 876432 672542
'vm1084' 413672 543215
'rl1889' 1876432 854213
'pr2392' 6764986 8765321
'u1060' 3453176 432876
")
datos <- read.table(textConnection(texto), header = TRUE)

# Ya con los datos leidos anteriormente, se procede a discutir que prueba
# param�trica corresponder�a en este caso. La prueba m�s acorde para este
# problema es la "prueba Anova de una v�a para muestras correlacionadas", 
# especificamente en un escenario con dise�o de medidas repetidas, ya que
# a una observaci�n se le toman medidas de diferentes formas, es decir que
# se estan eligiendo diferentes instacias para dos versiones del
# algoritmo.

# Posterior a esto, se utlizar� el siguiente procedimiento no param�trico:
# "Prueba de rangos con signo de Wilcoxon ", pues se tienen muestras pareadas,
# es decir a trav�s de una misma instancia se est�n analizando dos versiones
# del algoritmo (versi�n Tpo.A6 y tpo.B12)

# Sin embargo, para poder utilizar esta prueba es necesario llevar a cabo la
# verificaci�n de una serie de condiciones:


# CONDICIONES:

#1) Los pares de observaciones son independientes.
#   verificaci�n: Dado que cada instacia obtenida del algoritmo gen�tico es
#                 distinta a todas las dem�s, es razanoble concluir la 
#                 independencia entre cada una.
#                

#2) La escala de medici�n empleada para las mediciones es intr�nsicamente continua.
#   verificaci�n: Dado que se est� hablando de tiempo, es l�gico suponer que se 
#                 est� hablando de una escala de medici�n continua.

#3) La escala de medici�n empleada para ambas muestras debe ser a lo menos ordinal.
#   verificaci�n: Cuando se est� tratando con una escala de medici�n correspondiente
#                 al tiempo, es natural evaluarlo con palabras, mayor que, menor que
#                 igual que, etc. Por lo que la escala de medici�n utilizada en este
#                 problema es ordinal definitivamente.

# Ya con las condiciones verificadas, se procede a definir las hip�tsis a contrastar:

# Hip�tesis nula (H0): Los dos versiones del algoritmo son igual de r�pidas.
# Hip�tesis alternativa (HA): la versi�n A6 es m�s r�pida que la versi�n B12.
               

# Ya con las hip�tesis descritas y con las condiciones verificadas, se efectua 
# la prueba correspondiente:

# nivel de significancia
alpha <- 0.05

# ejecuci�n de la prueba
pruebaWilcoxon <- wilcox.test(datos[["Tpo.A6"]],
                              datos[["Tpo.B12"]], 
                              alternative = "less",
                              paired = TRUE,
                              config.level = 1 - alpha)
print(pruebaWilcoxon)

#RESPUESTA PREGUNTA 1
# Dado que el valor de p-value (0.4347) es mayor al nivel de significancia establecido
# (0.05), se falla en rechazar la hip�tesis nula. Por lo tanto, se puede concluir
# con 95% de confianza que las dos versiones del algoritmo son igual de r�pidas. Luego,
# no hay un algoritmo m�s r�pido que el otro.


#===================
#==== PREGUNA 2 ====
#===================
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en
# donde un estudio o experimento, relacionado con el alza que han experimentado las tasas de inter�s de
# los cr�ditos en Chile, necesite utilizar una prueba de suma de rangos de Wilcoxon (tambi�n llamada
# prueba de Mann-Whitney-Wilcoxon o prueba U de Mann-Whitney), debido a problemas con la escala de
# la variable dependiente en estudio. Indiqu� cu�les ser�an las variables/niveles involucrados en su ejemplo
# y las hip�tesis nula y alternativa a contrastar.

#RESPUESTA PREGUNTA 2
# Con las recientes alzas que han experimentado las tasas de inter�s de los cr�tidos a nivel nacional, 
# provocando as� que el costo de endeudamiento aumente, lo qu provoca una baja en el consumo que a su vez
# repercute en la inflaci�n. 
# En base a esto, la empresa JoseLuis LTDA. desea evaluar el consumo actual de las personas para dos 
# grandes cadenas de servicios al consumidor: WallMark y SudCenco, para ello la empresa ha seleccionado 
# al azar a 33 personas, quienes fueron asignados de manera aleatoria a evaluar 9 aspectos de alguna de estas dos empresas.
# Cada persona eval�a el cambio en el consumo de productos de alguna de las empresas en una escala de likert de 
# siete niveles, donde 1 corresponde a "Muy poco" (respecto al consumo antes del alza) y 7 a "Muy alto" (respecto
# al consumo antes del alza).

#Se obtuvieron los siguientes resultados (promedio de respuestas):
#
#                 WallMark    SudCenco
#                   2.7         3.4
#                   3.1         4.6
#                     .           .
#                     .           .
#                     .           .
#         Media     3.48        4.73

#Hip�tesis a contrastar:
#H0: no existe diferencia entre el consumo hacia ambas empresas (respecto al consumo antes del alza).
#HA: si existe diferencia entre el consumo hacia ambas empresas (respecto al consumo antes del alza).

#===================
#==== PREGUNA 3 ====
#===================
# Una compa��a de cosm�ticos hizo una prueba preliminar de su nueva crema quitamanchas, en que 30
# personas fueron separadas aleatoriamente en tres grupos de 10 voluntarios/as: uno de control, a quienes
# se les entreg� una crema placebo (humectante solamente); otro que usaron la crema quitamanchas que la
# compa��a comercializa actualmente; y el �ltimo que usaron el nuevo producto. A todos se les dijo que
# usaban la crema nueva de �ltima generaci�n. Dos personas del grupo de control y una del grupo con la
# crema existente abandonaron el estudio. Para el resto, se reportaron los siguientes n�meros de manchas
# removidas al finalizar el tiempo de prueba. �Es mejor la nueva crema quitamanchas?
texto2 <- ("
Nueva Actual Control
81 48 18
32 31 49
42 25 33
62 22 19
37 30 24
44 30 17
38 32 48
47 15 22
49 40 --
41 -- --
")
datos2 <- read.table(textConnection(texto2), header = TRUE, na.strings = "--")

datos2 <- datos2 %>% pivot_longer(c("Nueva", "Actual","Control"),
                                  names_to = "Producto",  
                                  values_to = "Manchas_Removidas"
)

# Ya con los datos leidos anteriormente, se procede a discutir que prueba no
# param�trica corresponder�a en este caso. La prueba m�s acorde para este
# problema es la "Prueba de Kruskal-Wallis", puesto que se el
# contexto presenta mas de dos muestras a comparar.

# Sin embargo, para poder utilizar esta prueba es necesario llevar a cabo la
# verificaci�n de una serie de condiciones:

# CONDICIONES:

#1) La variable independiente debe tener a lo menos dos niveles.
#   verificaci�n: Dado que el estudio posee tres variables a estudiar,
#                 se verifica explicitamente que el estudio posee dos o m�s niveles.

#2) La escala de la variable dependiente debe ser, a lo menos, ordinal.
#   verificaci�n: Dado que se est� tratando con una escala de medici�n correspondiente
#                 al "n�mero de" manchas removidas, es natural evaluarlo con palabras,
#                 mayor que, menor que igual que, etc. 
#                 Por lo que la escala de medici�n utilizada en este
#                 problema es ordinal definitivamente.

#3) Las observaciones son independientes entre s�.
#   verificaci�n: Dado que cada instacia obtenida del experimento corresponde a personas
#                 distintas a todas las dem�s que prueban el producto de manera separada,
#                 es razanoble concluir la independencia entre cada una.
#    

# Ya con las condiciones verificadas, se procede a definir las hip�tsis a contrastar:

# Hip�tesis nula (H0): todos los productos son igual de eficientes (en remoci�n de manchas)
# Hip�tesis alternativa (HA): al menos uno de los productos presenta eficiencia distinta a al menos
#                             alg�n otro producto.

# Ya con las hip�tesis descritas y con las condiciones verificadas, se efectua 
# la prueba correspondiente:

# nivel de significancia
alpha <- 0.05

# Prueba de Kruskal-Wallis
pruebaKruskal <- kruskal.test(Manchas_Removidas ~ Producto, data = datos2)
print(pruebaKruskal)

#RESPUESTA PREGUNTA 3
# Dado el valor de p-value (0.01228) es menor al nivel de significancia establecido
# (0.05), se rechaza la hip�tesis nula en favor de la alternativa. Por lo tanto, se puede concluir
# con 95% de confianza que al menos uno de los productos presenta eficiencia distinta 
# a al menos alg�n otro producto.

#Con este resultado, se realiza un prodecimiento post-hoc para verificar cual de los productos
# presenta la diferencia significativa (y as� poder dar respuesta a "�Es mejor la nueva crema quitamanchas?")
post_hoc <- pairwise.wilcox.test(datos2$Manchas_Removidas,
                                 datos2$Producto,
                                 p.adjust.method = "holm",
                                 paired = FALSE)
print(post_hoc)


#El resultado obtenido a partir del procedimiento post-hoc con Holm indica que s� existe diferencias significativas 
# del "Nuevo" producto respecto del producto "Actual", pero no as� con el producto "Control" (lo que es 'raro')
# con esto se puede verificar que la nueva crema quitamanchas efectivamente es mejor (que el producto actual).

#A pesar de lo anterior, estos resultados fueron concluidos a partir de un nivel de significancia no estricto (0.05),
# lo cual condujo a aseverar que el nuevo producto es efectivamente mejor (que el producto actual). Sin embargo, si se hubiese
# considerado un nivel de significancia m�s estricto (0.01), estos resultados hubiesen variado, puesto que el p-value de
# 0.01228 ya hubiese se�alado que se deb�a fallar al rechazar la hip�tesis nula, concluyendo as� que todos los productos son
# igual de eficientes.

#Ante esta situaci�n, se recomienda proceder con cautela ante la presentaci�n de resultados. A�n mas, se aconseja reformular
# el experimento, tomando una mayor cantidad de observaciones para las muestras, para as� verificar realmente los resultados
# obtenidos.

#===================
#==== PREGUNA 4 ====
#===================
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en
# donde un estudio o experimento, relacionado con el alza que han experimentado las tasas de inter�s de
# los cr�ditos en Chile, necesite utilizar una prueba de suma de Friedman, debido a problemas con la
# normalidad de los datos. Indiqu� cu�les ser�an las variables/niveles involucrados en su ejemplo y las
# hip�tesis nula y alternativa a contrastar.

#RESPUESTA PREGUNTA 4
# Con las recientes alzas que han experimentado las tasas de inter�s de los cr�tidos a nivel nacional, 
# provocando as� que el costo de endeudamiento aumente, lo qu provoca una baja en el consumo que a su vez
# repercute en la inflaci�n. 
# En base a esto, la empresa JoseLuis LTDA. desea evaluar el consumo actual de las personas para dos 
# grandes cadenas de servicios al consumidor: WallMark, SudCenco y RiPlay, para ello la empresa ha seleccionado 
# al azar a 8 personas, quienes fueron asignados de manera aleatoria para evaluar 4 aspectos de estas empresas.
# Cada persona eval�a el cambio en el consumo de productos en ambas empresas en una escala de likert de 
# siete niveles, donde 1 corresponde a "Muy poco" (respecto al consumo antes del alza) y 7 a "Muy alto" (respecto
# al consumo antes del alza).

#Se obtuvieron los siguientes resultados (suma de puntuacion de respuestas):
#
#         Usuario    WallMark    SudCenco    RiPlay
#            1          28          21         19
#            2          16          18         27
#            3          18          18         16
#            4          4           7          9
#            5          13          23         17
#            6          25          24         22
#            7          26          15         19
#            8          12          13         28

#Hip�tesis a contrastar:
#H0: el consumo actual hacia las empresas es similar entre ellas (respecto al consumo antes del alza).
#HA: al menos una de las empresas presenta diferencias respecto al consumo actual (respecto al consumo antes del alza).



