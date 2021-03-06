library(ggpubr)
library(pwr)
library(ggplot2)
library(tidyverse)
library(ez)

#La avicultura de carne es un negocio muy lucrativo, y cualquier m�todo que ayude al r�pido crecimiento de
#los pollitos es beneficioso, tanto para las av�colas como para los consumidores no veganos. En el paquete
#datasets de R (importado nativamente) est� el conjunto de datos chickwts con los resultados de un
#experimento hecho (supuestamente en 1948) para medir la efectividad de varios suplementos alimenticios
#en la tasa de crecimiento de las aves, en donde pollitos reci�n nacidos se separaron aleatoriamente en
#seis grupos, y a cada grupo se le dio un suplemento distinto por seis semanas. Se reportan los pesos, en
#gramos, alcanzados por los pollitos. Para productores de la 7� regi�n, es especialmente importante saber
#si deber�an usar suplementos basados en linaza (linseed), soya (soybean), habas (horsebean) o carne
#(meatmeal)

datos <- chickwts

datos <- datos %>% filter(feed == "linseed" | feed == "soybean" | feed == "horsebean" | feed == "meatmeal")

#Puesto que solo nos interesan cuatro alimentos espec�ficos (por enunciado): linseed, soybean, horsebean y meatmeal,
# se procede a filtrar los datos para realizar el procedimiento.

datos[["feed"]] <- factor(datos[["feed"]])
datos[["instancia"]] <- factor(1:nrow(datos))

#Condiciones para utilizar AVOVA de una via para muestras independientes
# 1) La escala con que se mide la variable dependiente tiene las propiedades de una
# escala de intervalos iguales
#   Si, puesto que el dataframe, especificamente la variable a medir se encuentra en
#   gramos para todas las observaciones.
# 
# 2) Las k muestras son obtenidas de manera aleatoria e independiente de la(s) poblaci�n(es)
# de origen.
#   Si, se asume que el paquete chickwts corresponde a muestras aleatorias e independientes,
#   esto se verifica con el enunciado, donde las observaciones de cada grupo fueron seleccionados
#   aleatoriamente.
#
# 3) Se puede suponer razonablemente que la(s) poblaci�n(es) de origen sigue(n) una distribuci�n
# normal.
#   Para ello, se realiza la gr�fica de los grupos de estudio, en este caso, seg�n tipo de alimento
#   y se analiza si existen valores at�picos.

g <- ggqqplot(datos,
              x = "weight",
              y = "feed",
              color = "feed")

g <- g + facet_wrap(~ feed)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

#   En base a lo anterior, dado que existen valores at�picos (especificamente en el alimento "sunflower")
#   es que se debe proceder con cautela, por lo que se define un nivel de significancia de 0.01

alfa <- 0.01

# 4) Las k muestras tienen varianzas aproximadamente iguales.
#   Para esto, se hace uso de ezAnova(), la cual permite realiza tambi�n la prueba de homocedasticidad
#   de Levene, la cual permite dar respuesta a la condici�n.

k <- 6 #Cantidad grupos

#Se establecen las hip�tesis para el test de levine.
#H0: las varianzas de las k muestras son iguales.
#HA: al menos una de las muestras tiene varianza diferente a alguna de las dem�s. 


cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
pruebaEzAnova <- ezANOVA(data = datos,
                        dv = weight,
                        between = feed,
                        wid = instancia,
                        type = 3,
                        return_aov = TRUE)
print(pruebaEzAnova)

#Gr�fico del tama�o del efecto.
g2 <- ezPlot(data = datos , 
              dv = weight,
              wid = instancia, 
              between = feed,
              y_lab = "Peso promedio de pollitos [g]", 
              x = feed)
print(g2)

#En torno a estos resultados del test de homogeneidad de las varianzas, se tiene 
# un p-value igual a 0.5896, que a su vez supera al nivel de significaci�n estipulado,
# con esto, se falla al rechazar la hip�tesis nula, por lo que se verifica que las varianzas de las
# k muestras son iguales, es decir, se cumple la cuarta condici�n.

#Con lo visto anteriormente, dado que se cumplen todas las condiciones para aplicar Anova, se procede a
# establecer las hip�tesis entorno al problema.
#H0: el peso promedio para las instancias es igual para todos los tipos de alimento.
#HA: el peso promedio para las instancias es diferente para al menos un tipo de alimento.

#Como se vi� anteriormente al aplicar ezAnova(), el resultado del p-value en torno a la situaci�n corresponde
# a p ~ 5.94*1^-10, es decir, un valor muy por debajo del nivel de significancia establecido, por lo que
# se rechaza la hip�tesis nula en favor de la hip�tesis alternativa, se puede asegurar con un 99% de confianza
# que el peso promedio para las instancias es diferente para al menos un tipo de alimento.

#Con todo lo analizado, SE PROCEDE A REALIZAR UN PROCEDIMIENTO POST-HOC con el fin de poder determinar
# que grupo presenta un peso promedio diferente.

#El procedimiento Post-Hoc seleccionado corresponde al HSD Tukey, por el nivel de precisi�n en la selecci�n
# de diferencias significativas.

#Procedimiento ANOVA con aov ().
cat(" Procedimiento    ANOVA    usando    aov \ n\ n") 
pruebaAnova <- aov(weight ~ feed, data = datos) 
print(summary(pruebaAnova))

#Prueba HSD de Tukey .
post_hoc <- TukeyHSD(pruebaAnova,
                    "feed",
                     ordered = TRUE,
                     conf.level = 1 - alfa)
print(post_hoc)

#En base a estos resultados, el �nico alimento que presenta diferencias significativas
# respecto de los dem�s es "Horsebean" (este presenta p-value < alfa cuando se compara con
# "soybean" y "meatmeal"). Por lo que los productores de la 7ma regi�n deber�an preferir entre
# alimentos basados en soya, linaza o carne, ya que estas no presentan diferencias significativas
# entre ellos y que adem�s entregan pesos promedios 'altos' (comparativamente).












