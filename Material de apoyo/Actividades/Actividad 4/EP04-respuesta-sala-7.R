#SALA 7
#INTEGRANTES:
# María Jesus Cañoles (20.300.159-2)
# Bastián Díaz (20.433.892-3)
# Simón Montenegro (19.794.721-7)


#Pregunta 1
#El artículo "Engineering Properties of Soil" (Soil Science 1998) puso a prueba la idea generalizada de que
#la materia orgánica en el suelo no supera el 3%. Para esto, los autores obtuvieron una muestra aleatoria
#de especímenes de suelo, determinando que el porcentaje de materia orgánica presente en cada
#espécimen era (usando punto en vez de coma decimal):
#  3.10 5.09 2.97 1.59 4.60 3.32 0.55 1.45 0.14 4.47
#0.80 3.50 5.02 4.67 5.22 2.69 3.98 3.17 3.03 2.21
#2.69 4.47 3.31 1.17 2.76 1.17 1.57 2.62 1.66 2.05
#¿Qué conclusión sugeriría a los autores?

#Se incluyen librerias
library(dplyr)
library(ggpubr)
library(gtools)
library(ggplot2)
library(TeachingDemos)
library(datasets)

#Se realiza la lectura de la base de datos
texto <- "3.10 5.09 2.97 1.59 4.60 3.32 0.55 1.45 0.14 4.47
0.80 3.50 5.02 4.67 5.22 2.69 3.98 3.17 3.03 2.21
2.69 4.47 3.31 1.17 2.76 1.17 1.57 2.62 1.66 2.05"
file <- textConnection(texto)
datos <- scan(file)

#Se establecen las hipótesis.
#H0: media =< 3%; la materia orgánica en el suelo no supera el 3%
#HA: media > 3%; la materia orgánica en el suela si supera el 3%


#Para poder realizar una PRUEBA Z, se debe cumplir que:
# 1) Cantidad de muestras >= 30
# 2) Observaciones independientes
# 3) Población sigue una distribución Normal

# 1) Cantidad de muestras >= 30
cantidad_muestras <- length(datos)
print(cantidad_muestras)

# 2) Observaciones independientes
# Por enunciado se tiene que las observaciones son independientes.

# 3) Se estudia la normalidad usando la prueba de Shapiro.

# Se define el nivel de significancia, para un 95% de confianza (1-alfa = 0.95)
alfa <- 0.05

# Nivel de confianza
confianza <- 1-alfa

normalidad <- shapiro.test(datos)
print(normalidad)
#p-value = 0.4197
#Con este valor de p (superior al nivel de significancia) se puede decir con relativa confianza que la población
# de donde proviene la muestra sigue una distribución normal.

#Se hace la prueba Z
# Valor nulo
nulo <- 3

# Desviación estandar
desviacion <- sd(datos)

# Se realiza z.test unilateral
prueba <- z.test(x=datos, mu = 3, stdev = desviacion, conf.level = confianza, alternative = "less")
print(prueba)
#p-value = 0.2637 y z = -0.63195

#Con esto, como p es mayor que el nivel de significancia, se falla al rechazar la hipótesis nula.
#Por lo que se asegura con un 95% de confianza que el porcentaje de materia orgánica en el suelo no supera el 3%.

################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################

#Pregunta 2
#Se sabe que la lactancia estimula una pérdida de masa ósea para proporcionar cantidades de calcio
#adecuadas para la producción de leche. Un estudio intentó determinar si madres adolescentes podían
#recuperar niveles más normales a pesar de no consumir suplementos (Amer. J. Clinical Nutr., 2004; 1322-
#1326). El estudio obtuvo las siguientes medidas del contenido total de minerales en los huesos del cuerpo
#(en gramos) para una muestra de madres adolescentes tanto durante la lactancia (6-24 semanas
#postparto) y posterior a ella (12-30 semana postparto):

#Se establecen las hipótesis.
#H0: mediaDiferencias > 50; la media de las diferencias excede los 50g
#HA: mediaDiferencias <= 50g; la media de las diferencias no excede por mas de 50g

#Se cargan los datos para lactancia
lactancia <- c(1928, 2549, 2825, 1924, 1628, 2175, 2114, 2621, 1843, 2541)

#Se cargan los datos para posdestete
destete <- c(1986, 2745, 2755, 1802, 1610, 2044, 2024, 2486, 1866, 2487)

#Se guardan las diferencias para cada sujeto
datosDiferencia <- destete - lactancia

#Se verifica que la distribución se acerce a la normal
# Se define el nivel de significancia, para un 95% de confianza (1-alfa = 0.95)
alfa2 <- 0.05

# Nivel de confianza
confianza2 <- 1-alfa2

normalidad2 <- shapiro.test(datosDiferencia)
print(normalidad2)

#p-value = 0.1389
#Con este valor de p (superior al nivel de significancia) se puede decir con relativa confianza que la población
# de donde proviene la muestra sigue una distribución normal.

#Se declara el valor nulo
nulo2 <- 50

#Se realiza la 
prueba2 <- t.test(x = lactancia, y = destete, paired = TRUE, alternative = "less", mu = nulo2, conf.level = confianza2)
print(prueba2)
#p-value = 0.322 y t = -0.47809

#En este caso la media está dentro del intervalo de confianza, además de que el valor p es mayor que el nivel de significación,
# por lo que se rechaza la hipótesis nula en favor de la hipótesis alternativa.
#Por lo que se asegura con un 95% de confianza que la media de las diferencias no excede por mas de 50g.

################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################

#Pregunta 3
#La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rápido crecimiento de
#los pollitos es beneficioso, tanto para las avícolas como para los consumidores. En el paquete datasets
#de R están los datos (chickwts) de un experimento hecho para medir la efectividad de varios
#suplementos alimenticios en la tasa de crecimiento de las aves. Pollitos recién nacidos se separaron
#aleatoriamente en 6 grupos, y a cada grupo se le dio un suplemento distinto. Para productores de la 7ma
#región, es especialmente importante saber si existe diferencia en la efectividad entre el suplemento
#basado en linaza (linseed) y el basado en soya (soybean).

#Se establecen las hipótesis.
#H0: mediaLinaza = mediaSoya; no existe diferencia entre ambos suplementos
#HA: mediaLinaza != mediaSoya; si existe diferencia entre ambos suplementos

#Se cargan los datos
datos <- chickwts

#Pesos de linseed.
linseed <- datos %>% filter(feed == "linseed")
linseed<- linseed[["weight"]]

#Pesos de soybean
soybean <- datos %>% filter(feed == "soybean")
soybean<- soybean[["weight"]]

#Se verifica que la distribución se acerce a la normal
# Se define el nivel de significancia, para un 95% de confianza (1-alfa = 0.95)
alfa3 <- 0.05

# Nivel de confianza
confianza3 <- 1-alfa3

#Test de shapiro para linseed
normalidadLinseed <- shapiro.test(linseed)
print(normalidadLinseed)

#Test de shapiro para soybean
normalidadSoybean <- shapiro.test(soybean)
print(normalidadSoybean)

#LINSEED p-value = 0.9035
#SOYBEAN p-value = 0.5064

#Con estos valores de p (muy superiores al nivel de significancia) se puede decir 
# con relativa confianza que la población muestral de sigue una distribución normal.

#Se declara el valor nulo
nulo3 <- 0

#Se realiza la prueba T para datos no pareados (independientes)
prueba3 <- t.test(x = linseed, y = soybean, paired = FALSE, alternative = "two.sided", mu = nulo3, conf.level = confianza3)
print(prueba3)

#Dado que el valor de p es mayor que el nivel de significancia, con esto, se falla al rechazar la hipótesis nula.
#Finalmente se puede asegurar con un 95% de confianza de que no existe diferencia entre ambos suplementos.














