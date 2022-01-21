#Integrantes
# Maria Jesus Cañoles
# Bastian Diaz
# Simon Montenegro

#(Los tildes se omitieron al momento de redactar para evitar generar caracteres 'raros' a la hora de reabrir el archivo)

library(ggpubr)
library(pwr)
library(tidyverse)
library(Hmisc)


#---------- ENUNCUADO SALA 7 ----------
#Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972) (Journal of chronic
#diseases, 25(12), 711-716) sobre la incidencia de la cantidad de alcohol y de tabaco que se consume en el
#riesgo de padecer cancer oral. Las tablas muestran el numero de personas que consumiendo una cierta
#cantidad de alcohol o de tabaco, mostrada en cada fila, desarrollaron o no desarrollaron (controles) la
#enfermedad durante su vida.

#---------- PREGUNTA 1 ----------
#Estudios previos habian determinado que la incidencia de cancer oral en la poblacion general que no fuma
#era de 18%. ¿Respaldan estos datos tal estimacion?
cat("\n\n---------- PREGUNTA 1 ----------\n")

#Se pide verificar (aceptar o rechazar) la hipotesis.
#Denotando como p a la proporcion  de la poblacion general que no fuma, donde p0 corresponde al 18%
#H0: p = p0
#HA: p != p0

#Se tiene que para aquellas personas que consumen (o no) tabaco, existen 26 casos de un universo de 85 controles
#que corresponden a casos de cancer oral.
#Con esto, se tiene que la proporcion p corresponde a:


n <- 111 #Poblacion
p_tongo <- 26/n #Proporcion

#Se verifican las condiciones iniciales:
# 1) Las observaciones son independientes. Se cumple puesto que las muestras se toman a personas distintas.
# 2) Condicion de exito-fracaso (np >= 10 && n(1-p) >= 10)

exito <- n*p_tongo
cat("Se cumple condicion de exito, con resultado igual a", exito, "\n")

fracaso <- n*(1-p_tongo)
cat("Se cumple condicion de fracaso, con resultado igual a", fracaso, "\n")

#Se define valor nulo para calcular el estadistico de prueba:
p0 <- 0.18 #Valor nulo

#Se define alfa para un 95% de confianza:
alfa <- 0.05

#Se calcula el estadistico de prueba:
SE <- sqrt((p_tongo*(1-p_tongo))/n)

Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
  
#Se crea el intervalo de confianza (p +- z*SE)
inferior  <- p_tongo - Z_critico * SE
superior  <- p_tongo + Z_critico * SE
cat("Intervalo  de  confianza = [", inferior , ", ", superior , "]\n", sep = "")

# Prueba  de hipotesis.
SE_hip  <- sqrt(( p0 * (1 - p0)) / n)
Z <- (p_tongo  - p0) / SE_hip
p_resultado <- 2 * pnorm(Z, lower.tail = FALSE)
cat("Hipotesis  alternativa  bilateral\n")
cat("Z =", Z, "\n")
cat("p =", p_resultado)

#Con este valor de p (0.13) > alfa (0.05), se puede asegurar con un 95% de confianza que no existe evidencia suficiente para rechazar 
# la hipotesis nula (H0). Por lo tanto, los datos si respaldan 
# la estimacion de la incidencia en la poblacion general que no fuma es de un 18%

#---------- PREGUNTA 2 ----------
#Segun estos datos, ¿da lo mismo no fumar que hacerlo diariamente consumiendo entre 1 y 19 cigarrillos?
cat ("\n\n---------- PREGUNTA 2 ----------\n")
#Se pide verificar (aceptar o rechazar) la hipotesis nula de una diferencia de proporciones.
#Denotando como p1 a la proporcion de la muestra de aquellas personas no fumadoras y p2 a la porporcion de la muestra
# de aquellas personas que furan entre 1 y 19 cigarrillos:
#H0: p1 = p2    -> p1 - p2 = 0
#HA: p1 != p2   -> p1 - p2 != 0

#Con esto en mente, dado que se busca comparar dos proporciones de dos muestras, se realiza la verificacion
# exito-fracaso y la estimacion del error estandar en base a la proporcion agrupada.

n1 <- 111 #Muestra de personas no fumadoras
n2 <- 163 #Muestra de personas que fuman entre 1 y 19 cigarrillos
p1_tongo <- 26/n1
p2_tongo <- 66/n2

p_tongo2 <- ((p1_tongo*n1)+(p2_tongo*n2))/(n1+n2)

#Con esto, podemos realizar la verificacion de condiciones iniciales
# 1) Las observaciones son independientes. Se cumple puesto que las muestras se toman a personas distintas.
# 2) Condicion exito-fracaso
#Para la primera muestra
exito_1 <- n1*p_tongo2
cat("Se cumple condicion de exito para la primera muestra, con resultado igual a", exito_1, "\n")

fracaso_1 <- n1*(1-p_tongo2)
cat("Se cumple condicion de fracaso para la primera muestra, con resultado igual a", fracaso_1, "\n")

#Para la segunda muestra
exito_2 <- n2*p_tongo2
cat("Se cumple condicion de exito para la segunda muestra, con resultado igual a", exito_2, "\n")

fracaso_2 <- n2*(1-p_tongo2)
cat("Se cumple condicion de fracaso para la segunda muestra, con resultado igual a", fracaso_2, "\n")
  
#Se define alfa para un 95% de confianza:
alfa <- 0.05

#Se define el valor nulo = 0 (ya que se pide la diferencia)
p0_2 <- 0

#Se calcula el error estandar en base a la proporcion agrupada
SE_2 <- sqrt(((p_tongo2*(1-p_tongo2))/n1) + ((p_tongo2*(1-p_tongo2))/n2))

#Se calcula el estimador puntual para la diferencia
p_diferencia <- p1_tongo - p2_tongo

#Se calcula el estadistico de prueba
Z_critico2 <- qnorm(alfa / 2, lower.tail = FALSE)

#Se crea el intervalo de confianza (p +- z*SE)
inferior2 <- p_diferencia - (Z_critico2 * SE_2)
superior2 <- p_diferencia + (Z_critico2 * SE_2)
cat("Intervalo  de  confianza = [", inferior2 , ", ", superior2 , "]\n", sep = "")


#Se realiza la prueba de hipotesis
p_agrupada <- (exito_1 + exito_2)/(n1+n2)
error_1 <- (p_agrupada*(1-p_agrupada))/n1
error_2 <- (p_agrupada*(1-p_agrupada))/n2
SE_hip_2 <- sqrt(error_1 + error_2)
Z2 <- (p_diferencia - p0_2)/SE_hip_2
p_resultado_2 <- 2*pnorm(Z2, lower.tail = FALSE)
cat("Hipotesis  alternativa  bilateral\n")
cat("Z =", Z2, "\n")
cat("p =", p_resultado_2)

#Con este valor de p (2) >> alfa (0.05), se puede asegurar con un 95% de confianza que no existe evidencia suficiente 
# para rechazar la hipotesis nula (H0). Por lo tanto, segun los datos,
# no existe diferencia entre la incidencia del cancer oral de no fumar y 
# fumar entre 1 y 19 cigarrillos diaramente.


#---------- PREGUNTA 3 ----------
#Suponiendo que la diferencia en la proporcion de personas que desarrollan la enfermedad entre quienes
#no fuman y aquellos que fuman de 1 a 19 cigarrillos al dia es de 0.25. ¿Cuanta gente deberiamos
#monitorear para obtener un intervalo de confianza del 95% y poder estadistico de 90%? si se intente
#mantener aproximadamente la misma proporcion de gente estudiada en cada caso.
cat ("\n\n---------- PREGUNTA 3 ----------\n")
#Se solicita encontrar nuevas muestras poblacionales para las muestras n1 y n2 (personas no fumadoras y
# personas que fuman de 1 a 19 cigarrillos diaramente, respectivamente)
#Se tiene que la diferencia de proporciones corresponde a 0.25, con esto, se plantea el valor nulo, es decir:
# p = p1 - p2 = 0.25, donde p1 corresponde a la proporcion de personas que no fuman cigarrillos y p2 a la proporcion
# de personas que fuman de 1 a 19 cigarrillos al dia.
#A partir de esto, la hipotesis:

#H0: p != 0.25; la diferencia de proporcion entre personas que no fuman y aquellas que fuman 1 a 19 cigarrillos diarios
#                         es distinta de 0.25
#HA: p =  0.25; la diferencia de proporcion entre personas que no fuman y aquellas que fuman 1 a 19 cigarrillos diario
#                         es igual a 0.25

#Muestras originales
n1 <- 111 #Muestra de personas no fumadoras
n2 <- 163 #Muestra de personas que fuman entre 1 y 19 cigarrillos

#Proporciones
p1_tongo <- 26/n1
p2_tongo <- 66/n2


fraccion <- n1/(n1+n2)
alfa <- 0.05
poder <- 0.9

#Se utiliza la funcion bsamsize para encontrar la nueva muestra necesaria para cada una de las proporciones.
cantidad <- bsamsize(p1_tongo, p2_tongo, n1/(n1+n2), alfa, poder)
n1_nuevo <- cantidad[1]
n2_nuevo <- cantidad[2]

cat ("La nueva cantidad de personas que no fuman a monitorear es de:", ceiling(n1_nuevo), "\n")
cat ("La nueva cantidad de personas que fuman de 1 a 19 cigarrillos diarios a monitorear es de:", ceiling(n2_nuevo), "\n")

















