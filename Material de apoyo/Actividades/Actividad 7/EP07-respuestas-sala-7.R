#(Los tildes se omitieron al momento de redactar para evitar generar caracteres 'raros' a la hora de reabrir el archivo)

library(ggpubr)
library(pwr)
library(tidyverse)
library(Hmisc)


#---------- ENUNCIADOS SALA 7 ----------

#---------- PREGUNTA 1 ----------
#Se cree que la ardilla chilena (Octodon degus) emite un chillido cuando es perseguido por un depredador,
#posiblemente para alertar a otros deg�s. Se hizo un experimento en que 45 degus se liberaron a 10 o a
#100 metros de su madriguera y luego se les perseguia hasta que se metian en ella, para simular la
#persecucion de un depredador. De las 24 ardillas liberadas a 10 metros de la madriguera, 15 emitieron el
#chillido esperado, mientras que 8 de las 21 ardillas liberadas a 100 metros de la madriguera lo hicieron.
#�Influye la distancia a la madriguera en la emision del chillido por parte de un degu?
cat("\n\n---------- PREGUNTA 1 ----------\n")

#Antecedentes:
# Del enunciado de la pregunta, se sabe que ambas variables de estudio son dicotomicas, puesto que miden la presencia
# o ausencia del chillido en relacion a la distancia entre el degu y su madriguera.
# Antes esto, el metodo apropiado a utilizar corresponde a Prueba exacta de Fisher

#Se pide verificar (aceptar o rechazar) la hipotesis de acuerdo a si el chillido es dependiente de la distancia
# del degu a su madriguera.
#Se declaran las hipotesis a contrastar
#H0: Las variables distancia a madriguera y ocurrencia del chillido son independientes entre si.
#HA: Las variables distancia a madriguera y ocurrencia del chillido son dependientes entre si.

#               10m         100m    TOTAL
#Chillido       15          8      |  23 
#No chillido    9           13     |  22
#----------------------------------------
#TOTAL          24          21     |  45
#

distancia <- c(rep("10_m", 24), rep("100_m", 21))
resultado <- c(rep("Chillido", 15), rep("No_chillido", 9), rep("Chillido", 8), rep("No_chillido", 13))
datos <- data.frame(resultado, distancia)
tabla <- xtabs(~., datos)
print(tabla)

#aplicar prueba exacta de fisher
alfa <- 0.05
prueba_fisher <- fisher.test(tabla, 1-alfa)

cat("\nRespuesta: p = 0.1392 > 0.05 (alfa), se falla al rechazar hipotesis nula. \n")
#Con este valor de p (0.1392), considerando un nivel de significancia de 0.05 (95% de confianza), se falla al rechazar
# la hipotesis nula, es decir, las variables distancia a madriguera y ocurrencia del chillido son independientes entre si.



#---------- PREGUNTA 2 ----------
#Un art�culo describe un estudio en que se compararon diferentes versiones de algoritmos evolutivos para
#resolver variadas instancias de problemas de clasificaci�n tomadas desde el UCI Machine Learning
#Repository. La siguiente tabla muestra los resultados de la clasificaci�n (COR: correcta, INC: incorrecta)
#hecha por dos versiones de un algoritmo gen�tico evaluado en el estudio para el problema Breast Cancer.
#�Hay un algoritmo con mejor desempe�o que el otro?
cat ("\n\n---------- PREGUNTA 2 ----------\n")
#Antecedentes
#Del enunciado se solicita entregar una respuesta de caracter dicot�mico, puesto que solo se quiere saber si existe
# un algoritmo mejor que el otro, pero sin especificar cu�l ni tampoco sus caracter�sticas o valores.
# Ante esta situacion, el metodo apropiado a emplear corresponde a la Prueba de mcNemar

#Se declaran las hipotesis a contrastar
#H0: No un hay algoritmo con mejor desempe�o entre los evaluados.
#HA: Si un hay algoritmo con mejor desempe�o entre los evaluados.

#Se declara la tabla de resultados y se construye la matriz de confusion
instancias <- seq(1:14)
modelo_1 <- c(rep("Incorrecto", 3), rep("Correcto", 1), rep("Incorrecto", 3), rep("Correcto", 1), 
              rep("Incorrecto", 3), rep("Correcto", 1), rep("Incorrecto", 2))
modelo_2 <- c(rep("Correcto", 5), rep("Incorrecto", 1), rep("Correcto", 5), rep("Incorrecto", 1), rep("Correcto", 2))
datos_2 <- data.frame(instancias, modelo_2, modelo_1)
tabla_2 <- table(modelo_2, modelo_1)
print(tabla_2)

#Se aplica la prueba de mcNemar
prueba_mcnemar <- mcnemar.test(tabla_2)

cat("\nRespuesta: p = 0.01586 < 0.05 (alfa), se rechaza la hipotesis nula. \n")

#Con este valor de p (0.01586), considerando un nivel de significancia de 0.05 (95% de confianza), se rechaza
# la hipotesis nula, es decir, si existe un algoritmo con mejor desempe�o que el otro.

#---------- PREGUNTA 3 ----------
#Una investigacion monitoreo a mas de 50 mil mujeres adultas durante 10 a�os (Lucas et al., 2011. Coffee,
#Caffeine, and Risk of Depression Among Women. Archives of Internal Medicine, 171(17), 1571-1578) con
#la intenci�n de identificar factores de riesgo de desarrollar un cuadro de depresi�n. Entre otras cosas, se
#registr� el consumo de cafe�na, cuyos datos se resumen en la siguiente tabla. �Existe alguna asociaci�n
#entre la cantidad de caf� que se bebe y la depresi�n?
cat ("\n\n---------- PREGUNTA 3 ----------\n")

#Antecedentes
#Se puede asumir que la muestra de mujeres fue tomada de manera aleatoria, puesto que se trata de una
# investigacion cientifica.
#Por otro lado, la muestra de 50.000 mujeres evidentemente no representa mas del 10% de la poblacion
# mundial de mujeres.

#Se declaran las hipotesis a contrastar
#H0: Las variables de estudio consumo de cafe y depresion son independientes.
#HA: Las variables de estudio consumo de cafe y depresion estan relacionadas.

si_depresion <- c(640, 353, 905, 584, 110)
no_depresion <- c(11775, 6264, 16329, 11706, 2273)

tabla_3 <- as.table(rbind(si_depresion, no_depresion))

dimnames(tabla_3) <- list(tipo = c("si_depresion", "no_depresion"), 
                        cantidad_cafe = c("<=1 taza p/semana", "2-6 taza p/semana", "1 taza p/dia", "2-3 taza p/dia", ">=4 taza p/dia"))
print(tabla_3)

#hacer prueba chi-cuadrado de independencia (0.01 nivel de significacion)
prueba_indip <- chisq.test(tabla_3)
#print(prueba_indip)

cat("\n\n")
#Se calculan los valores esperados de la prueba chi-cuadrada
esperados <-round(prueba_indip[["expected"]], 3)
print(esperados)


cat("\nRespuesta: p = 0.2086 > 0.01 (alfa), se falla al rechazar la hipotesis nula. \n")

#Con este valor de p (0.2086), considerando un nivel de significancia de 0.01 (99% de confianza), se falla al rechazar
# la hipotesis nula, es decir, las variables de estudio consumo de cafe y depresion son independientes.


#---------- PREGUNTA 4 ----------
#Enuncie un ejemplo novedoso (no discutido en clase ni en las lecturas) relacionado con el ejercicio regular
#que realizan los chilenos antes y despu�s de la pandemia de COVID19, que requiera utilizar una prueba Q
#de Cochran. Identifique las variables involucradas y las hip�tesis a contrastar.
cat ("\n\n---------- PREGUNTA 4 ----------\n")
cat ("\nRespuesta: ~Leer comentario del codigo~ . \n")
#Un ejemplo es la efectividad de las vacunas, por ejemplo: sinovac, pfizer y astraxeneca, 
#El ejercicio consiste en analizar cuantos exitos hubo despues de un tiempo relativo de la inyeccion de la 
#vacuna en una persona, siendo los exitos si la persona se ha contagiado.
#las variables involucradas seria el exito, representada por (1), en caso contrario (0)
#y el tipo de vacuna: sinovac, pfizer y astraxeneca.

#Las hipotesis a contrastar son:
#H0: La proporcion de exitos de la vacunas es la misma para todos los tipos de vacuna.
#H1: La proporcion de exitos es distinta para al menos una vacuna.


















