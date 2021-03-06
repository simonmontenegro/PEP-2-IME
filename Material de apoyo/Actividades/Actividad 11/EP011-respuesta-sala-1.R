library(ggpubr)
library(pwr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(boot)
library(ez)


#Se especifica y almacena la ruta del directorio de la base de datos.
dir <- "~/../Desktop/IME/Actividades/Actividad 11"
base <- "Datos-Casen-v2.xls"
arch <- file.path(dir, base)

# Se realiza la lectura de los datos, se escpecifica el formato de codificaci�n UTF-8
casen <- read_excel(arch)

#Se plantea la funcion que calcula la diferencia de dos muestras bajo una funcion dada
calcular_diferencia <- function(muestra_1, muestra_2, FUN){
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

#Se plantea la funcion que realiza la permutaci�n y calcula el estadistico
# de inter�s.
permutar <- function(muestra_1, muestra_2, FUN){
  #Se obtienen los largos (cantidad de observaciones) de cada muestra
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  
  #Se realiza la permutaci�n
  permutacion <- sample(c(muestra_1, muestra_2), size = n_1 + n_2,
                        replace = F)
  
  #Se asignan elementos a los dos grupos
  permutacion_1 <- permutacion[1 : n_1]
  permutacion_2 <- permutacion[n_1 + 1 : n_2]
  
  #Se calcula la diferencia y se retorna la diferencia de las medias.
  return(calcular_diferencia(permutacion_1, permutacion_2, FUN))
}

#Funci�n para calcular el valor p.
#Argumentos:
#- distribucion: distribuci�n nula del estad�stico de inter�s.
#- valor_ observado: valor del estad�stico de inter�s para las muestras originales.
#- repeticiones: cantidad de permutaciones a realizar.
#- alternative : tipo de hip�tesis alternativa. "two.sided " para
#  hip�tesis bilateral, "greater" o "less" para hip�tesis unilaterales. 
#Valor:
#- el valor p calculado.

calcular_valor_p <- function(distribucion, valor_observado, repeticiones, alternative ) { 
  if(alternative == "two.sided ") {
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1 
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  else if(alternative == "greater") {
    numerador <- sum(distribucion > valor_observado) + 1 
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  else {
    numerador <- sum(distribucion < valor_observado) + 1 
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  return (valor_p)
}


#Funci�n para graficar una distribuci�n.
#Argumentos:
# - distribucion: distribuci�n nula del estad�stico de inter�s.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot .
graficar_distribucion <- function(distribucion, estadistico_interes, columnas) {
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = estadistico_interes,
                            ylab = "Frecuencia",
                            bins = columnas,
                            add = "mean",
                            color = "blue",
                            fill = "blue"
  )
  
  qq <- ggqqplot(observaciones, x = "distribucion")
  
  # Crear una �nica figura con todos los gr�ficos de dispersi�n.
  figura <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

# Funci�n para hacer la prueba de permutaciones.
# Argumentos:
# - muestra_1, muestra_2: vectores num�ricos con las muestras a comparar.
# - repeticiones: cantidad de permutaciones a realizar .
# - FUN: funci�n del estad � stico E para el que se calcula la diferencia.
# - alternative: tipo de hip� tesis alternativa . "two. sided " para
#   hip�tesis bilateral, "greater" o "less" para hip�tesis unilaterales.
# - plot: si es TRUE, construye el gr�fico de la distribuci�n generada.
# - ...: otros argumentos a ser entregados a graficar_distribucion.

contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot, estadistico_interes, columnas) {
  cat("Prueba de permutaciones \n\n")
  cat("Hip�tesis alternativa:", alternative , "\n")
  observado <- calcular_diferencia(muestra_1, muestra_2, FUN)
  cat ("Valor observado :", observado , "\n")
  
  distribucion <- rep(NA , repeticiones)
  
  for (i in 1: repeticiones ) {
    distribucion[i] <- permutar(muestra_1, muestra_2, FUN)
  }
  
  if(plot) {
    graficar_distribucion(distribucion, estadistico_interes, columnas)
  }
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones, "two. sided ")
  cat ("Valor p:", valor_p, "\n\n")
}

#====================
#==== PREGUNTA 1 ====
#====================
# Propongan una pregunta de investigaci�n original, que involucre la comparaci�n de las medias de dos
# grupos independientes (m�s abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una
# muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulaci�n
# Monte Carlo.
# PREGUNTA propuesta: La edad promedio en Chile �es la misma entre personas casadas y solteras?

#Solucion
#Se filtra por edad y estado civil
edad <- casen[["edad"]]
ecivil <- casen[["ecivil"]]
datos <- data.frame(edad, ecivil)

#Se obtienen los datos necesarios para el problema.
datos <- datos %>% filter(ecivil == "Soltero(a)" | ecivil == "Casado(a)")

#Se setea la semilla
set.seed(123)

#Se selecciona una muestra de n elementos
n <- 350
datos <- datos[sample(nrow(datos), n, replace = T), ]

#Se obtienen las dos muestras a comparar
solteros <- datos %>% filter(ecivil == "Soltero(a)")
frameSolteros <- solteros
solteros <- solteros[["edad"]]

casados <- datos %>% filter(ecivil == "Casado(a)")
frameCasados <- casados
casados <- casados[["edad"]]

#Se realizan gr�ficas que permiten tener una mejor idea de como se distribuyen los
# datos.

#Solteros
#Histograma.
histogramaSolteros <- gghistogram(frameSolteros, 
                                  x = "edad",
                                  title = "Solteros",
                                  ylab = "Frecuencia",
                                  bins = 50,
                                  add = "mean",
                                  color = "blue",
                                  fill = "blue"
)

#Gr�fico QQ. (Normalidad)
qqSolteros <-   ggqqplot(frameSolteros, x = "edad", y = "ecivil", color = "blue") 
qqSolteros <-   qqSolteros + facet_wrap (~ ecivil )
qqSolteros <-   qqSolteros + rremove("x.ticks") + rremove("x.text") 
qqSolteros <-   qqSolteros + rremove("y.ticks") + rremove("y.text") 
qqSolteros <-   qqSolteros + rremove("axis.title")


#Casados
#Histograma.
histogramaCasados <- gghistogram(frameCasados,
                                 x = "edad",
                                 title = "Casados",
                                 ylab = "Frecuencia",
                                 bins = 50,
                                 add = "mean",
                                 color = "red",
                                 fill = "red"
)

#Gr�fico QQ. (Normalidad)
qqCasados <-   ggqqplot(frameCasados, x = "edad", y = "ecivil", color = "red") 
qqCasados <-   qqCasados + facet_wrap (~ ecivil )
qqCasados <-   qqCasados + rremove("x.ticks") + rremove("x.text") 
qqCasados <-   qqCasados + rremove("y.ticks") + rremove("y.text") 
qqCasados <-   qqCasados + rremove("axis.title")


figura <- ggarrange(histogramaSolteros, qqSolteros, histogramaCasados, qqCasados, ncol = 2, nrow = 2)
print(figura)
#En base a las gr�ficas y distribuciones generadas, se puede apreciar que los datos son
# problem�ticos puesto que presentan una notable asimetr�a, con lo que no se aproximan a una
# distribuci�n normal, por lo que el realizar un m�todo de boostrapping/monte carlo es ideal para
# este tipo de situaciones.

#Monte Carlo#
#Denotando uS al promedio de edad de personas solteras, uC al promedio de edad de personas casadas,
# entonces se busca verificar que:

#Se establecen las hip�tesis a contrastar.
#H0: uS - uC = 0
#HA: uS - uC != 0

#Para este estudio, se esteblece un nivel de significancia de:
alfa <- 0.05

# Hacer pruebas de permutaciones para la media y la varianza .
R = 5999

#Se realiza la prueba de permutaciones para la media (simulaci�n de MonteCarlo).
contrastar_hipotesis_permutaciones(solteros, casados, repeticiones = R, FUN = mean,
                                   alternative = "two.sided ", plot = TRUE,
                                   estadistico_interes = "media",
                                   columnas = 50)

#De los gr�ficos obtenidos, se puede ver que la distribuci�n generada se asemeja bastante a una 
# distribuci�n normal, y que el test de normalidad del gr�fico QQ no arroja valores at�picos.

#Adem�s, se tiene como valor observado -13.65, lo que indica que el promedio de edad de casados es mayor que
# el promedio de edad de solteros.

#Con el resultado obtenido y el previo an�lisis, se tiene que el p-value obtenido es 0.00016... muy por 
# debajo del nivel de significancia, en base a esto se rechaza la hip�tesis nula en favor de la alternativa,
# con lo que se concluye con un 95% de confianza que si existe diferencia entre el promedio de edades de personas 
# (en Chile) solteras y casadas.

#===================
#==== PREGUNA 2 ====
#===================
# Propongan una pregunta de investigaci�n original, que involucre la comparaci�n de las medias de m�s de
# dos grupos independientes (m�s abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta
# utilizando bootstrapping. Solo por ejercicio acad�mico, aplique un an�lisis post-hoc con bootstrapping
# aunque este no sea necesario.
# PREGUNTA propuesta: La cantidad de hijos (nacidos vivos) promedio �es similar entre hogares
#   de tres regiones de Chile? (metropolitana, tarapaca, la araucania)

#Solucion
#Se filtra por regiones y cantidad de hijos vivos que ha tenido (S4)
#Detalle: S4 es el c�digo correspondiente a "�Cu�ntos hijos nacidos vivos ha tenido en su vida?
hijos <- casen[["s4"]]
regiones <- casen[["region"]]
datos2 <- data.frame(regiones, hijos)

datos2 <- datos2 %>% filter(regiones == "Regi�n Metropolitana de Santiago" | regiones == "Regi�n de Tarapac�"
                            | regiones == "Regi�n de La Araucan�a")

instancia <- 1:nrow(datos2)
datos2 <- cbind(datos2,instancia)

#Se setea la semilla
set.seed(697)

#Se selecciona una muestra de n elementos
n2 <- 500
datos2 <- datos2[sample(nrow(datos2), n2, replace = T), ]

#Se dividen los datos para poder estudiarlos gr�ficamente.
frameMetropolitana <- datos2 %>% filter(regiones == "Regi�n Metropolitana de Santiago")
frameTarapaca <- datos2 %>% filter(regiones == "Regi�n de Tarapac�")
frameAraucania <- datos2 %>% filter(regiones == "Regi�n de La Araucan�a")

#Se realizan gr�ficas que permiten tener una mejor idea de como se distribuyen los
# datos.

#Metropolitana
#Histograma.
histogramaMetropolitana <- gghistogram(frameMetropolitana, 
                                  x = "hijos",
                                  title = "Regi�n Metropolitana",
                                  ylab = "Frecuencia",
                                  bins = 50,
                                  add = "mean",
                                  color = "blue",
                                  fill = "blue"
)

#Gr�fico QQ. (Normalidad)
qqMetropolitana <-   ggqqplot(frameMetropolitana, x = "hijos", y = "regiones", color = "blue") 
qqMetropolitana <-   qqMetropolitana + facet_wrap (~ regiones )
qqMetropolitana <-   qqMetropolitana + rremove("x.ticks") + rremove("x.text") 
qqMetropolitana <-   qqMetropolitana + rremove("y.ticks") + rremove("y.text") 
qqMetropolitana <-   qqMetropolitana + rremove("axis.title")

#Tarapac�
#Histograma.
histogramaTarapaca <- gghistogram(frameTarapaca, 
                                       x = "hijos",
                                       title = "Regi�n de Tarapac�",
                                       ylab = "Frecuencia",
                                       bins = 50,
                                       add = "mean",
                                       color = "green",
                                       fill = "green"
)

#Gr�fico QQ. (Normalidad)
qqTarapaca <-   ggqqplot(frameTarapaca, x = "hijos", y = "regiones", color = "green") 
qqTarapaca <-   qqTarapaca + facet_wrap (~ regiones )
qqTarapaca <-   qqTarapaca + rremove("x.ticks") + rremove("x.text") 
qqTarapaca <-   qqTarapaca + rremove("y.ticks") + rremove("y.text") 
qqTarapaca <-   qqTarapaca + rremove("axis.title")

#Araucan�a
#Histograma.
histogramaAraucania <- gghistogram(frameAraucania, 
                                  x = "hijos",
                                  title = "Regi�n de La Araucan�a",
                                  ylab = "Frecuencia",
                                  bins = 50,
                                  add = "mean",
                                  color = "red",
                                  fill = "red"
)

#Gr�fico QQ. (Normalidad)
qqAraucania <-   ggqqplot(frameAraucania, x = "hijos", y = "regiones", color = "red") 
qqAraucania <-   qqAraucania + facet_wrap (~ regiones )
qqAraucania <-   qqAraucania + rremove("x.ticks") + rremove("x.text") 
qqAraucania <-   qqAraucania + rremove("y.ticks") + rremove("y.text") 
qqAraucania <-   qqAraucania + rremove("axis.title")

figura2 <- ggarrange(histogramaMetropolitana, qqMetropolitana, histogramaTarapaca, qqTarapaca,
                     histogramaAraucania, qqAraucania, ncol = 2, nrow = 3)
print(figura2)

#En base a las gr�ficas y distribuciones generadas, se puede apreciar que los datos son
# problem�ticos puesto que presentan una notable asimetr�a (desviaci�n a la izquierda), 
#con lo que no se aproximan a una distribuci�n normal, por lo que el realizar un m�todo 
# de boostrapping/monte carlo es ideal para este tipo de situaciones.

#Boostrapping#
#Denotando uM al promedio de hijos (nacidos vivos) en hogares en la regi�n metropolitana, uT al promedio 
# hijos (nacidos vivos) en hogares en la regi�n de tarapac� y uA al promedio de hijos (nacidos vivos) 
# en hogares en la regi�n de la Araucan�a.
# Entonces se busca saber si "La cantidad de hijos (nacidos vivos) promedio �es similar entre hogares
# de las tres regiones (mencionadas) de Chile?"

#Se establecen las hip�tesis a contrastar.
#H0: No existe diferencia en la cantidad promedio de hijos (nacidos vivos) en hogares de las tres regiones.
#HA: Existe diferencia en la cantidad promedio de hijos (nacidos vivos) en hogares de las tres regiones.

#Se declara el nivel de significancia para este caso:
alfa <- 0.05

#Se filtran las cantidad de hijos por hogar en relaci�n a la regi�n en que nacieron:
hijos_rm <- datos2 %>% filter(regiones == "Regi�n Metropolitana de Santiago")
n_hijos_rm <- nrow(hijos_rm)
hijos_tp <- datos2 %>% filter(regiones == "Regi�n de Tarapac�")
n_hijos_tp <- nrow(hijos_tp)
hijos_ac <- datos2 %>% filter(regiones == "Regi�n de La Araucan�a")
n_hijos_ac <- nrow(hijos_ac)

my_boot <- function(x){
  #se toma una muestra con reemplazo para cada grupo
  hijos_rm_sample <- sample(1:n_hijos_rm, replace = TRUE) 
  hijos_tp_sample <- sample(1:n_hijos_tp, replace = TRUE)
  hijos_ac_sample <- sample(1:n_hijos_ac, replace = TRUE)
  rbind(hijos_rm[hijos_rm_sample,], hijos_tp[hijos_tp_sample,], hijos_ac[hijos_ac_sample,])
  
}

my_F <- function(frame){
  # Obtener valor observado, correspondiente al estad�stico F entregado
  # por ANOVA para la muestra original
  anova <- ezANOVA(frame, dv = hijos, between = regiones, 
                   wid = instancia, return_aov = FALSE)
  invisible(anova$ANOVA$F)
}

# Se obtiene el estad�stico F original
anova_original <- ezANOVA(datos2, dv = hijos, between = regiones, 
                          wid = instancia, return_aov = FALSE)


distribucion  <- lapply(1:2000, my_boot)
#guarda en un vector
suppressMessages(suppressWarnings(Fs <- sapply(distribucion, my_F))) # evitar los warnings
#Fs <- sapply(distribucion1, my_F)

p <- calcular_valor_p(Fs, anova_original$ANOVA$F, 2000, "two.sided")
print(p)

# p valor obtenido con 2000 repiticiones = 0.8375812 

#En torno al valor obtenido, donde p >> alfa, entonces se falla al rechazar
# la hip�tesis nula, con lo que se concluye con un 95% de confianza que no existe
# diferencia en la cantidad promedio de hijos (nacidos vivos) en hogares de las tres regiones.














