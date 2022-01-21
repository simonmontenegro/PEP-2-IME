#Se setea el directorio donde se encuentra el script
library(dplyr)
library(ggpubr)
library(pwr)

dir <- "~/../Desktop/IME/PEP_1_IME"
base <- "Datos PEP 1.csv"
arch <- file.path(dir, base)

# Se realiza la lectura de los datos, se escpecifica el formato de codificación UTF-8
datos <- read.csv2(arch, fileEncoding = "UTF-8")

#Se setea la semilla
set.seed(523)

#Se filtra por planeta
planetaC <- datos %>% filter(planeta == "Coruscant")
planetaT <- datos %>% filter(planeta == "Tatooine")
 

#Se obtienen las 25 muestras aleatorias
estaturaC <- planetaC[["estatura"]]
estaturaT <- planetaT[["estatura"]]

#Se obtiene la muestra de 40 reclutas
muestraEstaturaC <-  sample(estaturaC,40)
muestraEstaturaT <-  sample(estaturaT,40)

#Se define el nivel de significancia
alfa <- 0.05

#Dado que se busca comprobar acerca de la diferencia PROMEDIO de dos muestras pareadas (ya que se esta
# comparando entre diferencias de dos planetas), se utiliza
# la prueba t para dos muestras independientes, esto se sustenta además con que no se cuenta con la 
# desviación estandar POBLACIONAL, por lo que usar una prueba Z no tiene sentido.

#Se verifican condiciones para la prueba t
# 1) Las muestras son independientes entre si, puesto que se realiza la observacion sobre
#    reclutas distintos
# 2) Se realiza la prueba Shapiro Wilk
normalidadC <- shapiro.test(muestraEstaturaC)
print(normalidadC)
normalidadT <- shapiro.test(muestraEstaturaT)
print(normalidadT)

#Con esto, se comprueba que el resultado de Shapiro es mayor que el nivel de significancia, por lo que
# se comprueba que se acerca razonablemente a una distribucion normal 
# (p = 0.1721 para planetaC y p = 0.7935 para planetaT), muy superiores al nivel de significancia 0.05

#Se procede con la prueba de hipotesis
#Se declaran las hipotesis:
#H0: la media de las diferencias de las estaturas es igual a 0
#HA: la media de las diferencias de las estaturas es distinta de 0

#Es decir

#H0: uDif = 0
#HA: uDif != 0

#Se declara el valor nulo, en este caso, como se busca que no exista diferencia el valor nulo es 0
valor_nulo <- 0

#Se calcula la diferencia
diferencia <- muestraEstaturaC - muestraEstaturaT

pruebaT <- t.test( diferencia ,
                    alternative = "two.sided",
                    mu   = valor_nulo,
                    conf.level   = 1 - alfa)
print(pruebaT)

#El resultado obtenido muestra un p-value = 0.713, donde p > alfa, es decir p >> 0.05, por lo que se falla
# al rechazar la hipotesis nula, es decir,
# Se puede afirmar con un 95% de confianza que la media de las diferencias de estaturas es igual  0.
















