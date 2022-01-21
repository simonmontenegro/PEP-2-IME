#Integrantes:
#   Bastián Diaz  (20.433.892-2)
#   William Martinez  (20.426.177-6)
#   Simón Montenegro  (19.794.721-7)

#Se setea el directorio donde se encuentra el script
library(dplyr)
library(ggpubr)
library(gtools)

#EN LAS PROXIMAS 2 LINEAS, USTED DEBE INDICAR EL DIRECTORIO DONDE
#ALMACENA EL ARCHIVO
dir <- "~/../Desktop/IME/Clases Grabadas/Clase 4/actividad"
base <- "Casen 2017.csv"
arch <- file.path(dir, base)

# Se realiza la lectura de los datos, se escpecifica el formato de codificación UTF-8
población <- read.csv(arch, fileEncoding = "UTF-8")

tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )
set.seed(133)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

######################## 6 #########################
#Se define la semilla 25 y numero de repeticiones de 25
set.seed(25)
n.repeticiones <- 25

ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)

cantidad.repeticiones <- sapply(1:n.repeticiones, ensayo)


######################## 7 #########################
#Se genera la distribución binomial
cuenta.exitos <- function(i, k) sum(sample(cantidad.repeticiones, k, replace = TRUE))
bin <- sapply(1:5000, cuenta.exitos, k = 10)

#Se genera la gráfica de la distribución binomial
hist(bin)


######################## 8 #########################
#Se genera la distribución geométrica
#Con match obtenemos el primer exito; recordar que la distribucion geometrica
#corresponde a una binomial con k = 1
cuenta2.exitos <- function(i, k) match(1,sample(cantidad.repeticiones, k, replace = TRUE))
geo <- sapply(1:5000, cuenta2.exitos, k = 10)

#Se genera la gráfica de la distribución geométrica
hist(geo)


######################## 9 #########################
#Se genera la distribución binomial negativa
#Aquí se busca encontrar tres exitos (arbitrario) en 25 repeticiones (preestablecido al inicio)
cuenta3.exitos <- function(i) 
  ifelse(is.na(aux <- which(sample(cantidad.repeticiones, 10, replace = TRUE) %in% c(1,1,1,1,1))[2]), 0, aux)
bin.neg <- sapply(1:5000, cuenta3.exitos)

#Se genera la gráfica de la distribución binomial negativa
hist(bin.neg)

