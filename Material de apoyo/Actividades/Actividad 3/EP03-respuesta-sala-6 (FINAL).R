#Integrantes:
#   Basti�n Diaz  (20.433.892-2)
#   William Martinez  (20.426.177-6)
#   Sim�n Montenegro  (19.794.721-7)

#Se setea el directorio donde se encuentra el script
library(dplyr)
library(ggpubr)
library(gtools)

#EN LAS PROXIMAS 2 LINEAS, USTED DEBE INDICAR EL DIRECTORIO DONDE
#ALMACENA EL ARCHIVO
dir <- "~/../Desktop/IME/Clases Grabadas/Clase 4/actividad"
base <- "Casen 2017.csv"
arch <- file.path(dir, base)

# Se realiza la lectura de los datos, se escpecifica el formato de codificaci�n UTF-8
poblaci�n <- read.csv(arch, fileEncoding = "UTF-8")

tama�o <- nrow(poblaci�n)
ingreso <- as.numeric(poblaci�n[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tama�o.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tama�o.podado )
set.seed(133)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

######################## 6 #########################
#Se define la semilla 25 y numero de repeticiones de 25
set.seed(25)
n.repeticiones <- 25

ensayo <- function(x)
  ifelse(sample(poblaci�n[["sexo"]], 1) == "Mujer", 1, 0)

cantidad.repeticiones <- sapply(1:n.repeticiones, ensayo)


######################## 7 #########################
#Se genera la distribuci�n binomial
cuenta.exitos <- function(i, k) sum(sample(cantidad.repeticiones, k, replace = TRUE))
bin <- sapply(1:5000, cuenta.exitos, k = 10)

#Se genera la gr�fica de la distribuci�n binomial
hist(bin)


######################## 8 #########################
#Se genera la distribuci�n geom�trica
#Con match obtenemos el primer exito; recordar que la distribucion geometrica
#corresponde a una binomial con k = 1
cuenta2.exitos <- function(i, k) match(1,sample(cantidad.repeticiones, k, replace = TRUE))
geo <- sapply(1:5000, cuenta2.exitos, k = 10)

#Se genera la gr�fica de la distribuci�n geom�trica
hist(geo)


######################## 9 #########################
#Se genera la distribuci�n binomial negativa
#Aqu� se busca encontrar tres exitos (arbitrario) en 25 repeticiones (preestablecido al inicio)
cuenta3.exitos <- function(i) 
  ifelse(is.na(aux <- which(sample(cantidad.repeticiones, 10, replace = TRUE) %in% c(1,1,1,1,1))[2]), 0, aux)
bin.neg <- sapply(1:5000, cuenta3.exitos)

#Se genera la gr�fica de la distribuci�n binomial negativa
hist(bin.neg)

