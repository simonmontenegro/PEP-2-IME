#Se setea el directorio donde se encuentra el script
library(dplyr)
library(ggpubr)
library(pwr)

dir <- "~/../Desktop/IME/PEPS anteriores/PE1"
base <- "IME-2020-2-PE1-datos.csv"
arch <- file.path(dir, base)

# Se realiza la lectura de los datos, se escpecifica el formato de codificación UTF-8
datos <- read.csv(arch, fileEncoding = "UTF-8")

set.seed(127)

#Muestras
nSi <- 25
nNo <- 25

#Se realiza la lectura y filtrado de datos
noAnemia <- datos %>% filter(anaemia == 0)
noAnemia <- noAnemia[["creatinine_phosphokinase"]]

siAnemia <- datos %>% filter(anaemia == 1)
siAnemia <- siAnemia[["creatinine_phosphokinase"]]

#Se obtienen las 25 muestras aleatorias
noAnemiaCreatina <- sample(noAnemia,25)
siAnemiaCreatina <- sample(siAnemia,25)

#Se define el nivel de significancia
alfa <- 0.05

#Se realiza la prueba de hipotesis
#H0: No existe diferencia significativa entre los niveles de creatina quinasa en pacientes con/sin anemia
#HA: Si existe diferencia entre los niveles de creatina quinasa entre pacientes con/sin anemia

#Se verifican condiciones para la prueba t
# 1) Las muestras son independientes entre si, puesto que se realiza la observacion sobre
#    pacientes distintos
# 2) Se realiza la prueba Shapiro Wilk
normalidadSiAnemia <- shapiro.test(siAnemiaCreatina)
print(normalidadSiAnemia)
normalidadNoAnemia <- shapiro.test(noAnemiaCreatina)
print(normalidadSiAnemia)

#Se realiza la prueba t
prueba <- t.test(x = siAnemiaCreatina,
                 y = noAnemiaCreatina, 
                 paired = FALSE,
                 alternative = "two.sided", 
                 mu = 0 ,
                 conf.level = 1 - alfa)
print(prueba)

#Se realiza el calculo de d de Cohen para muestras menores a 50
#Se obtienen las medias de las muestras y sus desviaciones estandar
mediaSiAnemia <- mean(siAnemiaCreatina)
mediaNoAnemia <- mean(noAnemiaCreatina)

sdSiAnemia <- sd(siAnemiaCreatina)
sdNoAnemia <- sd(noAnemiaCreatina)

#Se calcula la desviacion estandar agrupada
sp <- sqrt( ((sdSiAnemia^2 * (nSi - 1)) + (sdNoAnemia^2 * (nNo - 1))) / (nSi + nNo - 2))

#Se calcula de la d de Cohen
factorCorreccion <- (nSi + nNo - 3)/(nSi + nNo - 2.25)
d <- ((mediaSiAnemia - mediaNoAnemia)/sp) * factorCorreccion

#Se realiza el calculo del poder
poder <- pwr.t.test(n = 25,
                    d = d,
                    sig.level = alfa,
                    power = NULL,
                    type = "two.sample",
                    alternative = "two.sided")

print(poder)



