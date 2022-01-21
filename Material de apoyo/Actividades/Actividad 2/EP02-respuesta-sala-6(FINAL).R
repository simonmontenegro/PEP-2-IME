
#Se asume que el archivo a leer y este documento están en el escritorio

#Se setea el directorio donde se encuentra el script
library(dplyr)
library(ggpubr)
dir <- "~/../Desktop"
base <- "Casen 2017.csv"
arch <- file.path(dir, base)

# Se realiza la lectura de los datos, se escpecifica el formato de codificación UTF-8
datos <- read.csv(arch, fileEncoding = "UTF-8")

#Se realiza el filtro de mujeres en los datos

cuartiles <- quantile(datos[["ytot"]], na.rm = TRUE)

datos2 <- datos %>% filter(sexo == "Mujer",region == "Región Metropolitana de Santiago", ytot>cuartiles[2] & ytot < cuartiles[4])

#Se grafican los datos, edad vs ingresos
g <- ggscatter(datos2,
               x = "edad",
               y = "ytot"
               )

print(g)
#Para responder la pregunta se escogen como medidas la covarianza, pues permite calcular la relación lineal entre dos variables (Fuente: https://www.cienciadedatos.net/documentos/pystats05-correlacion-lineal-python.html); y por otro lado los rangos intercuartiles,
#con los cuales es posible separar los datos en grupos más pequeños.
medidas_respuestas <-datos2 %>% summarise(Covarianza = cov(edad, ytot))

#En cuanto al gráfico, se decide usar un gráfico de dispersión, pero dada la existencia de datos atípico (ingresos muy grandes o muy pequeños; fuera del común)
#que dificultan el análisis del conjunto de datos y cayendo en imprecisión, se usan los datos que se encuentran en el rango intercuartílico. 
#Si se analiza este grafico, no se logra apreciar una relacion clara entre las variables edad e ingreso, por lo cual se concluye que son variables independientes.


