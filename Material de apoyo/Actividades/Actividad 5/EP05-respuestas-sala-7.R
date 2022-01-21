library(ggpubr)
library(pwr)
library(ggplot2)
library(tidyverse)

#---------- ENUNCUADO SALA 7 ----------
#Se sabe que el proceso de fabricación de barras de acero para concreto reforzado producen barras con
#medidas de dureza que siguen una distribución normal con desviación estándar de 10 kilogramos de
#fuerza por milímetro cuadrado. Usando una muestra aleatoria de tamaño 25, un ingeniero quiere averiguar
#si una línea de producción está generando barras con dureza media de 170 [kgf mm-2]

#---------- PREGUNTA 1 ----------
cat ("---------- PREGUNTA 1 ----------\n")
#Si el ingeniero está seguro que la verdadera dureza media no puede ser menor a los 170 [kgf mm-2] y
#piensa rechazar la hipótesis nula cuando la muestra presente una media mayor a 174 [kgf mm-2], ¿cuál es
#la probabilidad de que cometa un error de tipo 1?

#fijar valores conocidos
sigma <- 10 #desviacion estandar
alpha <- 0.05 #nivel de significacion
n <- 25
media_nula <- 170


#calcular el error estandar
SE <- sigma/sqrt(n)

#construyendo data.frame


valorz <- qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)
# el retorno de qnorm es un z = 2.575829 ~ 2.58
z <- valorz

#graficar la distribucion muestral de la media de las diferencias si la hipotesis nula fuera verdadera
x <- seq(media_nula-sigma, media_nula+sigma, alpha)
y <- dnorm(x, mean=media_nula, sd=SE)
datos <- data.frame(x, y)
g <- ggplot(data=data.frame(x, y), aes(x))

g <- g + stat_function(fun=dnorm, args=list(mean=media_nula, sd=SE), colour="red", size=1)

g <- g + ylab("")
g <- g + scale_y_continuous(breaks=NULL)
g <- g + scale_x_continuous(name="dureza media de las barras", breaks=seq(160, 180, 2))

g <- g + theme_pubr()


g <- g + geom_area(data = subset(datos, x > 174), aes(y = y),
                                 colour = "red", fill = "red", alpha = 0.5)

print(g)

# Area cola superior
area_superior <- pnorm(174, mean = media_nula, sd = SE, lower.tail = FALSE)*100

cat("La probabilidad de cometer un error del tipo I es: ", area_superior, "%\n")

#---------- PREGUNTA 2 ----------
cat ("---------- PREGUNTA 2 ----------\n")
#Si la verdadera dureza media de la línea de producción fuera 173 [kgf mm-2], ¿cuál sería la probabilidad de
#que el ingeniero, que obviamente no conoce este dato, cometa un error de tipo 2?
#superponer la distribucon muestral de la media de las diferencias si la diferencia de medias fuera 173

media_efecto <- 173



#colorear la region de la nueva curva situada en la region de rechazo de la curva original
x1 <- seq(media_nula-sigma, media_nula+sigma, alpha)

y1 <- dnorm(x, mean=media_efecto, sd=SE)
g2 <- ggplot(data=data.frame(x1, y1), aes(x))
g2 <- g + stat_function(fun=dnorm, args=list(mean=media_efecto, sd=SE), colour="blue", size=1)
g2 <- g2 + geom_area(data=subset(data.frame(x1, y1), x<174), aes(x=x1, y=y1), colour="blue", fill="blue", alpha=0.5)

print(g2)


#calcular el poder de acuerdo al analisis teorico
poder_teorico <- pnorm(174, mean=media_efecto, sd=SE, lower.tail=TRUE)


#calcular la prob de cometer un error tipo II
beta <- (1-poder_teorico)*100

cat("La probabilidad de cometer un error del tipo II es: ", beta, "%\n")

#---------- PREGUNTA 3 ----------
cat ("---------- PREGUNTA 3 ----------\n")
#Como no se conoce la verdadera dureza media, genere un gráfico del poder estadístico con las
#condiciones anteriores, pero suponiendo que las verdaderas durezas medias podrían variar de 170 a 178
#[kgf mm-2].

#generar vector con un rango de valores para el efecto de medias
efecto <- seq(170,178, 0.01)

#calcular el poder para una prueba t bilateral, para cada tamaño del efecto,
#asumiendo una muestra con desv igual a 1.

#una muestra de tamaño 6 y un nivel de significacion 0.05
calculo_poder <- power.t.test(n=25,
                              delta=efecto-173,
                              sd=2,
                              sig.level=0.022,
                              type="one.sample",
                              alternative="one.sided")$power

#cnstruir matriz de datos en formato ancho
datos <- data.frame(efecto, calculo_poder)

#llevar a formato largo
datos <- datos %>% pivot_longer(!"efecto", names_to="fuente", values_to="poder")

#formatear fuente como variable categorica
niveles <- c("calculo_poder")

etiquetas <- c("n=25, alfa=0,022")

datos[["fuente"]] <- factor(datos[["fuente"]], levels=niveles, labels=etiquetas)

#graficar las curvas de poder
g <- ggplot(datos, aes(efecto, poder, colour=factor(fuente)))
g <- g + geom_line()
g <- g + labs(colour="")
g <- g + ylab("poder estadistico")
g <- g + xlab("tamaño del efecto")

g <- g + scale_color_manual(values=c("red"))

g <- g + theme_pubr()
g <- g + ggtitle("curvas de poder para prueba t unilateral (pregunta 3)")
g <- g + geom_vline(xintercept=173, linetype="dashed")

print(g)
cat("Ver Grafico en Secci�n Plots \n")


#---------- PREGUNTA 4 ----------
cat ("---------- PREGUNTA 4 ----------\n")
#¿Cuántas barras deberían revisarse para conseguir un poder estadístico de 0,85 y un nivel de significación
#de 0,05?

#calculando d de Cohen:
d_cohen <- (media_efecto - media_nula)/sigma
d_cohen
# Aplicando la formula "pwr.t.test":
nEncontrado <- pwr.t.test(n = NULL,
                              d = d_cohen,
                              sig.level = 0.05,
                              power = 0.85,
                              type = "one.sample",
                              alternative = "greater")

valor <- ceiling(nEncontrado[["n"]])
valor
cat("La cantidad de barras N a revisar son aproximadamente: ",valor, "\n")

  
#---------- PREGUNTA 5 ----------
cat ("---------- PREGUNTA 5 ----------\n")
#¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error de tipo 1 a un 1% solamente?

#fijar valores conocidos
sigma2 <- 10 #desviacion estandar
alpha2 <- 0.05 #nivel de significacion
n2 <- 34
media_nula2 <- 170


#calcular el error estandar
SE2 <- sigma2/sqrt(n2)

#construyendo data.frame

# Area cola superior
area_superior2 <- pnorm(174, mean = media_nula2, sd = SE2, lower.tail = FALSE)*100

cat("Con n = " , n2, "la probabilidad de cometer un error del tipo I es: ", area_superior2, "%\n")

