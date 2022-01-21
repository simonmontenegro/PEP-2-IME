library(tidyverse)
library(ggpubr)
library(ez)
# La libreria "ez" contiene la funcion ezPlot(), la cualpermite ver graficamente el 
# tamaño del efecto medido

# Las hipotesis detras de esta prueba son:
# H0: Las varianzas de las k muestras son iguales
# HA: Al menos una de las muestras tiene varianza diferente a alguna de las demas

# Verificacion de las hipotesis:
# 1. La primera condicion se verifica, puesto que si para una misma instancia i un algoritmo 
#    tarda 20[ms] mientras que otro algoritmo tarda 30[ms], esa es la misma diferencia 
#    que se presenta para una instancia j en que uno tarda 35[ms] y el otro 45[ms].
# 2. A su vez, el enunciado señala que el proceso seguido por el ingeniero garantiza el cumplimiento 
#    de la segunda condicion.
# 3. Se verifica la distribucon normal de las tres muestras a partir del grafico Q-Q, donde se 
#    observan algunos valores que podrian ser atipicos  las muestras son pequeñas, es mejor que 
#    procedamos con cautela y usemos un nivel de significacion igual a 0.025
# 4. Para comprobar esta condicion se utiliza la regla de homogeneidad de las varianzas u 
#    homocedasticidad, en donde se comprueba que la razon entre la max y min varianza muestral 
#    de los grupos resulta 1.117, valor menor a 1.5, por tanto se cumple la condicion de homocedasticidad.

# Se define un alfa igual a 0.025

# Crear el data frame formato ancho
A <- c(23, 19, 25, 23, 20)
B <- c(26, 24, 28, 23, 29)
C <- c(19, 24, 20, 21, 17)
datos <- data.frame(A, B, C)

# Llevar data frame a formato largo
datos <- datos %>% pivot_longer(c("A", "B", "C"),
                                names_to = "algoritmo",
                                values_to = "tiempo")

datos [["algoritmo"]] <- factor(datos[["algoritmo"]])
datos[["instancia"]] <- factor(1:nrow(datos))

# Comprobacion de normalidad (condicion 3)
g <- ggqqplot(datos,
              x = "tiempo",
              y = "algoritmo",
              color = "algoritmo")

g <- g + facet_wrap(~ algoritmo)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Comprobacion de homocedasticidad (condicion 4)
muestra_A <- datos %>% filter(algoritmo == "A")
muestra_A <- muestra_A[["tiempo"]]
var_A <- var(muestra_A)

muestra_B <- datos %>% filter(algoritmo == "B")
muestra_B <- muestra_B[["tiempo"]]
var_B <- var(muestra_B)

muestra_C <- datos %>% filter(algoritmo == "C")
muestra_C <- muestra_C[["tiempo"]]
var_C <- var(muestra_C)

homocedasticidad <- var_C/var_A
  
# Procedimiento ANOVA con aov()
cat("Procedimiento ANOVA con aov\n\n")
prueba <- aov(tiempo ~ algoritmo, data = datos)
print(summary(prueba))

# Procedimiento ANOVA con ezANOVA()
# La ventaja de ezanova POR SOBRE AOV() es que, ademas de ejecutar la prueba ANOVA,
# realiza tambien la prueba de homocedasticidad de Levene (comprueba si k muestras
# tienen igual varianza)
cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
prueba2 <- ezANOVA(
  data = datos,
  dv = tiempo,
  between = algoritmo,
  wid = instancia,
  return_aov = TRUE)

print(prueba2)

# Grafico del tamaño del efecto
g2 <- ezPlot(
  data = datos,
  dv = tiempo,
  wid = instancia,
  between = algoritmo,
  y_lab = "Tiempo promedio de ejecucion [ms]",
  x = algoritmo)

print(g2)

# CONCLUSION
# Como el valor p resulta 0.0102, lo cual es menor al nivel de significacion igual a 0.025, 
# se rechaza la hipotesis nula en favor de la hipotesis alternativa. En consecuencia, se puede 
# concluir con un 97.5% de confianza que el tiempo de ejecucion promedio es diferente para al 
# menos uno de los algoritmos comparados.
