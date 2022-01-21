library(tidyverse)
library(ggpubr)
library(ez)
library(nlme) 
library(emmeans)

# Las hipotesis a formular son:
# H0: El tiempo de ejecucion promedio es igual para los cuatro algoritmos
# HA: El tiempo de ejecucion promedio es diferente para al menos un algoritmo

# Verificacion de las condiciones:
# 1. Se verifica esta condicion, puesto que el tiempo, como toda magnitud fisica, tiene 
#    una escala de intervalos iguales.
# 2. El enunciado señala que el proceso seguido por el ingeniero garantiza el cumplimiento 
#    de la segunda condicion.
# 3. Se verifica esta condicion con el grafico Q-Q, donde se puede apreciar que no se observan 
#    valores que pudieran ser considerados atipicos y se puede suponer razonablemente que las 
#    distribuciones se asemejan a la normal.
# 4. Se verifica con la funcion ezANOVA() de R (prueba de esfericidad de Mauchly), donde se puede 
#    ver que las diferencis parecen mas bien "pequeñas" si se considera que los tiempos promedio 
#    estan el rango de 23 a 30[ms], por lo que podriamos asumir que son iguales. Sí se cumple con 
#    la condición de esferidad (hipótesis nula de la prueba de Mauchly).

# Luego, para esta prueba se utilizara un 99% de confianza

# Crear el data frame
instancia <- factor(1:6)
Quicksort <- c(23.2, 22.6, 23.4, 23.3, 21.8, 23.9)
Bubblesort <- c(31.6, 29.3, 30.7, 30.8, 29.8, 30.3)
Radixsort <- c(30.1, 28.4, 28.7, 28.3, 29.9, 29.1)
Mergesort <- c(25.0, 25.7, 25.7, 23.7, 25.5, 24.7)
datos <- data.frame(instancia, Quicksort, Bubblesort, Radixsort, Mergesort)

# Llevar data frame a formato largo
datos <- datos %>% pivot_longer(c("Quicksort", "Bubblesort", "Radixsort", "Mergesort"),
                                names_to = "algoritmo", values_to = "tiempo")

datos[["algoritmo"]] <- factor(datos[["algoritmo"]])

# Comprobacion de normalidad
g <- ggqqplot(datos,
              x = "tiempo",
              y = "algoritmo",
              color = "algoritmo")

g <- g + facet_wrap(~ algoritmo)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Procedimiento ANOVA con aov
cat("Procedimiento ANOVA con aov\n\n")
prueba <- aov(tiempo ~ algoritmo + Error(instancia/(algoritmo)),
              data = datos)
print(summary(prueba))

# Procedimiento ANOVA con ezANOVA()
cat("\n\nProcedimiento ANOVA con ezANOVA\n\n")
prueba2 <- ezANOVA(data = datos, dv = tiempo, within = algoritmo,
                   wid = instancia, return_aov = TRUE)
print(summary(prueba2$aov))
# Correcciones que se emplean cuand se producen violaciones a la condicion e esferidad: la de 
# greenhouse-Geisser y la de Huynd-Feldt. Se destaca que la correción de Greenhouse-Geisser es 
# mas conservadora, tiende a subestimar "e" cuando esta es cercana a 1, por lo que se recomienda 
# su uso para "e"<0.75. Para "e">=0.75 se suele emplear la estimacion de Huynd-Feldt, algo mas liberal.
# Y si los datos del problema no cumplen con la esferidad, se deberia considerar p[GG] como p valor 
# de la prueba, y no el valor de tabla entregada por exANOVA().
cat("\n\nY factores de correcion para cuando no se cumple la\ncondicion de esfericidad:\n\n")
print(prueba2$'Sphericity Corrections')

# Grafico del tamaño del efecto
g2 <- ezPlot(data = datos, dv = tiempo, wid = instancia, within = algoritmo,
             y_lab = "Tiempo promedio de ejecuon [ms]", x = algoritmo)
print(g2)

# El valor p obtenido es muy menor a cualquier nivel de significación típico que se pueda considerar,
# por lo que se rechaza la hipotesis nula en favor de la hipotesis alternativa. Así, el estudiante del ejemplo
# concluye con más de 99% de confianza que existen diferencias significativas entre al menos dos de los 
# algoritmos de ordenamiento comparados.

# PRUEBAS POST-HOC PARA EL EJEMPLO

alfa <- 0.01

# Procedimiento post-hoc de Bonferroni
bonferroni <- pairwise.t.test(datos[["tiempo"]], datos[["algoritmo"]],
                              p.adj = "bonferroni", paired = TRUE)
cat("Corrección de Bonferroni\n")
print(bonferroni)

# Procedimiento post-hoc de Holm
holm <- pairwise.t.test(datos[["tiempo"]], datos[["algoritmo"]], 
                        p.adj = "holm", paired = TRUE)
cat("\n\nCorrección de Holm\n")
print(holm)

# Procedimiento post-hoc HSD de Tukey
mixto <- lme(tiempo ~ algoritmo, data = datos, random = ~1|instancia)
medias <- emmeans(mixto, "algoritmo")
tukey <- pairs(medias, adjust = "tukey")
cat("\n\nPrueba HSD de Tukey\n\n")
print(tukey)

# Procedimiento post-hoc de Scheffé
cat("\n\nComparación de Schefé\n")
scheffe <- pairs(medias, adjunst = "scheffe")
print(scheffe)
