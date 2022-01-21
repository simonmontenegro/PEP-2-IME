library(tidyverse)

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

# Establecer nivel de significacion (el mismo usado en ANOVA)
alfa <- 0.025

# Procedimiento ANOVA
anova <- aov(tiempo ~ algoritmo, data = datos)

# Prueba HSD de Tukey
post_hoc <- TukeyHSD(anova,
                     "algoritmo",
                     ordered = TRUE,
                     conf.level = 1-alfa)
print(post_hoc)

# CONCLUSION
# Solo existe diferencia significativa entre los tiempos promedio de ejecucin de los 
# algoritmos B y C, y se puede concluir que el algoritmo C es mas rapido que el algoritmo B.

# Es posible apreciar en la tabla resultante que la columna "diff" muestra las diferencias 
# de las medias entre grupos, y la columna "p.adj" entrega valores p asociados a cada 
# diferencia, ajustados para compararlos con el nivel de significacion original. Cabe 
# destacar que el unico valor p menor a este nivel (0.025) corresponde a la diferencia B-C. 
# Tambien debemos notar que las columnas "lwr" y "upr" muestran el limite inferior y superior, 
# respectivamente, del intervalo de (1-alfa)x100% confianza para la verdadera diferencia entre 
# las medias de los grupos.