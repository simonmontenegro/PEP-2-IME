# Ahora la Universidad del ejemplo desea saber si la diferencia entre las 
# calificaciones obtenidas en la primera y la segunda prueba de un curso inicial de 
# programación es de 5 décimas. Así, considerando un nivel de significación α = 0, 05, 
# los investigadores formulan las siguientes hipótesis:
# H0: μdif =0,5 
# HA: μdif ̸=0,5
library(bootES) 

set.seed(432)

# Ingresar datos originales.
alumno <- 1:20

prueba_1 <- c(3.5, 2.7, 1.0, 1.8, 1.6, 4.3, 5.8, 6.4, 3.9, 4.3, 3.4,
              5.3, 5.8, 5.3, 2.0, 1.3, 4.0, 5.3, 1.6, 3.6)

prueba_2 <- c(5.2, 5.1, 5.9, 4.8, 1.4, 2.3, 6.8, 5.3, 3.1, 3.8, 4.6,
              1.2, 3.9, 2.0, 1.7, 3.3, 6.0, 4.8, 6.9, 1.3)

# Establecer nivel de significación.
alfa <- 0.05

# Calcular la diferencia entre ambas observaciones.
diferencia <- prueba_2 - prueba_1

# Calcular la media observada de las diferencias.
valor_observado <- mean(diferencia)

# Generar la distribución bootstrap y su intervalo de confianza.
B <- 3999 
valor_nulo <- 0.5

distribucion_bootstrapES <- bootES(diferencia, R = B, ci.type = "bca", 
                                   ci.conf = 1 - alfa, plot = FALSE)

distribucion_nula <- distribucion_bootstrapES[["t"]] - valor_nulo

# Determinar el valor p.
p <- (sum(abs(distribucion_nula) > abs(valor_observado)) + 1) / (B + 1) 
cat("Valor p:", p)

# CONCLUSIÓN
# obtienen un valor p de p = 0, 573, por lo que la evidencia no es suficientemente 
# fuerte como para rechazar la hipótesis nula. En consecuencia, los investiadores 
# concluyen con 95 % de confianza que la diferencia de las calificaciones obtenidas 
# en ambas evaluaciones es de 5 décimas.