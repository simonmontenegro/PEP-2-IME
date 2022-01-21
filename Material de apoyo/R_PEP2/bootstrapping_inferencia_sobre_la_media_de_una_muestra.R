# Helen desea hacer una prueba de hipótesis para ver si el tiempo promedio de ejecución
# del algoritmo para instancias del tamaño seleccionado (n=10) es mayor a 75 milisegundos. 
# Así, tenemos que:

# Denotando como μ al tiempo medio que tarda el algoritmo de Helen para resolver 
# instancias de tamaño fijo del problema, entonces:
# H0: μ = 75 [ms]
# HA: μ > 75 [ms]


library(boot) 

#alfa <- 0.05
set.seed(432)

# Crear muestra inicial, mostrar su histograma y calcular la media.
muestra <- c(79, 75, 84, 75, 94, 82, 76, 90, 79, 88) 
valor_observado <- mean(muestra)
datos <- data.frame(muestra)

# Construir distribución bootstrap.
B <- 2000

media <- function(valores, i) {
  mean(valores[i])
}

distribucion_b <- boot(muestra, statistic = media, R = B)

# Desplazar la distribución bootstrap para que se centre en el valor nulo.
valor_nulo <- 75
desplazamiento <- mean(distribucion_b[["t"]]) - valor_nulo 
distribucion_nula <- distribucion_b[["t"]] - desplazamiento

# Determinar el valor p.
p <- (sum(distribucion_nula > valor_observado) + 1) / (B + 1) 
cat("Valor p:", p)

# CONCLUSIÓN
# obtenemos que p = 0, 001, menor que el nivel de significación, por lo que la 
# evidencia es suficientemente fuerte para rechazar la hipótesis nula en favor 
# de la hipótesis alternativa. En consecuencia, concluimos con 99,5% de confianza 
# que el tiempo de ejecución promedio del algoritmo para instancias del tamaño 
# seleccionado supera los 75 milisegundos.