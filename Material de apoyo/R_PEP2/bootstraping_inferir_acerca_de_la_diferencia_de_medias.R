# Supongamos ahora que el estudio del ejemplo desea determinar, con un nivel de 
# significación α = 0, 05, si la diferencia entre las calificaciones finales de 
# hombres y mujeres es igual a 1,5 puntos. Para ello, formulamos las siguientes 
# hipótesis:

# Sean μh y μm las calificaciones finales de hombres y mujeres, respectivamente, 
# que rinden una asignatura inicial de programación por primera vez en la 
# Universidad en estudio, entonces:
# H0: μh − μm =1,5
# HA: μh − μm ̸=1,5

library(simpleboot) 
library(boot) 
library(ggpubr)

set.seed(432)

# Ingresar datos originales
hombres <- c(1.3, 1.5, 1.6, 1.7, 1.7, 1.9, 2.3, 2.4,2.6, 2.6, 2.7,
             2.8, 3.2, 3.7, 4.1 , 4.4 , 4.5, 4.8, 5.2,5.2, 5.3, 5.5,
             5.5, 5.6, 5.6,5.7, 5.7)

mujeres <- c(3.5, 3.6, 3.8,4.3, 4.5,4.5, 4.9, 5.1,5.3, 5.3, 5.5,
             5.8, 6.0, 6.3, 6.3 , 6.4 ,6.4 , 6.6 , 6.7)

n_hombres <- length(hombres)
n_mujeres <- length(mujeres)

sexo <- c(rep("Hombre", n_hombres), rep("Mujer", n_mujeres))
nota <- c(hombres , mujeres)
datos <- data.frame(nota, sexo)

# Calcular la diferencia observada entre las medias muestrales.
media_hombres <- mean(hombres)
media_mujeres <- mean(mujeres)
valor_observado <- media_hombres - media_mujeres

# Crear la distribución bootstrap.
B <- 9999
valor_nulo <- 1.5
distribucion_bootstrap <- two.boot(hombres, mujeres, FUN = mean, R = B) 
desplazamiento <- mean(distribucion_bootstrap[["t"]]) - valor_nulo 
distribucion_nula <- distribucion_bootstrap[["t"]] - desplazamiento

# Determinar el valor p.
p <- (sum(abs(distribucion_nula) > abs(valor_observado)) + 1) / (B + 1) 
cat("Valor p:", p)

# CONCLUSIÓN
# obtenemos un valor p de p = 0,364, superior al nivel de significación, por lo 
# que fallamos al rechazar la hipótesis nula. En consecuencia, concluimos con 
# 95 % de confianza que la diferencia en la calificación final entre hombres y 
# mujeres es de 1,5 puntos.