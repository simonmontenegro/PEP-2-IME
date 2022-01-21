library(tidyverse)
library(DescTools)

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

# PREGUNTAS:
# 1. ¿Existe diferencia entre los tiempos de ejecucion de los algoritmos A y B?
# 2. ¿Es el tiempo promedio de ejecucion del algoritmo A distinto al tiempo de 
#    ejecucion promedio de los algoritmos B y C?

# Las hipotesis a formular son:
# (1)
# H0: Ua - Ub = 0
# HA: Ua - Ub != 0
# ------------------------
# 1xUa -1xUb +0xUc = 0

#(2)
# H0: Ua - ((Ub+Uc)/2) = 0
# HA: Ua - ((Ub+Uc)/2) != 0
# ------------------------
# 1xUa -1/2xUb -1/2xUc = 0

# Crear matriz de contrastes
contrastes <- matrix(c(1, -1, 0, 
                       1, 0, -1, 
                       0, 1, -1, 
                       1, -0.5, -0.5,
                       -0.5, 1, -0.5,
                       -0.5, -0.5, 1),
                     nrow = 6,
                     byrow = TRUE)

# Trasponer matriz de contrastes (para que cada contraste sea una columna)
contrastes <- t(contrastes)
 
# Hacer prueba de Scheffe
scheffe <- ScheffeTest(x = anova,
                       which = "algoritmo",
                       contrasts = contrastes,
                       conf.level = 1-alfa)
print(scheffe)

# OBS: 
# Se puede hacer la llamada "Scheffetest()" sin entregar los argumentos "which" y "contrasts", en cuyo
# caso unicamente se contrastan todos los pares, como en las pruebas post-hoc precedentes.

# La funcion "Scheffetest()" nos entrega un valor p ajustado para cada contraste e 
# identifica aquellos que son relevantes para diferentes niveles de significacion. 
# Se destaca que las columnas "lwr" y "upr" señalan los limites del intervalo de 
# confianza para la verdadera diferencia entre las medias de los grupos.

# CONCLUSION
# 1. Existe una diferencia significativa entre las eficienicas de los algoritmos B y C.
# 2. El tiempo promedio de ejecucion del algoritmo C es distinto del tiempo promedio de 
#    ejecucion (combinado) de los algoritmos A y B.