# Las hipótesis a formular con:
# H0: Las mismas personas no perciben diferencia en la usabilidad de ambas interfaces
# HA: Las mismas personas consideran que la interfaz A tiene mejor usabilidad que la interfaz B

# Las condiciones a verificar son:
# 1. Los pares de observaciones son independientes.
# 2. La escala de medición empleada ara las observaciones es intrinsecamente continua.
# 3. La escala de medición empleada es ordinal, ya que se tienen valores del 1 al 7,
#    donde 1 significa muy malo, y 7 muy bueno.

# Ingresar los datos
a <- c(2.9, 6.1, 6.7, 4.7, 6.4, 5.7, 2.7, 6.9, 1.7, 6.4)
b <- c(6.0, 2.8, 1.3, 4.7, 3.1, 1.8, 2.9, 4.0, 2.3, 1.6)

# Establecer nivel de significación
alfa <- 0.05

# Hacer la prueba de rangos con signo de Wilcoxon
prueba <- wilcox.test(a, b, alternative = "greater", paired = TRUE, conf.level = 1-alfa)
print(prueba)

# CONCLUSIÓN
# Se rechaza la hipótesis nula en favor de la hipótesis alternativa. En consecuencia, se 
# concluye con un 95% de confianza que la usabilidad de la interfaz A es mejor que la de 
# la interfaz B.