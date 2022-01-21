# Las hipótesis a formular son:
# H0: No hay diferencia en la usabilidad de ambas interfaces (se distribuyen de igual forma)
# HA: Sí hay diferencia en la usablidad de ambas interfaces (distribuciones distintas)

# Se verifican las condiciones:
# 1. Las observaciones de ambas muestras son independientes.
# 2. La escala de medición empleada es ordinal, ya que se tienen valores del 1 al 7,
#    donde 1 significa muy malo, y 7 muy bueno.

# Ingresar los datos
a <- c(2.7, 6.6, 1.6, 5.1, 3.7, 6.1, 5.0, 1.4, 1.8, 1.5, 3.0, 5.3)
b <- c(5.0, 1.4, 5.6, 4.6, 6.7, 2.7, 1.3, 6.3, 3.7, 1.3, 6.8)

# Establecer nivel de significacion
alfa <- 0.05

# Hacer la prueba de Mann-Withney
prueba <- wilcox.test(a, b, alternative = "two.sided", conf.level = 1 - alfa)
print(prueba)

# CONCLUSIÓN:
# Se obtiene un valor p igual a 0.781, lo cual resulta mayor que nuestro alfa, por tanto,
# se falla en rechazar la hipótesis nula, por lo que se concluye con un 95% de confianza
# que no hay diferencia en la usabilidad de ambas interfaces.