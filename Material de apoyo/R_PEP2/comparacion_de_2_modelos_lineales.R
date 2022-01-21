# comparación de dos modelos lineales.

# Cargar datos.
datos <- mtcars

# Ajustar modelo con el peso como predictor.
modelo_1 <- lm(mpg ~ wt, data = datos) 
print(summary(modelo_1))
aic_1 <- AIC(modelo_1)
cat("Modelo 1: AIC =", AIC(modelo_1), "\n")

# Ajustar modelo con el peso y el cuarto de milla como predictores.
modelo_2 <- lm(mpg ~ wt + qsec, data = datos) 
print(summary(modelo_2))
aic_2 <- AIC(modelo_2)
cat("Modelo 2: AIC =", AIC(modelo_2), "\n")

# Comparar ambos modelos.
comparacion <- anova(modelo_1, modelo_2) 
print(comparacion)

# CONCLUSIÓN
# obteniendo como resultado p = 0, 0015. En consecuencia, podemos concluir con 
# 95 % de confianza que la diferencia es significativa, por lo que el segundo modelo es mejor.
# si el valor p obtenido es significativo, entonces el modelo más complejo (con más predictores) es mejor.