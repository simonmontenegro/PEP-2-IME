# Ajuste y evaluacion del mejor modelo para predecir el tipo de transmision d eun automovil

library(car)

set.seed(1313)

# Cargar los datos
datos <- mtcars
am <- factor(datos$am)
datos$am <- NULL
datos <- cbind(am, datos)

# Separar conjuntos de entrenamiento y prueba
n <- nrow(datos)
n_entrenamiento <- floor(0.8 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- datos[muestra, ]
prueba <- datos[-muestra, ]

# Ajustar modelo nulo
nulo <- glm(am ~ 1, family = binomial(link = "logit"), data = entrenamiento)

# Ajustar modelocompleto
cat("\n\n")
completo <- glm(am ~ ., family = binomial(link = "logit"),
                data = entrenamiento)

# Ajustar modelo con regresion escalonada
cat("Modelo con regresion escalonada\n")
cat("-------------------------------\n")
mejor <- step(nulo, scope = list(lowe = nulo, upper = completo),
              direction = "both", trace = 0)
print(summary(mejor))

# Verificacion de multicolinealidad
cat("Verificacion de multicolinealidad\n")
cat("---------------------------------\n")
cat("\nVIF:\n")
vifs <- vif(mejor)
print(vifs)
cat("\nPromedio VIF: ")
print(mean(vifs))

# Ajustar modelo con el peso como preductor
cat("modelo con el peso como predictor\n")
cat("---------------------------------\n")
modelo_peso <- glm(am ~ wt, family = binomial(link = "logit"),
                       data = entrenamiento)
print(summary(modelo_peso))

# Ajustar modelo con la potencia como predictor
cat("modelo con la potencia como predictor\n")
cat("---------------------------------\n")
modelo_potencia <- glm(am ~ hp, family = binomial(link = "logit"),
                       data = entrenamiento)
print(summary(modelo_potencia))

# Se puede ver que ninfuna de las varibales presenta un VIF superior a 10, el 
# promedio es bastante superior a 1, lo que confirma que le modelo puede tener 
# problemas, En consecuencia, no es recomendable usar este modelo.
# Se recomienda eliminar la variable con mayor VIF, pero en este caso son iguales.

# Comparar los modelos con el peso y la potencia como predictores
cat("\n\n")
cat("Likelihood Ratio Test para los modelos\n")
cat("--------------------------------------\n")
print(anova(modelo_peso, modelo_potencia, test = "LRT"))

# A modo de ejercicio,comparar el modelo obtenido mediante
# regresion escalonada con el que solo tiene el peso como predictor
cat("\n\n")
cat("Likelihood Ratio Test para los modelos\n")
cat("--------------------------------------\n")
print(anova(modelo_peso, mejor, test = "LRT"))
# En este caso la prueba anova no sirve, pues ambos modeos tiene igual cant de 
# predictores y no entrega un valor p. Sin embargo, podemos ver que el VIF del
# modelo con la potencia como prectir (37.444) es mas alto que para el modelo 
# con el peso como predictor (16.23). En consecuencia, este ultimo es mejor.


# Independencia de los residuos
cat("Verificacion de independencia de los residuos\n")
cat("---------------------------------------------\n")
print(durbinWatsonTest(modelo_peso, max.lag = 5))

# Detectar posibles valores atipicos
cat("Identificacion de posibles valores atipicos\n")
cat("-------------------------------------------\n")
plot(mejor)

# Obtener los residuos y las estadisticas
output <- data.frame(predicted.probabilities = fitted(modelo_peso))
output[["standardized.residuals"]] <- rstandard(modelo_peso)
output[["studentized.residuals"]] <- rstudent(modelo_peso)
output[["cooks.distance"]] <- cooks.distance(modelo_peso)
output[["dfbeta"]] <- dfbeta(modelo_peso)
output[["dffit"]] <- dffits(modelo_peso)
output[["leverage"]] <- hatvalues(modelo_peso)

# Evaluar residuos estandarizados que escapen a la normalidad.
# 95% de os residuos estandarizados deberian estar entre
# -1.96 y 1.96, y un 99% entre -2.58 y 2.58
sospechosos1 <- which(abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos2)
cat("\n\n")
cat("Residuos estandarizados fuera del 95% esperado\n")
cat("----------------------------------------------\n")
print(rownames(entrenamiento[sospechosos1, ]))

# Revisar casos con distancia de Cook mayor a uno
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat("\n\n")
cat("Residuos con una distancia de Cook alta\n")
cat("---------------------------------------\n")
print(rownames(entrenamiento[sospechosos2, ]))

# Revisar casos cuyo apalancamiento sea mas del doble
# o triple del apalancamiento promedio
leverage.promedio <- ncol(entrenamiento) / nrow(datos)
sospechosos3 <- which(output[["leverage"]] > leverage.promedio)
sospechosos3 <- sort(sospechosos3)
cat("\n\n")
cat("Residuales con levarage fuera de rango (> ")
cat("round(leverage.promedio, 3", ")", "\n", sep = "")
cat("--------------------------------------\n")
print(rownames(entrenamiento[sospechosos3, ]))

# Revisar casos con DFBeta >= 1
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1, 1, any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4) <- NULL
cat("\n\n")
cat("Residuales con DFBeta sobre 1\n")
cat("-----------------------------\n")
print(rownames(entrenamiento[sospechosos4, ]))

# Detalle de las observaciones posiblemente atipicas
sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4)
sospechosos <- sort(unique(sospechosos))
cat("\n\n")
cat("Casos sospechosos\n")
cat("-----------------\n")
print(entrenamient[sospechosos, ])
cat("\n\n")
print(output[sospechosos, ])

# CONCLUSIONES

# Al verificar los factores de inflación de la varianza de los predictores podemos apreciar que, 
# si bien ninguna de las variables presenta un VIF superior a 10, el promedio es bastante 
# superior a 1, lo que confirma que el modelo puede tener problemas. En consecuencia, no es 
# recomendable usar este modelo.

# Por regla general, se recomienda eliminar la variable con mayor VIF , pero en este caso ambos 
# son iguales. En consecuencia, se ajustan los dos modelos posibles Y LUEGO SE COMPARAN.
# Pero en este caso la prueba ANOVA no sirve, pues ambos modelos tienen igual cantidad de predictores 
# y no entrega un valor p. Sin embargo, podemos ver que el VIF del modelo con la potencia como predictor 
# (VIF = 37, 444) es más alto que para el modelo con el peso como predictor (VIF = 16, 23). En consecuencia, 
# este último parece ser mejor.

# A modo de ejercicio, a pesar de que lo descartamos por tener problemas de colinealidad, comparamos 
# el modelo obtenido mediante regresión escalonada con el que tiene al peso como variable predictora, 
# obteniendo como resultado un valor p < 0, 001. Puesto que el valor p obtenido es 
# significativo, la prueba arroja que el modelo más complejo (es decir, el que tiene dos predictores) 
# reduce la varianza de los residuos de forma significativa (¡llegando a cero!).

# Sin embargo, aún resta verificar el cumplimiento de la condición de independencia de los residuos, 
# para lo cual, al igual que con modelos de regresión lineal, empleamos la prueba de Durbin-Watson 
# (script 15.3, línea 75), cuyo resultado mostramos en la figura 15.15, donde podemos notar que, 
# aunque cerca del borde para α = 0, 05, los residuos son independientes.

# Una vez verificadas las condiciones, podemos concluir que el modelo es adecuado y puede ser generalizado.