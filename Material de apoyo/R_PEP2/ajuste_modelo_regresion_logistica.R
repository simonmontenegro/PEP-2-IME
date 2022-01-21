library(pROC)
library(caret)

set.seed(1313)

# Cargar ls datos
datos <- mtcars
datos$am <- factor(datos$am)

# Separar conjuntos de entrenamiento y prueba
n <- nrow(datos)
# Se considera un conjunto de entrenamiento con 80% de las instancias
n_entrenamiento <- floor(0.8 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- datos[muestra, ]
prueba <- datos[-muestra, ]

# Ajustar modelo
# la función glm() para ajustar un modelo de regresión logística que prediga el 
# tipo de transmisión de un automóvil (0 = automática, 1 = manual) a partir de su peso
modelo <- glm(am ~ wt, family = binomial(link = "logit"), data = entrenamiento)
print(summary(modelo))

# Evaluar el modelo con el conjunto de entrenamiento
cat("Evaluacion del modelo a partir del conjunto de entrenamiento:\n")
probs_e <- predict(modelo, entrenamiento, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e, function(p) ifelse(p >= umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(datos[["am"]]))

ROC_e <- roc(entrenamiento[["am"]], probs_e)
plot(ROC_e)
# Con respecto al grafico entregado, curva se aleja bastante de la diagonal, por lo que
# al parecer se trata de un buen modelo.

matriz_e <- confusionMatrix(preds_e, entrenamiento[["am"]])
print(matriz_e)
# La exactitus del modelo es de 92.0%, la sensibilidad de 100% y la especificidad de 83.33%
# muestran que el modelo se desempeña un poco mejor identificando elementos de la clase postiva, 
# correspondiente en este caso a los vehiculos de transmision automatica.

# Evaluar el modelo con el conjunto de prueba
cat("Evaluación del modelo a partir del conjunto de prueba:\n")
probs_p <- predict(modelo, prueba, type = "response")

preds_p <- sapply(probs_p, function(p) ifelse(p >= umbral, "1", "0"))
preds_p <- factor(preds_p, levels = levels(datos[["am"]]))

ROC_p <- roc(prueba[["am"]], probs_p)
plot(ROC_p)

matriz_p <- confusionMatrix(preds_p, prueba[["am"]])
print(matriz_p)

# CONSLUSIONES

# donde podemos apreciar que el AIC es bastante bajo (AIC = 16, 23) y que la desviación 
# del modelo con una variable (23 grados de libertad) es de 12, 23.

# La curva ROC se aleja bastante de la diagonal, por lo que al parecer se trata de un buen modelo.

# Podemos ver que el modelo tiene una exactitud de 92,0 %. La sensibilidad de 100 % y la especificidad 
# de 83,33 % muestran que el modelo se desempeña un poco mejor identificando elementos de la clase 
# positiva, correspondiente en este caso a los vehículos de transmisión automática.

# Así, las líneas 36–46 obtienen la curva ROC (figura 15.6) y la matriz de confusión (figura 15.7) 
# para el conjunto de prueba, donde observamos un resultado con menor exactitud que con el conjunto 
# de entrenamiento. Esto es una indicación de que el modelo podría estar un poco sobreajustado para 
# el conjunto de entrenamiento, pero también de que el conjunto de prueba puede ser muy pequeño para 
# obtener una evaluación confiable.