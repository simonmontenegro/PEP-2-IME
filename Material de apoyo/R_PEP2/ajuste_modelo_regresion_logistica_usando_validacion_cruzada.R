# la validación cruzada como herramienta para mejorar la estimación del error

library(caret)

set.seed(1313)

# Cargar los datos
datos <- mtcars
datos$am <- factor(datos$am)

# Ajustar modelo usando validacion cruzada de 5 pliegues
modelo <- train(am ~wt, data = entrenamiento, method = "glm", 
                family = binomial(link = "logit"), trControl = trainControl(method = "cv", number = 5, 
                                                                            SavePredictions = TRUE))
print(summary(modelo))

# Evaluar el modelo
cat("Evaluacion del modelo basada en validacion cruzada:\n")
matriz <- confusionmatrix(modelo$pred$pred, modelo$pred$obs)
print(matriz)