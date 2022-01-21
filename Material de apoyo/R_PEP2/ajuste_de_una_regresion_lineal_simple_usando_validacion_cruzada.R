# AJUSTE DE UNA REGRESIÓN LINEAL SIMPLE USANDO VALIDACIÓN CRUZADA

# Cargar los datos.
datos <- mtcars 
# Crear conjuntos de entrenamiento y prueba.
set.seed(101)
n <- nrow(datos)
n_entrenamiento <- floor(0.7 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- datos[muestra , ]
prueba <- datos[-muestra , ]

# Ajustar modelo con el conjunto de entrenamiento.
modelo <- lm(mpg ~ wt, data = entrenamiento)
print(summary(modelo))

# Calcular error cuadrado promedio para el conjunto de entrenamiento.
mse_entrenamiento <- mean(modelo$residuals ** 2)
cat("MSE para el conjunto de entrenamiento:", mse_entrenamiento, "\n")

# Hacer predicciones para el conjunto de prueba.
predicciones <- predict(modelo , prueba)

# Calcular error cuadrado promedio para el conjunto de prueba.
error <- prueba[["mpg"]] - predicciones
mse_prueba <- mean(error ** 2)
cat("MSE para el conjunto de prueba:", mse_prueba)

# CONCLUSIÓN
# Fijémonos en que, para el conjunto de entrenamiento, el error cuadrático medio es 
# MSEe = 5,652, mientras que para el conjunto de prueba obtenemos MSE p = 17, 516, 
# bastante más elevado (¡más del triple!). Esto sugiere que el modelo puede estar 
# sobreajustado, es decir, que se adapta bien a los datos del conjunto de entrenamiento 
# pero no tanto al conjunto de prueba, por lo que podría ser imprudente suponer que puede 
# ser generalizado. Sin embargo, esto puede deberse a la separación aleatoria de los datos. 
# Al ejecutar el script 13.4 reemplazando la semilla aleatoria por 125, obtenemos el resultado 
# de la figura 13.15. Podemos notar que los parámetros del modelo son algo diferentes a los 
# obtenidos con la semilla 101. Además, ahora el error cuadrático medio para el conjunto de 
# entrenamiento es MSEe = 8,596 y para el conjunto de prueba, MSEp = 9,122. Estos últimos 
# valores son muy parecidos, por lo que este segundo modelo sí podría ser generalizable.

