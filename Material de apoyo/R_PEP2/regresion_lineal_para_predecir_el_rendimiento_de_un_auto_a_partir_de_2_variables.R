# regresión lineal para predecir el rendimiento de un automóvil a partir de dos variables.

# Ahora consideremos una RLM con dos predictores para el rendimiento: el peso (columna wt) 
# y el tiempo mínimo requerido para recorrer un cuarto de milla (columna qsec)

library(scatterplot3d) 
# Cargar los datos.
datos <- mtcars

# Ajustar modelo usando validación cruzada de 5 pliegues.
modelo <- lm(mpg ~ wt + qsec, data = datos) 
print(summary(modelo))

# Graficar modelo ajustado.
g <- scatterplot3d(datos$wt, datos$qsec, datos$mpg, type = "p",
                   highlight.3d = TRUE, pch = 20, xlab = "Peso [lb x 1000]",
                   ylab = "Rendimiento [millas/galón]", zlab = "1/4 de milla [s]")
g$plane3d(modelo ,draw_polygon = TRUE, draw_lines = TRUE) 
print(g)


# Para usar este modelo a fin de predecir valores para la respuesta a partir de un nuevo conjunto de datos, 
# usamos una vez más la función predict(), del mismo modo que vimos en el capítulo 13 para la RLS.
# Como en este caso tenemos dos predictores, lo que se ajusta ya no es una recta, sino un plano, como muestra 
# la figura 14.2. Así, ya no tiene sentido hablar de la pendiente de la recta al momento de interpretar los 
# parámetros del modelo. Un análisis de regresión lineal con múltiples variables busca aislar la relación 
# entre cada predictor y la respuesta, por lo que el coeficiente βi, asociado al i-ésimo predictor, representa 
# el cambio esperado que se produce en la respuesta al incrementar dicho predictor en una unidad, manteniendo 
# constantes todos los demás predictores. Si b1 es el parámetro ajustado para el peso y b2 es el parámetro 
# ajustado para el cuarto de milla, b1 puede entenderse como la pendiente del plano de la figura 14.1 con 
# respecto al eje y, mientras que b2 puede entenderse como la pendiente del mismo plano con respecto al eje z. 
# A su vez, la intercepción fija la posición del plano con respecto al origen.