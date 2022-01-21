
# Condiciones a verificar:

# 1. El gráfico 1muestra la recta ajustada para el rendimiento de un automóvil de acuerdo a su peso, 
#    donde podemos observar que los datos presentan una relación lineal, aunque algunos 
#    puntos parecen estar algo alejados de la recta ajustada
# 2. en general, sigue una distribución razonablemente cercana a la normal (figura 13.8b), aunque en 
#    ambas figuras se aprecian unos pocos modelos que se comportan como valores atípicos.
# 3. variabilidad de los residuos no es muy grande (figura 13.8a) 
# 4. Además, podemos suponer que las observaciones son independientes entre sí y, evidentemente, 
#    no corresponden a una serie de tiempo.

library(ggpubr)

# Cargar los datos.
datos <- mtcars

# Ajustar modelo con R.
modelo <- lm(mpg ~ wt, data = datos)
print(summary(modelo))

# Graficar el modelo.
# wt: Peso total, en miles de libras. (predictor)
# mpg: Rendimiento, en millas (EEUU) por galón [millas/galón]
p <- ggscatter(datos, x = "wt", y = "mpg", color = "blue", fill = "blue",
               xlab = "Peso [lb x 1000]", ylab = "Rendimiento [millas/galón]", title = "Gráfico 1")
p <- p + geom_smooth(method = lm, se = FALSE, colour = "red") 
print(p)

# Crear gráficos para evaluar el modelo.
plot(modelo)

# Ingresar algunas instancias artificiales.
mpg <- c(23.714, 19.691, 19.242, 12.430, 10.090, 9.565, 18.171, 26.492, 7.054,
            24.447 , 15.683 , 17.403 , 13.465 , 18.850 , 29.493)

wt <- c(2.973, 4.532, 2.332, 3.016, 4.220, 4.286, 2.580, 3.084, 3.816, 2.775,
           3.251 , 3.013 , 4.951 , 2.644 , 2.218)

nuevos <- data.frame(mpg, wt) 

# Usar el modelo para predecir el rendimiento de los nuevos y ver los
# residuos resultantes.
predicciones <- predict(modelo , nuevos)
residuos <- nuevos$mpg - predicciones
nuevos <- data.frame(nuevos , residuos)

r <- ggscatter( nuevos, x = "wt", y = "residuos", color = "blue",
                   fill = "blue", xlab = "Peso [lb * 1000]", ylab = "Residuo")

r <- r + geom_hline(yintercept = 0, colour = "red")
print(r)
             
             