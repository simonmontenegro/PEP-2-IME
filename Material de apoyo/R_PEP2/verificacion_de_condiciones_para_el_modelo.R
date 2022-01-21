# verificación de condiciones para el modelo.

library(carData)
library(car)

# Cargar datos.
datos <- mtcars

# Ajustar modelo.
modelo <- lm(mpg ~ wt + qsec + am, data = datos)

# Comprobar independencia de los residuos.
cat("Prueba de Durbin-Watson para autocorrelaciones ") 
cat("entre errores:\n") 
print(durbinWatsonTest(modelo))
# debemos tener en cuenta que los resultados de esta prueba dependen del orden de los datos, 
# por lo que al reordenar los datos se podrían obtener resultados diferentes. Al aplicar 
# esta prueba para el ejemplo obtenemos un valor p = 0, 236, por lo que podemos concluir 
# que los residuos son, en efecto, independientes.

# Comprobar normalidad de los residuos.
cat("\nPrueba de normalidad para los residuos:\n") 
print(shapiro.test(modelo$residuals))
# obtenemos como resultado p = 0, 080, por lo que podemos asumir que el supuesto se cumple, 
# aunque manteniendo cautela por la cercanía con el nivel de significación.

# Comprobar homocedasticidad de los residuos.
cat("Prueba de homocedasticidad para los residuos:\n") 
print(ncvTest(modelo))
# Una prueba adecuada para verificar esta condición es la de Breusch-Pagan-Godfrey (Glen, 2016), 
# cuya hipótesis nula es que las varianzas de los residuos son iguales. 
# Al usarla, obtenemos como resultado p = 0, 212, por lo que podemos concluir que el supuesto de 
# homocedasticidad se cumple.

# Comprobar la multicolinealidad.
vifs <- vif(modelo)
cat("\nVerificar la multicolinealidad:\n") 
cat("- VIFs:\n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")
# Aunque no hay un acuerdo general, muchos autores usan el valor VIF ≥ 10 como umbral para preocuparse, 
# aunque hay autores que consideran críticos valores más conservadores, de 5 o incluso de 2,5
# En el caso de la tolerancia, la literatura sugiere que valores bajo 0,2 podrían ser problemáticos, 
# aunque algunos académicos creen que valores cercanos a 0,4 deberían ser revisados.

# Si miramos los factores de inflación de la varianza, en general no parecen ser preocupantes. Sin embargo, 
# los estadísticos de tolerancia son preocupantes. Adicionalmente, el VIF promedio también indica que el 
# modelo podría estar sesgado. Es necesario, entonces, buscar un modelo más confiable. Podríamos, por ejemplo, 
# eliminar la variable menos significativa (am, que tiene el menor valor p), obteniendo el modelo con dos 
# predictores de la figura 14.1, el que tendríamos que evaluar y comparar con este modelo de tres predictores.


