# regresión lineal para la cantidad de requisitos funcionales de acuerdo a la cantidad de stakeholders

# el gerente de una empresa de desarrollo de software cree que, mientras más stakeholders 
# tiene un proyecto, menos requisitos funcionales tiene el software a desarrollar. Para 
# llevar a cabo el estudio pertinente, seleccionó aleatoriamente los datos de 15 proyectos 
# de entre los 200 que ha desarrollado la empresa hasta la fecha

# Puesto que la correlación entre ambas variables es relativamente fuerte (R = −0, 706), podemos 
# comprobar que los datos siguen una tendencia lineal. Al aplicar la prueba de normalidad de 
# Shapiro-Wilk a los residuos, concluimos que estos siguen una distribución cercana a la normal 
# (p = 0, 924). Podemos apreciar en la figura 13.16 que la variabilidad de los residuos es 
# relativamente constante. Por otra parte, las observaciones son independientes entre sí, pues 
# han sido seleccionadas de manera aleatoria y corresponden a menos del 10 % de la población. 
# En consecuencia, se verifica el cumplimiento de todas las condiciones necesarias para emplear 
# un modelo de RLS ajustado mediante mínimos cuadrados.


library(ggpubr) 
# Crear los datos originales.
requisitos <- c(11, 10, 12, 14, 8, 13, 18, 15, 20, 16, 21, 13, 10, 9, 21)
stakeholders<-c(8,8,6,6,8,7,3,1,3,4,5,4,4,9,2)
datos <- data.frame(requisitos , stakeholders)

# Ajustar modelo.
modelo <- lm(requisitos ~ stakeholders, data = datos)
print(summary(modelo))

# Graficar el modelo.
p <- ggscatter(datos, x = "stakeholders", y = "requisitos", color = "blue", fill = "blue",
  xlab = "Stakeholders", ylab = "Requisitos funcionales")
p <- p + geom_smooth(method = lm, se = FALSE, colour = "red")

# Graficar los residuos
b_1 <- modelo$coefficients[2]
b_0 <- modelo$coefficients[1]
residuos <- datos[["requisitos"]] - (b_1 * datos[["stakeholders"]] + b_0)
datos <- data.frame(datos , residuos)

r <- ggscatter(datos, x = "stakeholders", y = "residuos", color = "blue",
                  fill = "blue", xlab = "Stakeholders", ylab = "Residuo")

r <- r + geom_hline(yintercept = 0, colour = "red") 
g <- ggarrange(p, r, ncol = 2, nrow = 1)
print(g)

# Verificar normalidad de los residuos.
cat("Prueba de normalidad para los residuos\n")
print(shapiro.test(datos$residuos))

# CONCLUSIÓN
# podemos notar, bajo el encabezado Coefficients, una tabla con dos filas: una por cada 
# parámetro del modelo, donde la primera corresponde a la intercepción y la segunda, a la 
# pendiente. A su vez, la primera columna identifica los parámetros del modelo y la segunda 
# presenta sus valores estimados. Como toda estimación tiene asociado un margen de error, 
# la tercera columna muestra el error estándar para cada parámetro.

# Tras muchas horas buscando información, la estudiante ha formulado las siguientes hipótesis:
# H0: β1 = 0. La pendiente del modelo es igual a 0 o, lo que es lo mismo, la cantidad de 
#     stakeholders no explica en absoluto la cantidad de requisitos funcionales.
# HA: β1 < 0.
# Puesto que el valor p entregado por R corresponde a una prueba bilateral (fijarse en el 
# valor absoluto que incluye el título de la columna: Pr(>|t|)), en el caso unilateral se 
# debe considerar la mitad de este valor. En consecuencia, el gerente concluye, con 99 % 
# de confianza (p < 0.002), que en efecto la cantidad de requisitos funcionales disminuye 
# a medida que la cantidad de stakeholders aumenta.