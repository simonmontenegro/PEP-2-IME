library(ggpubr)
library(ggplot2)
library(dplyr)
library(leaps)
library(car)
library(lmtest)
library(caret)

#Se especifica y almacena la ruta del directorio de la base de datos.
dir <- "~/../Desktop/IME/Actividades/Actividad 12"
base <- "Body-EP12.csv"
arch <- file.path(dir, base)

# Se realiza la lectura de los datos, se escpecifica el formato de codificación UTF-8
datos <- read.csv(arch, fileEncoding = "UTF-8")

######################
##### PREGUNTA 1 #####
######################

# PRIMER PASO
# Se establece la semilla de acuerdo a integrante de menor edad del equipo
# (últimos 4 digítos de su rut)
semilla <- 2166
set.seed(semilla)

# SEGUNDO PASO
# Se procede a crear una muestra de 50 mujeres en relación al valor de la semilla
# establedico
datos_filtrados <- filter(datos, Gender == 0)
datos_filtrados <- datos_filtrados[sample(nrow(datos_filtrados), 50, replace = F), ]
datos_filtrados["Gender"] <- NULL
# TERCER PASO
# A continuación se seleciona de forma aleatoria 8 variables predictoras de acuerdo
# a la muestra seleccionada (nuestra_mujeres)
muestra_mujeres <- datos_filtrados

#Se guarda la columna peso para utilizarla mas adelante.
peso <- muestra_mujeres["Weight"]


#Se obtienen las 8 variables
muestra_mujeres["Weight"] <- NULL
columnas_muestra <- colnames(muestra_mujeres)
predictores_aleatorios <- sample(columnas_muestra,8)

# CUARTO PASO
# Se seleciona una variable predictora de las otras retantes
# que podría ser útil para predecir la variable peso ("Weight")

#Se almacenan los posibles predictores que pueden ser seleccionados
predictores_posibles <- setdiff(columnas_muestra, predictores_aleatorios)

#Se acota el data.frame solo con los predictores a seleccionar
muestra_mujeres <- muestra_mujeres[predictores_posibles]

#Se realiza el ajuste del modelo nulo y completo (será utilizado para el cuarto paso)
#Para ello, se agrega nuevamente la columna peso
muestra_mujeres <- cbind(muestra_mujeres, peso)
nulo <- lm(Weight ~ 1, data = datos_filtrados)
completo <- lm(Weight ~ ., data = muestra_mujeres)

#Para ello se realiza un ajuste con selección hacia delante
adelante <- step(nulo, scope = list(upper = completo), direction = "forward",
                 trace = 1, steps = 2)

#Con lo anterior, se tiene que una variable predictora adecuada es
# "Waist.Girth" (Grosor a la altura de la cintura). Esto dado que es la variable
# que presenta menor AIC, lo que significa que es la variable que penaliza en
# menor medida el modelo (genera un mejor modelo).
#Esta selección se realizó en el dominio de predictores que no están 
#en los 8 seleccionados aleatoriamente ni tampoco los predictores Gender y Weight.

#Adicionalmente, con la función cor se obtiene el nivel de correlación que existe
# entre las variables del data.frame, así, se ve que Waist.Girth presenta la
# correlación mas fuerte con Weight, esto motiva aún mas a seleccionar dicha variable
# como predictor del modelo.
correlaciones <- round(cor(x = muestra_mujeres, method = "pearson"), 3)

# QUINTO PASO
# Posteriormente, se construye un modelo de regresión simple con el
# predictor seleccionado en el paso anterior:

#Se construye el modelo
modelo <- lm(Weight ~ Waist.Girth, data = muestra_mujeres)
print(summary(modelo))

#Se grafica el modelo.
g <- ggscatter(muestra_mujeres, x = "Waist.Girth", y = "Weight", color = "blue",
               xlab = "Grosor a la altura de la cintura", ylab = "Peso")
print(g)


# SEXTO PASO
#Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco
# predictores de entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de
# regresión líneal simple obtenido en el paso 5

#Se crea un nuevo modelo con las 8 variables aleatorias seleccionadas
muestra_mujeres_2 <- datos_filtrados
waist <- muestra_mujeres_2["Waist.Girth"]
muestra_mujeres_2["Weight"] <- NULL

#Se acota el data.frame solo con los predictores a seleccionar
muestra_mujeres_2 <- muestra_mujeres_2[predictores_aleatorios]

#Se realiza el ajuste del modelo nulo y completo (será utilizado para el cuarto paso)
#Para ello, se agrega nuevamente la columna peso
muestra_mujeres_2 <- cbind(muestra_mujeres_2, peso)
muestra_mujeres_2 <- cbind(muestra_mujeres_2, waist)

modelo_2 <- lm(Weight ~ Waist.Girth, data = muestra_mujeres_2)
print(summary(modelo_2))

#Se grafica el modelo.
g_2 <- ggscatter(muestra_mujeres_2, x = "Waist.Girth", y = "Weight", color = "blue",
               xlab = "Grosor a la altura de la cintura", ylab = "Peso")
print(g_2)

completo_2 <- lm(Weight ~ ., data = muestra_mujeres_2)

#Para ello se realiza un ajuste con selección hacia delante
adelante_2 <- step(modelo_2, scope = list(upper = completo_2), direction = "forward",
                 trace = 1)

#Con esto, se agregan cuatro predictores al modelo: Thigh.Girth (Grosor promedio de ambos 
# muslos bajo el pliegue del gluteo), Wrists.diameter (Suma de los diámetros de las muñecas),
# Calf.Maximum.Girth (Grosor promedio de la parte más ancha de ambas pantorrillas) y 
# Shoulder.Girth (Grosor de los hombros sobre los músculos deltoides), esta cantidad
# queda dentro del rango solicitado de agregar 2-5 predictores. Estos predictores se
# agregan dado que minimizan el valor de AIC (es decir, mejoran el modelo).

modelo_final <- update(modelo_2, . ~ . + Thigh.Girth + Wrists.diameter + 
                         Calf.Maximum.Girth + Shoulder.Girth)

# SEPTIMO PASO
#Evaluar los modelos y "arreglarlos" en caso de que tengan algún problema con las condiciones que
# deben cumplir.
#Esto en torno a un nivel de significancia de
alfa <- 0.05

#Se realiza la comprobación de condiciones para el RLS

#Comprobación de que los datos presentan una relación lineal.
#Este se puede verificar con:
correlaciones <- round(cor(x = muestra_mujeres, method = "pearson"), 3)

#Donde el R obtenido para el par Weight ~ Waist.Girth corresponde a 0.890, lo que indica
# que existe una relación relativamente fuerte, así se puede comprobar que los datos siguen
# una tendencia lineal.

#Comprobación de la distribución de los residuos (aproxima a la normal)
cat("Prueba de normalidad para los residuos\n")
print(shapiro.test(modelo$residuals))

#Se cumple con la normalidad de residuos, puesto que el p-value obtenido está
# por sobre el nivel de significancia, por lo que se puede concluir que los resi
# -duos tienen un comportamiento aproximado a normal.

#Comprobación de la variabilidad de los puntos entorno a la línea de mínimos cuadrados
# debe ser aproximadamente constante.
b_1 <- modelo$coefficients[2]
b_0 <- modelo$coefficients[1]
residuos <- muestra_mujeres[["Weight"]] - (b_1 * muestra_mujeres[["Waist.Girth"]] + b_0)
muestra_mujeres <- data.frame(muestra_mujeres, residuos)
g_var <- ggscatter(muestra_mujeres, x = "Waist.Girth", y = "residuos", color = "blue", fill = "blue",
               xlab = "Grosor a la altura de la cintura", ylab = "Residuos")
g_var <- g_var + geom_hline(yintercept = 0, colour = "red")
print(g_var)

#Ante esto, al observar la gráfica se puede apreciar que la variabilidad de de los
# residuos es relativamente constante.

#Comprobación de que las observaciones deben ser independientes entre si.
#En este caso, las observaciones son independientes entre sí, pues han sido
# seleccionadas de manera aleatoria y corresponden a menos del 10% de la población.


#Se realiza la comprobación de condiciones para el RLM.
#Las variables predictoras deben ser cuantitativas o dicotómicas.
#   Si, son cuantitativas, ya que cada uno de los predictores son 'medidas' numéricas.
#
#La variable de respuesta debe ser cuantitativa y continua, sin restricciones para su variabilidad.
#   Si, la variable respuesta(peso) es cuantitativa y además continua.
#
#Los predictores deben tener algún grado de variabilidad (no pueden ser constantes).
#   Si, los predictores poseen variabilidad; no son constantes (son medidas que varían).
#
#Cada predictor se relaciona linealmente con la variable de respuesta.
#   Si, puesto que al analizar las correlaciones con la variable de respuesta (peso)
#   se puede ver que presentan valores muy cercanos a 1 (fuertemente relacionados).
correlaciones_2 <- round(cor(x = muestra_mujeres_2, method = "pearson"), 3)

#Comprobación de independencia de los residuos.
cat("\nPrueba de Durbin-Watson para autocorrelaciones.")
cat("entre errores:\n")
print(durbinWatsonTest(modelo_final))

#Comprobación de normalidad de los residuos.
cat("\nPrueba de normalidad para los residuos:\n")
print(shapiro.test(modelo_final$residuals))

#Comprobación de homocedasticidad de los residuos.
cat("\nPrueba de homocedasticidad para los residuos:\n")
print(ncvTest(modelo_final))

#Comprobación de multicolinealidad
vifs <- vif(modelo_final)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

#Ante esto se puede ver que:
# i)Se cumple con la independencia de los residuos, puesto que el p-value obtenido
#   está muy por sobre el nivel de significancia, por lo que se puede concluir que 
#   en efecto los residuos son independientes.
#
# ii)Se cumple con la normalidad de residuos, puesto que el p-value obtenido está
#   por sobre el nivel de significancia, por lo que se puede concluir que los resi
#   -duos tienen un comportamiento aproximado a normal.
#
# iii)Se cumple con la homocedasticidad de los residuos, puesto que el p-value obtenido
#   está muy por sobre el nivel de significancia, por lo que se puede concluir que
#   los residuos tienen varianzas similares para cada nivel de los predictores.
#
# iv)En el caso de la multicolinealidad, los datos recabados sugieren que el modelo
#   podría estar sesgado, puesto que las tolerancias no superan (en la mayoría de 
#   los casos) el valor 0.4, donde además el VIF promedio supera el valor 2.5, lo 
#   que aumenta la preocupación en este aspecto.
modelo_final_corregido <- update(modelo_final, . ~ .  -Shoulder.Girth - Calf.Maximum.Girth)

#Comprobación de multicolinealidad
vifs_corregido <- vif(modelo_final_corregido)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs_corregido)
cat("- Tolerancias:\n")
print(1 / vifs_corregido)
cat("- VIF medio:", mean(vifs_corregido), "\n")

#Con lo anterior, eliminando dos predictores se pudo corregir el modelo, obteniendo
# valores que permiten verificar la multicolinealidad.


# OCTAVO PASO
#Evaluar el poder predictivo de los modelos en datos no utilizados para construirlo 
# (o utilizando validación cruzada)

#PARA RLS
#Se crea el conjunto de entrenamiento para la validación.
nRLS <- nrow(muestra_mujeres)
n_entrenamientoRLS <- floor(0.8 * nRLS)
muestraRLS <- sample.int(n=nRLS, size=n_entrenamientoRLS, replace = F)
entrenamientoRLS <- muestra_mujeres[muestraRLS, ]
pruebaRLS <- muestra_mujeres[-muestraRLS, ]

modelo_cruzadaRLS <- train(Weight ~ Waist.Girth, data = entrenamientoRLS, method = "lm",
                        trControl = trainControl(method = "cv", number = 5))
print(summary(modelo_cruzadaRLS))

#Se obtiene el MSE (error cuadrático medio) para el conjunto entrenamiento.
mse_entrenamientoRLS <- modelo_cruzadaRLS$results$RMSE

#Se realizan las predicciones
prediccionesRLS <- predict(modelo_cruzadaRLS, pruebaRLS)

#Se calcula el error cuadrático medio para el conjunto de prueba.
errorRLS <- pruebaRLS[["Weight"]] - prediccionesRLS
mse_pruebaRLS <- sqrt(mean(errorRLS ** 2))

#PARA RLM
#Se crea el conjunto de entrenamiento para la validación.
nRLM <- nrow(muestra_mujeres_2)
n_entrenamientoRLM <- floor(0.8 * nRLM)
muestraRLM <- sample.int(n=nRLM, size=n_entrenamientoRLM, replace = F)
entrenamientoRLM <- muestra_mujeres_2[muestraRLM, ]
pruebaRLM <- muestra_mujeres_2[-muestraRLM, ]

modelo_cruzadaRLM <- train(Weight ~ Waist.Girth + Thigh.Girth + Wrists.diameter, data = entrenamientoRLM, method = "lm",
                           trControl = trainControl(method = "cv", number = 5))
print(summary(modelo_cruzadaRLM))

#Se obtiene el MSE (error cuadrático medio) para el conjunto entrenamiento.
mse_entrenamientoRLM <- modelo_cruzadaRLM$results$RMSE

#Se realizan las predicciones
prediccionesRLM <- predict(modelo_cruzadaRLM, pruebaRLM)

#Se calcula el error cuadrático medio para el conjunto de prueba.
errorRLM <- pruebaRLM[["Weight"]] - prediccionesRLM
mse_pruebaRLM <- sqrt(mean(errorRLM ** 2))  

#Errores
cat("\nError cuadrático medio RLS para entrenamiento: ", mse_entrenamientoRLS, "\n")
cat("\nError cuadrático medio RLS para prueba: ", mse_pruebaRLS, "\n")
cat("\nError cuadrático medio RLM para entrenamiento: ", mse_entrenamientoRLM, "\n")
cat("\nError cuadrático medio RLM para prueba: ", mse_pruebaRLM, "\n")


#En base a los resultados obtenidos para el modelo de RLS se tiene una diferencia
# bastante aceptable, pues son valores bastante cercanos, por lo que se puede decir
# que el modelo es generalizable, es decir, el modelo genera buenos resultados
# predictivos para el predictor seleccionado, lo cual tiene sentido pues se consideró
# un predictor que 'mejora' el modelo.

#Así mismo, los resultados para el modelo de RLM también denotan una diferencia
# pequeña (menor que la de RLS incluso), por lo que se puede decir que el modelo
# también es generalizable, esto pues genera buenos resultados predictivos para 
# los predictores seleccionados, lo que cobra sentido, puesto que se han seleccionado
# aquellos predictores que fueron agregados al modelo RLS, los que disminuyeron 
# su valor AIC, con lo que el modelo 'mejora'.









