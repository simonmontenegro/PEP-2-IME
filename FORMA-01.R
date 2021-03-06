library(ggpubr)
library(pwr)
library(ggplot2)
library(tidyverse)
library(ez)
library(leaps)
library(car)
library(lmtest)
library(caret)
library(pROC)
library(caret)

# Se especifica y almacena la ruta del directorio de la base de datos.
dir <- "~/../Desktop/IME/PEP_2/PEP-2-IME"
base <- "Datos PEP 2.csv"
arch <- file.path(dir, base)

# Se realiza la lectura de los datos, se escpecifica el formato de codificaci???n UTF-8
datos <- read.csv2(arch, fileEncoding = "UTF-8")

# --------------- PREGUNTA 1 ---------------
# Lord Vader desea saber si los niveles de exigencia con que los instructores de las 
# diferentes divisiones evalúan a los nuevos soldados son similares, por lo que le ha 
# solicitado estudiar si existen diferencias significativas en el promedio de la 
# evaluación realizada por el instructor entre las distintas divisiones. El Lord Sith 
# ha sido muy claro al solicitar un reporte de aquellas divisiones en las que se observen 
# diferencias.

# Dado que se solicita evaluar diversas divisiones bajo la evaluacion del instructor, es que
# se debe planear un abordaje mediante ANOVA de una ví­a para muestras independientes, esto pues 
# se considera un estudio para mas de dos muestras (divisiones en este caso), considerando una 
# variable independiente cuyos niveles definen los grupos que se están comparando.

#En torno al enunciado y las solicitudes de Lord Vader, se plantea la prueba de hipótesis que
# representa el problema.

# Las hipótesis a formular son:
# H0: No existen diferencias significativas en el promedio de la evaluación realizada por
#     el instructor entre las distintas divisiones.
# HA: Sí existe diferencias significativas en el promedio de la evaluación realizada por
#     el instructor entre las distintas divisiones.

#Se filtran los datos de interés del problema, es decir, division y evaluacion del instructor,
# en este caso también se incluye 'id' para facilitar los procedimientos de más adelante.
datos_filtrados <- datos %>% select(id, division, eval_instructor)

#Para el procedimiento de ANOVA de una vía para muestras independientes se deben verificar algunas
# condiciones previas para trabajar.
# Las condiciones a verificar:
# 1.La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos 
# iguales. 
#    La escala con que se mide la variable dependiente tiene las propiedades de una escala de
#    de intervalos iguales, dado que la variable a medir se encuentra en forma numérica para todas 
#    las observaciones. (Se asume como premisa que el instructor realiza la asignación de puntaje
#    dentro de una escala de intervalos iguales, es decir, utiliza la misma escala independiente de
#    la división del recluta o del recluta en sí)
# 
# 2.Las k muestras son obtenidas de manera aleatoria e independiente desde la(s) población(es) de origen.
#    Se asume que las observaciones para las k muestras obtenidas se recopilan y seleccionan de manera
#    totalmente aleatoria y de manera independiente desde la población para cada división.
#   
# 3.Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.
#   Para verificar esta condicion, se realiza el gráfico Q-Q

#   Comprobación de normalidad .
g <-   ggqqplot(datos_filtrados, x = "eval_instructor", y = "division", color = "division") 
g <-   g + facet_wrap (~ division )
g <-   g + rremove("x.ticks") + rremove("x.text") 
g <-   g + rremove("y.ticks") + rremove("y.text") 
g <-   g + rremove("axis.title")
print(g)

#   Es correcto afirma que en base al gráfico, existen algunos valores atípicos, pero son muy leves.
#   Sin embargo, debido a estos valores, es necesario ser cautelosos al realizar la prueba, por tanto,
#   se utilizará un nivel de significación igual a 0.01.

alfa <- 0.01

# 4.Las k muestras tienen varianzas aproximadamente iguales.
#   Para esto, se hace uso de ezAnova(), la cual permite realizar también la prueba de homocedasticidad
#   de Levene, la cual permite dar respuesta a la condición.

# Se establecen las hipótesis para el test de Levine.
# H0: Las varianzas de las k muestras son iguales.
# HA: Al menos una de las muestras tiene varianza diferente a alguna de las demás. 

cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
pruebaEzAnova <- ezANOVA(data = datos_filtrados,
                        dv = eval_instructor,
                        between = division,
                        wid = id,
                        type = 2,
                        return_aov = TRUE)
print(pruebaEzAnova)

#Gráfico del tama?o del efecto.
g2 <- ezPlot(data = datos_filtrados , 
              dv = eval_instructor,
              wid = id, 
              between = division,
              y_lab = "Promedio de la evaluación realizada por el instructor entre las distintas divisiones", 
              x = division)
print(g2)

#En torno a estos resultados del test de homogeneidad de las varianzas, se tiene 
# un p-value igual a 0.5035376, que a su vez supera al nivel de significación estipulado,
# con esto, se falla al rechazar la hipótesis nula, por lo que se verifica que las varianzas de las
# k muestras son iguales, es decir, se cumple la cuarta condición.

# Conclusiones
# Como se vió anteriormente al aplicar ezAnova(), el resultado del p-value en torno a la situación corresponde
# a p ~ 2.575512e-107, es decir, un valor muy (mucho) por debajo del nivel de significancia establecido, por lo que
# se rechaza la hipótesis nula en favor de la hipótesis alternativa, se puede asegurar con un 99% de confianza
# que sí existen diferencias significativas en el promedio de la evaluación realizada por el instructor entre las 
# distintas divisiones. Por tanto, es necesario realizar un anÃ¡lisis Post-Hoc para saber en qué divisiones se 
# encuentran esas diferencias.
# Esto es muy evidente si analiza el gráfico del tamañoo del poder, donde "Cavetrooper" y "Spacetrooper" se desmarcan
# totalmente en el gráfico.

# Dado que el enunciado menciona que "El Lord Sith ha sido muy claro al solicitar un reporte de aquellas divisiones 
# en las que se observen diferencias" se hace totalmente necesario obtener aquellas divisiones donde existen dichas
# diferencias (debemos recordar que ANOVA corresponde a una prueba de tipo OMNIBUS, esto significa que no menciona/detalla
# donde se encuentran las diferencias, solo dice si existen o no).

# Con lo analizado, se procede a realizar un procedimiento Post-Hoc HSD Tukey, por el nivel de precisión en la
# seleccion de diferencias significativas.

#Procedimiento ANOVA con aov ().
cat(" Procedimiento    ANOVA    usando    aov \ n\ n") 
pruebaAnova <- aov(eval_instructor ~ division, data = datos_filtrados) 
print(summary(pruebaAnova))

#Prueba HSD de Tukey .
post_hoc <- TukeyHSD(pruebaAnova,
                    "division",
                     ordered = TRUE,
                     conf.level = 1 - alfa)
print(post_hoc)

# Conclusiones
# En base a los resultados obtenidos, es posible notar que existen diferencias muy significativas entre la división 
# Spacetrooper y las demÃ¡s, como tambiÃ©n entre la división Cavetrooper y las demás (esto se infiere a partir del análisis
# del p-value conseguido para cada una de las diferencias, donde las divisiones mencionadas presentan un p-value í­nfimo).
# Por otro lado, el resto de divisiones presentan un p-value mucho mayor que el nivel de signficancia 0.01, esto se
# verifica a partir del grÃ¡fico del tamaño del poder, donde las seis divisiones están centradas casi en 650 y presentan
# leves diferencias entre sÃ­. PodrÃ­a destacarse que la división "Shoretrooper" presenta una diferencia que se escapa, sin
# embargo, esta diferencia no llega a ser significativa.

# A partir de lo concluido, se debe notificar al Lord Sith de que aquellas divisiones que presentan diferencias 
# significativas son las divisiones: "Spacetrooper" y "Cavetrooper".


# --------------- PREGUNTA 2 ---------------
# A fin de determinar si es necesario establecer programas de entrenamiento diferenciados para clones y
# reclutas, Lord Vader quiere saber si es posible distinguir entre ambas clases de soldados con los datos actuales. Para ello,
# ha solicitado evaluar un modelo clasificador que contemple entre 2 y 5 variables predictoras. Considere que, para ser
# aceptable, el modelo:
# â¢ Debe lograr una exactitud (accuracy) de al menos 0,8 en datos de prueba
# â¢ No puede considerar casos con demasiada influencia (considerando la distancia de Cook)
# â¢ No debe presentar autocorrelaciÃ³n (usando la prueba de Durbin-Watson para un retardo y un nivel de significaciÃ³n Î± = .01)
# â¢ No debe presentar multicolinealidad severa (considerando el factor de inflaciÃ³n de la varianza, con un VIF promedio
#   inferior a 1,03).
# Considere la semilla 21 para obtener una muestra de 400 datos, 80% de los cuales serÃ¡n empleados para ajustar el
# modelo y el 20% restante, para evaluarlo.

# Dado que se requiere evaluar un modelo clasificador con 2-5 variables predictoras, estamos ante un modelo de tipo regresiÃ³n mÃºltiple,
# especÃ­ficamente, como la variable a "predecir" corresponde a una variable dicotÃ³mica (es clon / es recluta) se trata de una
# regresiÃ³n logÃ­stica. Ante esto, se procede a realizar el paso a paso para evaluar el modelo.

# Se establece la semilla segÃºn enunciado (21)
semilla <- 21
set.seed(semilla)

# Se define la siguiente funciÃ³n para expresar la variable dicotÃ³mica de manera numÃ©rica (1 o 0); 0 para reclutas, 1 para clones.
es_clon <- function(x){
  if(x == "N"){
    return(0)
  }else if (x == "S"){
    return(1)
  }
}

# Se transforma el string de "es_clon" y se expresa numÃ©ricamente (0 o 1)
datos2 <- datos
datos2[["es_clon"]] <- sapply(datos[["es_clon"]], es_clon)


# Ahora se filtra de la muestra separando clones de reclutas
datos_filtrados0 <- filter(datos2, es_clon == 0)
datos_filtrados1 <- filter(datos2, es_clon == 1)

# Se obtiene una muestra aleatoria de 200 reclutas y 200 clones; asÃ­ evitar posibles problemas de poca representatividad en la ROC.
# Nota: esto lo hago porque en la actividad 13 nos pasÃ³ que al seleccionar los datos tuvimos problemas con la curva ROC, por lo que
# el profesor nos indicÃ³ seleccionar 50% de la muestra total para cada caso de la variable dicotomica.
datos_filtrados0 <- datos_filtrados0[sample(nrow(datos_filtrados0), 200, replace = F), ]
datos_filtrados1 <- datos_filtrados1[sample(nrow(datos_filtrados1), 200, replace = F), ]

# Se juntan ambas muestras para formar una Ãºnica muestra aleatoria de tamaÃ±o 400 (200 reclutas y 200 clones)
datos_filtrados_final <- rbind(datos_filtrados0, datos_filtrados1)

#Se guarda la columna es_clon por si se utilizan más adelante.
is_clon <- datos_filtrados_final["es_clon"]
  
#Se obtienen las 8 variables, primero eliminando la variable es_clon, id y division.
datos_filtrados_final["es_clon"] <- NULL
datos_filtrados_final["id"] <- NULL
datos_filtrados_final["division"] <- NULL
columnas_muestra <- colnames(datos_filtrados_final)

#Ahora, se seleccionan aleatoria mente las 8 variables
predictores_aleatorios <- sample(columnas_muestra, 8)
nuevos_datos <- datos_filtrados_final[predictores_aleatorios]
nuevos_datos <- cbind(nuevos_datos, is_clon)

# Se realiza el ajuste del modelo nulo y completo
nulo <- glm(es_clon ~ 1, family = binomial(link = "logit"), data = nuevos_datos)
completo <- glm(es_clon ~ ., family = binomial(link = "logit"), data = nuevos_datos)

# Se realiza un ajuste con selecciÃ³n hacia delante.
modelo <- step(nulo, scope = list(upper = completo), direction = "forward", trace = 1, steps = 4)
print(summary(modelo))

# Con lo anterior, se puede ver que el procedimiento de "step" muestra aquellas variables que pueden ser agregadas al modelo y que 
# lo "mejoran", esto dado a que reducen el valor de AIC cada vez que se agregan un nuevo predictor.
# Los predictores seleccionados son: velocidad, agilidad, fuerza y precision (en ese orden; de izquierda a derecha), con esto
# se realiza una actualización del modelo añadiendo estas variables (predictores); esta acción es propia de la función step(),
# asi que "modelo" ya contiene los predictores mencionados.

# Así, se procede a comprobar las condiciones para la regresión logística.
# Debe existir una relación lineal entre los predictores y la respuesta transformada.
correlaciones <- round(cor(x = nuevos_datos, method = "pearson"), 3)
#Al analizar las correlaciones con la variable de respuesta (es_clon)
# se puede ver que presentan valores que se relacionan de buena manera con la variable a predecir.

#Comprobación de multicolinealidad
vifs <- vif(modelo)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

#En el caso de la multicolinealidad, los datos recabados sugieren que el modelo
# podría estar sesgado, puesto que mas de la mitad de los predictores resultaron en una
# tolerancia menor a 0.4, donde además el VIF promedio supera por mucho el valor 2.5,  
# lo que aumenta la preocupación en este aspecto.
modelo_corregido <- update(modelo, . ~ .  -fuerza, -precision)

#ComprobaciÃ³n de multicolinealidad
vifs <- vif(modelo_corregido)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

#En así como se corrige el modelo, se quitan los ultimos dos predictores añadidos,
# obteniendose así un resultado favorable que comprueba la condición de multicoli-
# nealidad.

#Los residuos deben ser independientes entre sí.
# Comprobación de independencia de los residuos.
cat("\nPrueba de Durbin-Watson para autocorrelaciones.")
cat("entre errores:\n")
print(durbinWatsonTest(modelo_corregido))
#Se cumple con la independencia de los residuos, puesto que el p-value obtenido
# está muy por sobre el nivel de significancia, por lo que se puede concluir que 
# en efecto los residuos son independientes.

# Revisar casos con distancia de Cook mayor a uno.
resultado <- data.frame(predicted.probabilities = fitted(modelo))
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat ("\n\n")
cat ("Residuales con una distancia de Cook alta \n")
cat ("- - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos2, ]))


#Se crea el conjunto de entrenamiento para la validación.
nuevos_datos$es:clon <- factor(nuevos_datos$es_clon)
nRL <- nrow(nuevos_datos)
n_entrenamientoRL <- floor(0.8 * nRL)
muestraRL <- sample.int(n=nRL, size=n_entrenamientoRL, replace = F)
entrenamientoRL <- nuevos_datos[muestraRL, ]
pruebaRL <- nuevos_datos[-muestraRL, ]

modelo_RL <- glm(es_clon ~ velocidad + agilidad, family = binomial(link = "logit"),
                 data = entrenamientoRL)
print(summary(modelo_RL))

#Se revisan las curvas Roc
comparar_umbral <- function(x){
  umbral <- 0.5
  if(x >= umbral){
    return(1)
  }else{
    return(0)
  }
} 

#Se evalua el modelo RL con el conjunto de entrenamiento.
probs_RL_entrenamiento <- predict(modelo_RL, entrenamientoRL, type = "response")
preds_RL_entrenamiento <- sapply(probs_RL_entrenamiento, comparar_umbral)
preds_RL_entrenamiento <- factor(preds_RL_entrenamiento, levels = levels(nuevos_datos[["es_clon"]]))

#Se evalua el modelo RL con el conjunto de prueba
probs_RL_prueba <- predict(modelo_RL, pruebaRL, type = "response")
preds_RL_prueba <- sapply(probs_RL_prueba, comparar_umbral)
preds_RL_prueba <- factor(preds_RL_prueba, levels = levels(nuevos_datos[["es_clon"]]))



#Se revisa la curva ROC
roc_RL_entrenamiento <- roc(entrenamientoRL[["es_clon"]], probs_RL_entrenamiento)
plot(roc_RL_entrenamiento)

roc_RL_prueba <- roc(pruebaRL[["es_clon"]], probs_RL_prueba)
plot(roc_RL_prueba)

#Evaluar modelo para prueba
matrizRL_prueba <- confusionMatrix(preds_RL_prueba, pruebaRL[["es_clon"]])
print(matrizRL_prueba)

#Evaluar modelo para entrenamiento
matrizRL_entrenamiento <- confusionMatrix(preds_RL_entrenamiento, entrenamientoRL[["es_clon"]])
print(matrizRL_entrenamiento)

# Conclusiones
# No alcancé


# --------------- PREGUNTA 3 ---------------
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en donde un
# estudio o experimento, relacionado con el sentir de los estudiantes de la Universidad de Santiago respecto al retorno a
# la presencialidad, necesite utilizar una prueba de Friedman debido a problemas con la escala de la variable dependiente
# en estudio. Indique cuáles serÃ­an las variables involucradas en su ejemplo (con sus respectivos niveles) y las hipótesis
# nula y alternativa a contrastar.

# La directiva de la Universidad de Santiago de Chile desea conocer la satisfacción de sus estudiantes con respecto al retorno
# a clases presenciales de acuerdo a algunas medidas que ha tomado el establecimiento con respecto a convatir el coronavirus,
# brindandoles la confianza de volver a clases a sus estudiantes. Específicamente, se desea evaluar las variables de distanciamiento 
# físico (en la sala de clases), la sanitización (qué tan bien es realizado este y cada cuánto tiempo), y la seguridad (verificar 
# la temperatura de todas las personas que ingresan y si estas tienen sus vacunas al día). Esta evaluación, lo hace con ayuda 
# de la escala Likert de 6 puntos, donde 1 indica "estoy muy en desacuerdo", y el 6 indica "estoy muy de acuerdo". 
# Se destaca que que las variables independientes son "Distanciamiento", "Sanitización" y "Seguridad", mientras que la variable
# dependiente corresponde al nivel de satisfacción que posee cada estudiantes (en una escala del 1 al 6).

# Por tanto, se establecen las siguientes hipótesis:
# H0: Las variables evaluadas tienen resultados positivos (mayor o igual a 5).
# HA: Al menos una de las variables evaluadas tiene un resultado no positivo (mayor o igual a 5).

#      Usuario    Distanciamiento    Sanitización    Seguridad
#       1              5                3              6
#       2              4                1              4
#      ...            ...              ...            ...






