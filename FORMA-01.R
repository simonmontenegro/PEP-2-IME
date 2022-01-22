library(ggpubr)
library(pwr)
library(ggplot2)
library(tidyverse)
library(ez)

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
# se debe planear un abordaje mediante ANOVA de una vía para muestras independientes, esto pues 
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
# distintas divisiones. Por tanto, es necesario realizar un análisis Post-Hoc para saber en qué divisiones se 
# encuentran esas diferencias.
# Esto es muy evidente si analiza el gráfico del tamaño del poder, donde "Cavetrooper" y "Spacetrooper" se desmarcan
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
# Spacetrooper y las demás, como también entre la división Cavetrooper y las demás (esto se infiere a partir del análisis
# del p-value conseguido para cada una de las diferencias, donde las divisiones mencionadas presentan un p-value ínfimo).
# Por otro lado, el resto de divisiones presentan un p-value mucho mayor que el nivel de signficancia 0.01, esto se
# verifica a partir del gráfico del tamaño del poder, donde las seis divisiones están centradas casi en 650 y presentan
# leves diferencias entre sí. Podría destacarse que la división "Shoretrooper" presenta una diferencia que se escapa, sin
# embargo, esta diferencia no llega a ser significativa.

# A partir de lo concluido, se debe notificar al Lord Sith de que aquellas divisiones que presentan diferencias 
# significativas son las divisiones: "Spacetrooper" y "Cavetrooper".


# --------------- PREGUNTA 2 ---------------
# A fin de determinar si es necesario establecer programas de entrenamiento diferenciados para clones y
# reclutas, Lord Vader quiere saber si es posible distinguir entre ambas clases de soldados con los datos actuales. Para ello,
# ha solicitado evaluar un modelo clasificador que contemple entre 2 y 5 variables predictoras. Considere que, para ser
# aceptable, el modelo:
# • Debe lograr una exactitud (accuracy) de al menos 0,8 en datos de prueba
# • No puede considerar casos con demasiada influencia (considerando la distancia de Cook)
# • No debe presentar autocorrelación (usando la prueba de Durbin-Watson para un retardo y un nivel de significación α = .01)
# • No debe presentar multicolinealidad severa (considerando el factor de inflación de la varianza, con un VIF promedio
#   inferior a 1,03).
# Considere la semilla 21 para obtener una muestra de 400 datos, 80% de los cuales serán empleados para ajustar el
# modelo y el 20% restante, para evaluarlo.

# Dado que se requiere evaluar un modelo clasificador con 2-5 variables predictoras, estamos ante un modelo de tipo regresión múltiple,
# específicamente, como la variable a "predecir" corresponde a una variable dicotómica (es clon / es recluta) se trata de una
# regresión logística. Ante esto, se procede a realizar el paso a paso para evaluar el modelo.

# Se establece la semilla según enunciado (21)
semilla <- 21
set.seed(semilla)

# Se define la siguiente función para expresar la variable dicotómica de manera numérica (1 o 0); 0 para reclutas, 1 para clones.
es_clon <- function(x){
  if(x == "N"){
    return(0)
  }else{
    return(1)
  }
}

#Se transforma el string de "es_clon" y se expresa numéricamente (0 o 1)
datos[["es_clon"]] <- sapply(datos[["es_clon"]], es_clon)

#Se pegan ambas columnas de datos a los datos de la muestra
#datos_mujeres <- cbind(datos_mujeres, IMC, EN)



# Las hipótesis a formular son:
# H0:
# HA: 

# Las condiciones a verificar:
# 1.
# 2.
# 3.
# 4.


# CONCLUSIONES
#


# --------------- PREGUNTA 3 ---------------
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en donde un
# estudio o experimento, relacionado con el sentir de los estudiantes de la Universidad de Santiago respecto al retorno a
# la presencialidad, necesite utilizar una prueba de Friedman debido a problemas con la escala de la variable dependiente
# en estudio. Indiqué cuáles serían las variables involucradas en su ejemplo (con sus respectivos niveles) y las hipótesis
# nula y alternativa a contrastar.




