library(ggpubr)
library(pwr)
library(ggplot2)
library(tidyverse)

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

# CONCLUSIONES
#




# --------------- PREGUNTA 2 ---------------

# Las hip�tesis a formular son:
# H0:
# HA: 

# Las condiciones a verificar:
# 1.
# 2.
# 3.
# 4.


# CONCLUSIONES
#