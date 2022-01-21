library ( ggpubr )
# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv", stringsAsFactors = TRUE ,
                      row.names = 1)
# Crear la tabla de frecuencias para la variable Cambios y convertirla a
# data frame .
contingencia <- as.data.frame ( xtabs (~ Cambios , data = datos ))
# Crear el gráico de barras .
g <- ggbarplot ( contingencia ,
                 x = " Cambios ",
                 y = " Freq ",
                 fill = c(" brown ", " purple ", " orange "),
                 title = " Cantidad de cambios de los autom ó viles ",
                 xlab = " Cantidad de cambios ",
                 ylab = " Frecuencia ")
print (g)