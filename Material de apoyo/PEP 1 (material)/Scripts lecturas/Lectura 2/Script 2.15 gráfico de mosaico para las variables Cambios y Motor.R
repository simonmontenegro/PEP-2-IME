library ( ggmosaic )
# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv", stringsAsFactors = TRUE ,
                      row.names = 1)
# Crear tabla de contingencia para las variables gear y vs ,
# y guardarla como data frame .
tabla <- xtabs (~ Cambios + Motor , data = datos )
contingencia <- as.data.frame ( tabla )
# Crear grá fico de mosaico .
g <- ggplot ( data = contingencia )
g <- g + geom_mosaic (aes( weight = Freq , x = product ( Cambios ), fill = Motor ))
g <- g + labs (y = " Motor ", x = " Cambios ",
               title = " Tipo de motor por cantidad de cambios ")
g <- g + scale_fill_manual ( values =c(" orange ", " purple "))
print (g)