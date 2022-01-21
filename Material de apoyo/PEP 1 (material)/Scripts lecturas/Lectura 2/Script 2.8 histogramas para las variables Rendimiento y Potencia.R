library ( ggpubr )
# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv", stringsAsFactors = TRUE ,
                      row.names = 1)
# Histograma para la variable Rendimiento .
g1 <- gghistogram (datos ,
                   x = " Rendimiento ",
                   bins = 10,
                   add = " mean ",
                   xlab = " Rendimiento [ Millas /gal ón]",
                   ylab = " Frecuencia ",
                   color = " blue ",
                   fill = " blue ")
print (g1)
# Histograma para la variable Potencia .
g2 <- gghistogram (datos ,
                   x = " Potencia ",
                   bins = 10,
                   add = " mean ",
                   xlab = " Potencia [hp]",
                   ylab = " Frecuencia ",
                   color = " red",
                   fill = " yellow ")
print (g2)