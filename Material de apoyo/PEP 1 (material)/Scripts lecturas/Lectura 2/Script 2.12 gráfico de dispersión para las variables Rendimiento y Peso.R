library ( ggpubr )
# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv ", stringsAsFactors = TRUE ,
                      row.names = 1)
# Crear grá fico de dispersi ón.
g <- ggscatter (datos ,
                x = " Rendimiento ",
                y = " Peso ",
                color = " red",
                title = " Rendimiento v/s peso ",
                xlab = " Rendimiento [ millas /gal ón]",
                ylab = " Peso [1000 lb]")
print (g)