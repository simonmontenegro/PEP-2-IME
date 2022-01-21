library ( ggpubr )
# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv ", stringsAsFactors = TRUE ,
                      row.names = 1)
g <- ggstripchart (datos , x = " Cambios ",
                   y = " Rendimiento ",
                   palette = c(" blue ", " red", " dark green "),
                   color = " Cambios ",
                   title = " Rendimiento por cantidad de cambios ",
                   xlab = " Cambios ",
                   ylab = " Rendimiento [ millas /gal ón]")
print (g)