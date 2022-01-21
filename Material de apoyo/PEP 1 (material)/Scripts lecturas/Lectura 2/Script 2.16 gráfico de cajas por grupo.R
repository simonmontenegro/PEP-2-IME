library ( ggpubr )
# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv ", stringsAsFactors = TRUE ,
                    row.names = 1)
g <- ggboxplot (datos , x = " Cambios ",
                y = " Rendimiento ",
                palette = c(" light blue ", " pink ", " yellow "),
                fill = " Cambios ",
                title = " Rendimiento por cantidad de cambios ",
                xlab = " Cambios ",
                ylab = " Rendimiento [ millas /gal ón]")
print (g)