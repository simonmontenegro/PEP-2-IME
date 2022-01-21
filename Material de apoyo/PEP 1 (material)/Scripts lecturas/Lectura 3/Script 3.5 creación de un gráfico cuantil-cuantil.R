library ( ggpubr )
# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv", stringsAsFactors = TRUE ,
                      row.names = 1)
# Grá fico Q-Q para la variable Rendimiento .
g <- ggqqplot (datos ,
               x = " Rendimiento ",
               color = " red")
print (g)