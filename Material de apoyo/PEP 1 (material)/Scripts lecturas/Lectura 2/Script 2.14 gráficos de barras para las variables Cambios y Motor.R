library ( ggpubr )
# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv", stringsAsFactors = TRUE ,
                      row.names = 1)
# Crear tabla de contingencia para las variables Motor y Cambios ,
# y guardarla como data frame .
tabla <- xtabs (~ Motor + Cambios , data = datos )
contingencia <- as.data.frame ( tabla )
# Crear grá fico de barras segmentadas .
g1 <- ggplot ( contingencia , aes( fill = Motor , y = Freq , x = Cambios ))
g1 <- g1 + geom_bar( position = " stack ", stat = " identity ")
g1 <- g1 + labs (y = " Frecuencia ") + ggtitle (" Barras apiladas ")
g1 <- g1 + theme_pubr ()
# Crear grá fico de barras agrupadas .
g2 <- ggplot ( contingencia , aes( fill = Motor , y = Freq , x = Cambios ))
g2 <- g2 + geom_bar( position = " dodge ", stat = " identity ")
g2 <- g2 + labs (y = " Frecuencia ") + ggtitle (" Barras agrupadas ")
g2 <- g2 + theme_pubr ()
# # Crear grá fico de barras segmentadas estandarizado .
g3 <- ggplot ( contingencia , aes( fill = Motor , y = Freq , x = Cambios ))
g3 <- g3 + geom_bar( position = " fill ", stat = " identity ")
g3 <- g3 + labs (y = " Frecuencia ") + ggtitle (" Barras estandarizadas ")
g3 <- g3 + theme_pubr ()
# Crear una figura que contenga los tres grá ficos .
g <- ggarrange (g1 , g2 , g3 , nrow = 1, common.legend = TRUE )
# Agregar un tí tulo com ún en negrita y con fuente de 24 puntos .
titulo <- text_grob (" Tipo de motor por cantidad de Cambios ",
                       face = " bold ", size = 24)
g <- annotate_figure (g, top = titulo )
# Guardar la figura en formato png con tama ño 960 x 480 pixeles .
ggexport (g, filename = "C:/ Inferencia /f-barras -2. png",
          height = 480 , width = 960)