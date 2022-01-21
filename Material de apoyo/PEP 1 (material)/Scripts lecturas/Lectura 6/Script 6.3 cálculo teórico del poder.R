library ( ggpubr )
library (pwr)
# Fijar valores conocidos .
sigma <- 12
alfa <- 0.05
n <- 36
# Calcular el error está ndar .
SE <- sigma / sqrt (n)
# Grá ficar la distribuci ón muestral de la media de las diferencias si

# la hipó tesis nula fuera verdadera .
x <- seq (-6 * SE , 4 * SE , 0.01)
y <- dnorm (x, mean = media_nula , sd = SE)
g <- ggplot ( data = data.frame (x, y), aes(x))
g <- g + stat_function (
  fun = dnorm ,
  args = list ( mean = media_nula , sd = SE),
  colour = "red ", size = 1)
  g <- g + ylab ("")
g <- g + scale_y_continuous ( breaks = NULL )
g <- g + scale_x_continuous ( name = " Diferencia en tiempos de ejecuci ón [ms]",
                                breaks = seq (-6, 4, 2))
g <- g + theme_pubr ()
# Colorear la regi ón de rechazo de la hipó tesis nula .
media_nula <- 0
Z_critico <- qnorm ( alfa /2, mean = media_nula , sd = SE , lower.tail = FALSE )
q_critico_inferior <- media_nula - Z_critico
q_critico_superior <- media_nula + Z_critico
g <- g + geom_area ( data = subset (df , x < q_critico_inferior ),
                       aes (y = y),
                       colour = "red ",
                       fill = "red",
                       alpha = 0.5)
g <- g + geom_area ( data = subset (df , x > q_critico_superior ),
                       aes (y = y),
                       colour = "red ",
                       fill = "red",
                       alpha = 0.5)
print (g)
# Superponer la distribuci ón muestral de la media de las diferencias
# si la la diferencia de medias fuera -4.
g <- g + stat_function (
  fun = dnorm ,
  args = list ( mean = media_efecto , sd = SE),
  colour = " blue ", size = 1)
  # Colorear la regi ón de la nueva curva situada en la regi ón de
  # rechazo de la curva original .
  x1 <- seq (-6 * SE , 4 * SE , 0.01)
y1 <- dnorm (x, mean = media_efecto , sd = SE)
g <- g + geom_area ( data = subset ( data.frame (x1 , y1),
                                       x < q_critico_inferior ),
                       aes (x = x1 , y = y1),
                       colour = " blue ",
                       fill = " blue ",
                       alpha = 0.5)
g <- g + geom_area ( data = subset ( data.frame (x1 , y1),
                                       x > q_critico_superior ),
                       aes (x = x1 , y = y1),
                       colour = " blue ",
                       fill = " blue ",
                       alpha = 0.5)
print (g)
# Calcular el poder de acuerdo al aná lisis teó rico .
poder <- pnorm (q_critico_inferior ,
                mean = media_efecto ,
                sd = SE ,
                lower.tail = TRUE )
+ pnorm (q_critico_superior ,
         mean = media_efecto ,
         sd = SE ,
         lower.tail = FALSE )
cat (" Poder = ", poder , "\n")
# Calcular la probabilidad de cometer un error tipo II.
beta <- 1 - poder_teorico
cat (" Beta = ", beta , "\n")