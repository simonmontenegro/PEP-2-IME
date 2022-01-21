library ( ggpubr )
# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv", stringsAsFactors = TRUE ,
                      row.names = 1)
g <- ggboxplot ( datos [[" Potencia "]],
                 color = " red",
                 fill = " pink ",
                 ylab = " Potencia [hp]")
g <- g + rremove ("x. ticks ")
g <- g + rremove ("x. text ")
g <- g + rremove ("x. title ")
print (g)