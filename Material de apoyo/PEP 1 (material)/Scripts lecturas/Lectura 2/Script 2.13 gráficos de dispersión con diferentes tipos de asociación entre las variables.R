library ( ggpubr )
# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv ", stringsAsFactors = TRUE ,
                      row.names = 1)
# Grá fico para variables independientes .
g1 <- ggscatter (datos ,
                 x = " Peso ",
                 y = " Cuarto _ milla ",
                 color = " blue ",
                 title = " Independientes ",
                 xlab = " Peso [1000 lb]",
                 ylab = " Tiempo para recorrer un cuarto de milla [s]")
# Grá fico para variables con asociaci ón positiva .
g2 <- ggscatter (datos ,
                 x = " Peso ",
                 y = " Potencia ",
                 color = " orange ",
                 title = " Asociaci ón positiva ",
                 xlab = " Peso [1000 lb]",
                 ylab = " Potencia [hp]")
# Grá fico para variables con asociaci ón negativa .
g3 <- ggscatter (datos ,
                 x = " Peso ",
                 y = " Rendimiento ",
                 color = " black ",
                 title = " Asociaci ón negativa ",
                 xlab = " Peso [1000 lb]",
                 ylab = " Rendimiento [ millas /gal ón]")
# Crear figura con tres grá ficos .
g <- ggarrange (g1 ,g2 ,g3 , ncol = 3, nrow = 1, common.legend = TRUE )
print (g)