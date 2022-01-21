library ( dplyr )
# Cargar conjunto de datos .
datos <- mtcars
# Renombrar columnas .
datos <- datos %> % rename ( Rendimiento = mpg , Cilindrada = cyl ,
                             Desplazamiento = disp , Potencia = hp ,
                             Eje = drat , Peso = wt , Cuarto_milla = qsec ,
                             Motor = vs , Transmision = am , Cambios = gear ,
                             Carburadores = carb )
# Dar formato categ ó rico a las variables Motor y Transmision , renombrando
# sus niveles .
datos [[" Motor "]] <- factor ( datos [[" Motor "]], levels = c(0, 1) ,
                                labels = c("V", " Recto "))
datos [[" Transmision "]] <- factor ( datos [[" Transmision "]], levels = c(0, 1) ,
                                      labels = c(" Autom á tico ", " Manual "))
# Dar formato ordinal a las variables Cilindrada y Cambios , renombrando
# sus niveles .
datos [[" Cilindrada "]] <- factor ( datos [[" Cilindrada "]], levels = c(4, 6, 8) ,
                                     labels = c("4 cilindros ", "6 cilindros ",
                                                "8 cilindros "),
                                     ordered = TRUE )
datos [[" Cambios "]] <- factor ( datos [[" Cambios "]], levels = c(3, 4, 5) ,
                                  labels = c("3 cambios ", "4 cambios ", "5 cambios "),
                                  ordered = TRUE )
write.csv2 ( datos , "C:/ Inferencia / Mtcars .csv ")