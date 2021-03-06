library ( dplyr )
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv", stringsAsFactors = TRUE ,
                      row.names = 1)
# C� lculo de varias medidas para la variable Potencia .
medidas_potencia <- datos %> % summarise ( Media = mean ( Potencia ),
                                             Mediana = median ( Potencia ),
                                             Varianza = var( Potencia ),
                                             IQR = IQR( Potencia ))
print ( medidas_potencia )
cat ("\n")
# C� lculo de la media y la desviaci �n est� ndar para las variables Peso y
# Cuarto _ milla .
medidas_varias <- datos %> % summarise ( Media_P = mean ( Peso ),
                                           Media_C = median ( Cuarto_milla ),
                                           SD_P = sd( Peso ),
                                           SD_C = sd( Cuarto_milla ))
print ( medidas_varias )
cat ("\n")
