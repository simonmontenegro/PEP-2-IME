# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv", stringsAsFactors = TRUE ,
                      row.names = 1)
# Crear tabla de contingencia para las variables Transmision y gear .
contingencia <- table ( datos [[" Transmision "]], datos [[" Cambios "]])
cat (" Tabla de contingencia generada con table () :\n")
print ( contingencia )
cat ("\n")
# Otra forma de crear la misma tabla .
contingencia <- xtabs (~ Transmision + Cambios , data = datos )
cat (" Tabla de contingencia generada con xtabs () :\n")
print ( contingencia )
cat ("\n")
# Proporciones con totales por fila .
proporciones_fila <- prop.table ( contingencia , margin =1)
proporciones_fila <- addmargins ( proporciones_fila , margin =2)
cat (" Tabla de contingencia con proporciones totales por fila :\n")
print ( proporciones_fila )
cat ("\n")
# Proporciones con totales por columna .
proporciones_columna <- prop.table ( contingencia , margin =2)
proporciones_columna <- addmargins ( proporciones_columna , margin =1)
cat (" Tabla de contingencia con proporciones totales por columna :\n")
print ( proporciones_columna )
cat ("\n")
# Proporciones con totales .
proporciones <- prop.table ( contingencia )
proporciones <- addmargins ( proporciones )
cat (" Tabla de contingencia con proporciones totales :\n")
print ( proporciones )
cat ("\n")