# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv", stringsAsFactors = TRUE ,
                      row.names = 1)
# Convertir la variable Cambios en categ ó rica .
datos [[" Cambios "]] <- factor ( datos [[" Cambios "]])
# Crear tabla de contingencia para las variables Transmision ,
# Cambios y Motor .
contingencia <- ftable ( datos [[" Transmision "]], datos [[" Cambios "]],
                         datos [[" Motor " ]])
cat (" Tabla de contingencia generada con ftable () :\n")
print ( contingencia )
cat ("\n")
# Otra forma de crear la misma tabla .
xtabs (~ Cambios + Transmision + Motor , data = datos )
cat (" Tabla de contingencia generada con xtabs () :\n")
print ( contingencia )
cat ("\n")