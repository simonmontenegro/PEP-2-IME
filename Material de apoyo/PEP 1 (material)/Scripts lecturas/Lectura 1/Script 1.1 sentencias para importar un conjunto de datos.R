# Cargar un conjunto de datos disponible en R.
datos1 <- mtcars
# Importar desde un archivo de texto plano delimitado por tabuladores .
datos2 <- read.delim(file.choose ())
# Importar desde un archivo de valores separados por coma
# en formato ingl és ( figura 1.2 b).
datos3 <- read.csv("C:\\ Inferencia \\ ejemplo1 -csv -eng.csv ")
# Configurar carpeta de trabajo
setwd ("C:\\ Inferencia ")
# Importar desde un archivo de valores separados por coma
# en formato espa ñol ( figura 1.2 c).
datos4 <- read.csv2 (" ejemplo1 -csv -esp. csv")
# Mostrar las primeras 6 filas del conjunto de datos
# almacenado en datos1 .
head ( datos1 )
# Mostrar las ú ltimas 6 filas del conjunto de datos
# almacenado en datos1 .
tail ( datos1 )