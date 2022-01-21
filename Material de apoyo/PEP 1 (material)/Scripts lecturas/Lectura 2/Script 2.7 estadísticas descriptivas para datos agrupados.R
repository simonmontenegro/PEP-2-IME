library ( dplyr )
# Cargar datos .
datos <- read.csv2 ("C:/ Inferencia / Mtcars .csv ", stringsAsFactors = TRUE ,
                      row.names = 1)
resumen <- group_by(datos , Cambios ) %> %
  summarise ( count = n() , mean ( Rendimiento ), median ( Rendimiento ),
              sd( Rendimiento ), IQR( Rendimiento ), mean ( Potencia ))
print ( resumen )