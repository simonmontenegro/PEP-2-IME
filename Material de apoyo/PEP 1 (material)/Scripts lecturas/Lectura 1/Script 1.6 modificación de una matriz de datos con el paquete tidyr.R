library ( dplyr )
library ( tidyr )
# Crear el data frame .
Instancia <- 1:6
Quicksort <- c(23.2 , 22.6 , 23.4 , 23.3 , 21.8 , 23.9)
Bubblesort <- c(31.6 , 29.3 , 30.7 , 30.8 , 29.8 , 30.3)
Radixsort <- c(30.1 , 28.4 , 28.7 , 28.3 , 29.9 , 29.1)
Mergesort <- c(25.0 , 25.7 , 25.7 , 23.7 , 25.5 , 24.7)
datos <- data.frame ( Instancia , Quicksort , Bubblesort , Radixsort , Mergesort )
# Mostrar las primeras filas de la matriz de datos .
cat (" Datos originales \n")
print ( head ( datos ))
cat ("\n")

# Convertir la matriz de datos a formato largo .
datos_largos <- datos %> % pivot_longer (c(" Quicksort ", " Bubblesort ",
                                               " Radixsort ", " Mergesort "),
                                             names_to = " Algoritmo ",
                                             values_to = " Tiempo ")
# Mostrar las primeras filas de la matriz de datos largos .
cat (" Datos largos \n")
print ( head ( datos_largos ))
cat ("\n")
# Convertir la matriz de datos largos a formato ancho .
datos_anchos <- datos_largos %> % pivot_wider ( names_from = " Algoritmo ",
                                                     values_from = " Tiempo ")
# Mostrar las primeras filas de la matriz de datos largos .
cat (" Datos anchos \n")
print ( head ( datos_anchos ))
cat ("\n")