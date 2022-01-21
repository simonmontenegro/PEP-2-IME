library ( dplyr )
# Cargar dataframe iris incluido en R.
datos <- iris
# Seleccionar observaciones correspondientes a la especie versicolor .
versicolor <- datos %> % filter ( Species == " versicolor ")
# Seleccionar observaciones de la especie versicolor cuyos sé palos tengan una
# longitud igual o superior a 6 cm.
largas <- datos %> % filter ( Species == " versicolor " & Sepal.Length >= 6)
# Seleccionar la especie y variables relativas a los pé talos .
petalos <- datos %> % select ( Species , starts_with (" Petal "))
# Seleccionar variables de ancho y la especie .
anchos <- datos %> % select ( ends_with (" Width "), Species )
# Agregar al conjunto de datos de los pé talos una nueva variable con la razón
# entre el largo y el ancho de é stos .
petalos <- petalos %> % mutate ( Species , Petal.Width ,
                                 Petal.Ratio = Petal.Length / Petal.Width )
# Ordenar el conjunto de datos de pé talos en forma descendente seg ún la razón
# de los pé talos .
petalos <- petalos %> % arrange ( desc ( Petal.Ratio ))
# Ordenar el conjunto de datos de pé talos en forma ascendente según el largo de
# los pé talos .
petalos <- petalos %> % arrange ( Petal.Length )