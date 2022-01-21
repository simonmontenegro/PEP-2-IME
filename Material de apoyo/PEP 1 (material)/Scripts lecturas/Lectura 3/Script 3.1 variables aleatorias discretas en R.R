library ( discreteRV )
# Crear una variable discreta para representar el dado
# adulterado de la tabla 3.1.
resultados <- 1:6
probabilidades = c(0.25 , 0.125 , 0.125 , 0.125 , 0.125 , 0.25)
X <- RV( outcomes = resultados , probs = probabilidades )
# Calcular el valor esperado .
esperado <- E(X)
cat (" Valor esperado :", esperado , "\n")
# Calcular la varianza .
varianza <- V(X)
cat (" Varianza :", varianza , "\n")
# Calcular la desviaci ón está ndar .
desviacion <- SD(X)
cat (" Desviaci ón está ndar :", desviacion , "\n")