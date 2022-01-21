# Las condiciones a verificar:
# 1. La variable independiente debe ser categórica y tener a lo menos tres niveles.
# 2. La escala de la variable dependiente debe ser, a lo menos, ordinal.
# 3. Los sujetos son una muestra aleatoria e independiente de la población.

# Las hipótesis a formular son:
# H0: Las interfaces tienen preferencias similares.
# HA: Al menos una interfaz obtiene una preferencia distinta a las demás.


# Construir la matriz de datos
A <- c(21, 10, 7, 21, 24, 27, 17)
B <- c(6, 21, 18, 7, 24, 13, 13)
C <- c(13, 25, 18, 20, 24, 8, 29)

Puntuación <- c(A, B, C)

Interfaz <- c(rep("A", length(A)),
              rep("B", length(B)),
              rep("C", length(C)))

Sujeto <- rep(1:7, 3)

Interfaz <- factor(Interfaz)

datos <- data.frame(Sujeto, Puntuacion, Interfaz)

# Establecer nivel de significacion
alfa <- 0.05

# Hacer la prueba de friedman
prueba <- friedman.test(Puntuacion ~ Interfaz | Sujeto, data = datos)
print(prueba)

# Efectuar procedimiento post-hoc de Holm si se encuentran diferencias significativas
if(prueba$p.value < alfa){
  post_hoc <- pairwise.wilcox.test(datos$Puntuacion,
                                   datos$Interfaz,
                                   p.adjust.method = "holm",
                                   paied = TRUE)
  print(post_hoc)
}

# CONCLUSIÓN
# Considerando un nivel de significacion 0.05 y un p igual a 0.507, se falla al rechazar la 
# hipotesis nula. En consecuencia, se concluye con un 95% de confianza que no hay diferencias 
# significativas de preferencia entre las distintas interfaces.