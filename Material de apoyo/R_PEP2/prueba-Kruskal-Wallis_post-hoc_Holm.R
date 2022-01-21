# Las condiciones a verificar:
# 1. La variable independiente debe tener a lo menos dos niveles.
# 2. La escala de la variable dependiente debe ser, a lo menos, ordinal.
# 3. Las observaciones son independientes entre sí.

# Las hipótesis a contrastar son:
# H0: Todos los algoritmos son igual de eficientes (o, de manera similar, ningún algoritmo
#     es menos ni más eficiente que los demás)
# HA: Al menos uno de los algoritmos presenta una eficiencia diferente a al menos algún
#     otro algoritmo.

# Construir la matriz de datos
A <- c(24, 23, 26, 21, 24, 24, 25, 22, 23, 22, 23, 23)
B <- c(17, 15, 18, 20, 19, 21, 20, 18, 19)
C <- c(10, 11, 14, 11, 15, 12, 12, 0, 9, 13, 12, 12, 10, 10)
D <- c(18, 16, 18, 15, 16, 15, 18, 16)
Tiempo <- c(A, B, C, D)

Algoritmo <- c(rep("A", length(A)),
             rep("B", length(B)),
             rep("C", length(C)),
             rep("D", length(D)))
             
Algoritmo <- factor(Algoritmo)

datos <- data.frame(Tiempo, Algoritmo)

# Establecer nivel de significación
alfa <- 0.01

# hacer la prueba de Kruskal-Wallis
prueba <- kruskal.test(Tiempo ~Algoritmo, data = datos)
print(prueba)

# Efectuar procedimiento post-hoc de Holm si se encuentran diferencias significativas
if(prueba$p.value < alfa){
  post_hoc <- pairwise.wilcox.test(datos$Tiempo,
                                   datos$Algoritmo,
                                   p.adjus.method = "holm",
                                   paired = FALSE)
  print(post_hoc)
}

# CONCLUSIÓN
# Se concluye con un 99% de confianza que eisten diferencias significativas entre los 
# tiempos promedio de jecución de los algoritmos A, B, C, y D. Además, a partir de los
# resultados del procedimiento post-hoc, considerando un nivel de significación 0.01, 
# se puede concluir con un 99% de confianza que existen diferencias significativas entre 
# los tiempos promedio de ejecución de todos los pares de algoritmos con excepción de los 
# algoritmos B y D.
