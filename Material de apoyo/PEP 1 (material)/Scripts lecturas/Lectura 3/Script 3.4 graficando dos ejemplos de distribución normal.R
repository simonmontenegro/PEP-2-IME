library ( ggpubr )
# Generar valores para una distribuci ón normal con media 0 y
# desviaci ón está ndar 1.
media <- 0
desv_est <- 1
x <- seq ( -15, 35, 0.01)
y <- dnorm (x, mean = media , sd = desv_est)
normal_1 <- data.frame (x, y)
# Repetir el proceso para una distribuci ón normal con media 10
# y desviaci ón está ndar 6.
media <- 10
desv_est <- 6
x <- seq ( -15, 35, 0.01)
y <- dnorm (x, mean = media , sd = desv_est)
normal_2 <- data.frame (x, y)
# Graficar ambas distribuciones .
g <- ggplot ( normal_1, aes(x, y)) + geom_line ( color = " blue ")
g <- g + geom_line ( data = normal_2, color = "red")
g <- g + theme_pubr ()
print (g)

