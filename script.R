data <- read.table("proceso.txt", header=T)
Y    <- data$y
X1   <- data$x1
X2   <- data$x2
X3   <- data$x3
X4   <- data$x4
X5   <- data$x5
X6   <- data$x6
X7   <- data$x7
Cent <- data$Cent

# 1. Realizar un análisis descriptivo de los datos.
par(mfrow = c(3,3))

hist(Y, main="Óxido de aluminio que precipita.", xlab="Porcentaje", ylab="Frecuencia")
hist(X1, main="Concentración del ácido.", xlab="Concentración", ylab="Frecuencia", ylim=c(0,25))
hist(X2, main="Valor final del pH de la solución.", xlab="Unidades de pH", ylab="Frecuencia")
hist(X3, main="Temperatura del proceso", xlab="Grados centígrados", ylab="Frecuencia", ylim=c(0,70))
hist(X4, main="Concentración de la base.", xlab="Concentración", ylab="Frecuencia")
hist(X5, main="Velocidad de agitación", xlab="Revoluciones por minuto", ylab="Frecuencia")
hist(X6, main="Velocidad de adición de la base.", xlab="Mililitros por hora", ylab="Frecuencia", ylim=c(0,60))
hist(X7, main="Tiempo del proceso.", xlab="Horas", ylab="Frecuencia",ylim=c(0,60))
barplot(table(Cent), main="Centro de Investigación.", ylab="Frecuencia",ylim=c(0,70))

print(summary.data.frame(data))

# 2. Realizar un gráfico de dispersión y una matriz de correlación de las variables.
dataMatrix <- data
dataMatrix$Cent <- NULL
attach(dataMatrix)
pairs(dataMatrix)
cor(dataMatrix)

# 3. Realizar una prueba de bondad de ajuste para determinar si la variable “y” se distribuye en forma normal.

n = 220
k <- round(sqrt(n))
A <- ceiling((max(data$y) - min(data$y)) / k)
alpha <- 0.05
r <- 2
while (A*(k-1) > max(data$y)){
  k <- k-1
}
fi <- numeric(k)
mi <- numeric(k)

for (x in 1:k){
  fi[x] <- length(data[ which(data$y < A*x & data$y >= A*(x-1)), ]$y)
  mi[x] <- (x-1)*A + (A/2)
}
qqnorm(fi)
qqline(fi)

xbarra <- sum(fi*mi)/n
x_barra <- rep(xbarra, k)
S_cuadrado <- sum( fi*(mi-x_barra)^2 )/(n-1)
S <- sqrt(S_cuadrado)
pi <- pnorm((mi + A/2), xbarra, S) - pnorm((mi - A/2), xbarra, S)
chi2_obs <- sum((fi-n*pi)^2/(n*pi))
chi2_alpha <- qchisq(1 - alpha, k - 1 -r)
p_valor <- 1 - pchisq(chi2_obs, k - 1 - r) 

# 4. Realice un intervalo de confianza del 93% para cada variable en estudio.

t.test(Y, conf.level = 0.93)
t.test(X1, conf.level = 0.93)
t.test(X2, conf.level = 0.93)
t.test(X3, conf.level = 0.93)
t.test(X4, conf.level = 0.93)
t.test(X5, conf.level = 0.93)
t.test(X6, conf.level = 0.93)
t.test(X7, conf.level = 0.93)

# 5. Halle un modelo lineal que explique mejor la variable “y”. Incluya todas las pruebas necesarias para
# llegar a este modelo, así como un análisis de residuos del modelo final.

modelo1 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7)
summary(modelo1)
# Sin Intercept
modelo2 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 - 1)
summary(modelo2)
# Sin X4
modelo3 <- lm(Y ~ X1 + X2 + X3 + X5 + X6 + X7 - 1)
summary(modelo3)
# Sin X1
modelo4 <- lm(Y ~ X2 + X3 + X5 + X6 + X7 - 1)
summary(modelo4)
# Sin X3
modelo5 <- lm(Y ~ X2 + X5 + X6 + X7 - 1)
summary(modelo5)
# Sin X5
modelo6 <- lm(Y ~ X2 + X6 + X7 - 1)
summary(modelo6)
# Sin X2
modelo7 <- lm(Y ~ X6 + X7 - 1)
summary(modelo7)

# 6. Con los datos proceso_pre.txt haga una predicción de la variable “y” (con el mejor modelo) y haga un
# histograma, diagrama de cajas y resumen estadístico de los residuos de predicción (valor observado vs.
# prediccion del modelo) para concluir con relación al poder predictivo del modelo.

data_pred <- read.table("proceso_pre.txt", header=T)
ybarra <- numeric(80)
for (x in 1:length(data_pred$y)) {
  # Modelo numero 2
  ybarra[x] <- (modelo7$coefficients["X6"]*data_pred$x6[x]) + (modelo7$coefficients["X7"]*data_pred$x7[x])
}

yobs <- data_pred$y
resY <- yobs - ybarra
(sse <- sum((resY)^2))
(varianza <- sse/(length(data_pred$y)-1))
