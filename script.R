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
hist(X1, main="Concentración del Ácido.", xlab="Concentración", ylab="Frecuencia", ylim=c(0,25))
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

# 3. Realizar una prueba de bondad de ajuste para determinar si la variable "y" se distribuye en forma normal.

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
par(mfrow=c(1,1))
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

# 5. Halle un modelo lineal que explique mejor la variable "y". Incluya todas las pruebas necesarias para
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

# 6. Con los datos proceso_pre.txt haga una predicción de la variable "y" (con el mejor modelo) y haga un
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

hist(resY, main="Histograma de residuos (observaciones menos predicciones)", xlab="Residuo", ylab="Frecuencia", ylim=c(0,20), xlim=c(-30,30))
boxplot(resY, main="Histograma de residuos (observaciones menos predicciones)", ylab="Residuo")
summary(resY)

#7. Realice un análisis de varianza para decidir si las medias por centro son iguales, para cada variable.
CentA = data[ which(data$Cent == 'A'), ]
CentB = data[ which(data$Cent == 'B'), ]
CentC = data[ which(data$Cent == 'C'), ]
CentD = data[ which(data$Cent == 'D'), ]

# Variable Y
datosY <- c(CentA$y, CentB$y, CentC$y, CentD$y)
facY = c(replicate(55,"Oxido de aluminio A"), replicate(46,"Oxido de aluminio B"), replicate(51,"Oxido de aluminio C"), replicate(68,"Oxido de aluminio D"))
factrY = factor(facY)
tapply(datosY, factrY, mean)
boxplot(datosY ~ factrY)
modY.lm2 = lm(datosY ~ factrY)
anova(modY.lm2)
pairwise.t.test(datosY,factrY)

# Variable X1
datosX1 <- c(CentA$x1, CentB$x1, CentC$x1, CentD$x1)
facX1 = c(replicate(55,"Concentracion del acido A"), replicate(46,"Concentracion del acido B"), replicate(51,"Concentracion del acido C"), replicate(68,"Concentracion del acido D"))
factrX1 = factor(facX1)
tapply(datosX1, factrX1, mean)
boxplot(datosX1 ~ factrX1)
modX1.lm2 = lm(datosX1 ~ factrX1)
anova(modX1.lm2)
pairwise.t.test(datosX1,factrX1)

# Variable X2
datosX2 <- c(CentA$x2, CentB$x2, CentC$x2, CentD$x2)
facX2 = c(replicate(55,"Valor final del pH A"), replicate(46,"Valor final del pH B"), replicate(51,"Valor final del pH C"), replicate(68,"Valor final del pH D"))
factrX2 = factor(facX2)
tapply(datosX2, factrX2, mean)
boxplot(datosX2 ~ factrX2)
modX2.lm2 = lm(datosX2 ~ factrX2)
anova(modX2.lm2)
pairwise.t.test(datosX2,factrX2)

# Variable X3
datosX3 <- c(CentA$x3, CentB$x3, CentC$x3, CentD$x3)
facX3 = c(replicate(55,"Temperatura del proceso A"), replicate(46,"Temperatura del proceso B"), replicate(51,"Temperatura del proceso C"), replicate(68,"Temperatura del proceso D"))
factrX3 = factor(facX3)
tapply(datosX3, factrX3, mean)
boxplot(datosX3 ~ factrX3)
modX3.lm2 = lm(datosX3 ~ factrX3)
anova(modX3.lm2)
pairwise.t.test(datosX3,factrX3)

# Variable X4
datosX4 <- c(CentA$x4, CentB$x4, CentC$x4, CentD$x4)
facX4 = c(replicate(55,"Concentracion de la base A"), replicate(46,"Concentracion de la base B"), replicate(51,"Concentracion de la base C"), replicate(68,"Concentracion de la base D"))
factrX4 = factor(facX4)
tapply(datosX4, factrX4, mean)
boxplot(datosX4 ~ factrX4)
modX4.lm2 = lm(datosX4 ~ factrX4)
anova(modX4.lm2)
pairwise.t.test(datosX4,factrX4)

# Variable X5
datosX5 <- c(CentA$x5, CentB$x5, CentC$x5, CentD$x5)
facX5 = c(replicate(55,"Velocidad de agitacion A"), replicate(46,"Velocidad de agitacion B"), replicate(51,"Velocidad de agitacion C"), replicate(68,"Velocidad de agitacion D"))
factrX5 = factor(facX5)
tapply(datosX5, factrX5, mean)
boxplot(datosX5 ~ factrX5)
modX5.lm2 = lm(datosX5 ~ factrX5)
anova(modX5.lm2)
pairwise.t.test(datosX5,factrX5)

# Variable X6
datosX6 <- c(CentA$x6, CentB$x6, CentC$x6, CentD$x6)
facX6 = c(replicate(55,"Velocidad-adicion-base A"), replicate(46,"Velocidad-adicion-base B"), replicate(51,"Velocidad-adicion-base C"), replicate(68,"Velocidad-adicion-base D"))
factrX6 = factor(facX6)
tapply(datosX6, factrX6, mean)
boxplot(datosX6 ~ factrX6)
modX6.lm2 = lm(datosX6 ~ factrX6)
anova(modX6.lm2)
pairwise.t.test(datosX6,factrX6)

# Variable X7
datosX7 <- c(CentA$x7, CentB$x7, CentC$x7, CentD$x7)
facX7 = c(replicate(55,"Tiempo del Proceso A"), replicate(46,"Tiempo del Proceso B"), replicate(51,"Tiempo del Proceso C"), replicate(68,"Tiempo del Proceso D"))
factrX7 = factor(facX7)
tapply(datosX7, factrX7, mean)
boxplot(datosX7 ~ factrX7)
modX7.lm2 = lm(datosX7 ~ factrX7)
anova(modX7.lm2)
pairwise.t.test(datosX7,factrX7)
