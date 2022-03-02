## ------------------------------------- 
# Author: Jan Sebastian Velasquez Acevedo
# Code:  1744936
## -------------------------------------

# cargar paquetes
library(readr)
library(here)

# Importar csv
data <-read.csv("FIFA_2018_Statistics.csv")

#-------------------------------------------------------------------------
#Regresion Lineal Simple
#Para nuestro modelo lineal simple, tomamos:
#Posesion de balon (Ball.Poseesion) : x -> Variable independiente-regresora
#Oportunidades de gol (Atttemps) : y -> Variable dependiente-respuesta
# Modelo Lineal para la posesion de balon y las oportunidades de gol
modLinealSimple = lm(data$Attempts ~ data$Ball.Possession..)
modLinealSimple

#Summary para insppeccionar
#Y: Variable respuesta o dependiente.
#X: Variable predictora o independiente. data$Ball.Possession..
#βo: Intercepto de la línea con el eje Y. (Intercept)
#β1: Pendiente de la línea de regresión.
#Coeficiente de determinacion : r.squared
#Sigma^2->varianza del error del modelo

summary(modLinealSimple)
summary(modLinealSimple)$r.square
summary(modLinealSimple)$sigma^2

#Grafica
plot(data$Ball.Possession..,data$Attempts,
     main = "Possession vs Attemps",
     xlab = "Posesión de Balón",
     ylab = "Oportunidades de gol",
     col = "blue")
abline(modLinealSimple,col='red')

#Errores residuales
error1 <- residuals(modLinealSimple)
hist(error1)
#-------------------------------------------------------------------------
#Regresion Lineal Simple 2
#Para nuestro modelo lineal simple, tomamos:
#Posesion de balon (Passes) : x -> Variable independiente-regresora
#Oportunidades de gol (Ball.Possession) : y -> Variable dependiente-respuesta
# Modelo Lineal para la posesion de balon y el número de pases exitosos
modLinealSimple2 = lm(data$Ball.Possession.. ~ data$Passes)
modLinealSimple2

#Summary para insppeccionar
#Y: Variable respuesta o dependiente.
#X: Variable predictora o independiente. data$Ball.Possession..
#βo: Intercepto de la línea con el eje Y. (Intercept)
#β1: Pendiente de la línea de regresión.
#Coeficiente de determinacion : r.squared
#Sigma^2->varianza del error del modelo

summary(modLinealSimple2)
summary(modLinealSimple2)$r.square
summary(modLinealSimple2)$sigma^2

#Graficas
plot(data$Passes,data$Ball.Possession..,
     main = "Passes vs Possession",
     xlab = "Número de pases exitosos",
     ylab = "Posesión del balón",
     col = "blue")
abline(modLinealSimple2,col='red')


#Errores residuales
error2 <- residuals(modLinealSimple2)
hist(error2)

#-------------------------------------------------------------------------
# Regresion Lineal Multiple
# Analizamos en este caso la relación entre la posesión del balón (Ball.Possession)
# las oportunidades de gol (Attemps) y los tiros libres (Free.Kicks) 

modLinealMultiple = lm(data$Attempts ~ data$Ball.Possession..+data$Free.Kicks)
modLinealMultiple


#Graficos
plot(data$Ball.Possession..,data$Attempts,
     main = "Ball Possession vs Attempts (Considering free kicks)",
     xlab = "Posesión de balón",
     ylab = "Oportunidades de Gol",
     col = "blue")
abline(modLinealMultiple,col='red')

#Summary
summary(modLinealMultiple)

#Errores residuales
eMult <- residuals(modLinealMultiple)
hist(eMult)

