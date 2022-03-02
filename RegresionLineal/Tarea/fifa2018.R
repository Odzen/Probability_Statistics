
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

summary(modLinealSimple)

plot(data$Ball.Possession..,data$Attempts,
     main = "Possession vs Attemps",
     xlab = "Posesión de Balón",
     ylab = "Oportunidades de gol",
     col = "blue")
abline(modLinealSimple,col='red')

#-------------------------------------------------------------------------
#Regresion Lineal Simple
#Para nuestro modelo lineal simple, tomamos:
#Posesion de balon (PassAccuracy) : x -> Variable independiente-regresora
#Oportunidades de gol (Ball.Possession) : y -> Variable dependiente-respuesta
# Modelo Lineal para la posesion de balon y la precision de los pases
modLinealSimple2 = lm(data$Ball.Possession.. ~ data$Passes)
modLinealSimple2

#Summary para insppeccionar
#Y: Variable respuesta o dependiente.
#X: Variable predictora o independiente. data$Ball.Possession..
#βo: Intercepto de la línea con el eje Y. (Intercept)
#β1: Pendiente de la línea de regresión.
#Coeficiente de determinacion : r.squared

summary(modLinealSimple2)

plot(data$Passes,data$Ball.Possession..)
abline(modLinealSimple2,col='red')
