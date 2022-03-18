# Punto 2 - Parcial 2 - Probabilidad y Estadística 
# Universidad del Valle
# Estudiante: Juan Sebastian Velasquez Acevedo

# Un abogado viaja todos los días de su casa en los suburbios a su oficina en el centro de la
# ciudad. El tiempo promedio para un viaje sólo de ida es de 24 minutos, con una desviación
# estándar de 3.8 minutos. Si se supone que la distribución de los tiempos de viaje está
# distribuida normalmente.

##-------------------------------------------------------------

# Grafica de la distribucion normal
# Rejilla de valores para el eje X
x <- seq(10, 35, 1)
plot(x, dnorm(x, mean = 24, sd = 3.8), type = "o",
     ylim = c(0, 0.125), lwd = 2,lty=3, col = "blue",
     main = "Distribucion normal X∼N(24,3.8)",
     xlab = "Valores para X",
     ylab = "f(x)")

##-------------------------------------------------------------
# a) ¿Cuál es la probabilidad de que un viaje tome al menos 1/2 hora?

# 1/2 hora = 30 minutos
# Nos piden P(X > x) = P(X>30)
# media (μ) = 24
# Desviacion estandar (σ)= 3.8
# Funcion en R
# pnorm: Distribución normal (Función de distribución acumulada)
# lower.tail = FALSE porque estoy buscando valores mayores a 30 minutos
distr_acum1 = pnorm(30,mean = 24, sd = 3.8,lower.tail=FALSE)
distr_acum1
# Pasandolo a porcentaje
prob_procentaje1 = distr_acum1*100
prob_procentaje1

## RESPUESTA: 
## La probabilidad de que un viaje tome al menos 1/2 hora es de 0.05717406
## hay una posibilidad del 5.717406 %

##-------------------------------------------------------------
# b) Si la oficina abre a las 9:00 a.m. y él sale diario de su 
# casa a las 8:45 a.m., ¿qué porcentaje de las veces llegará tarde al trabajo?.

# La idea es calcular la probabilidad de que el tiempo sea mayor a 
# 15 minutos. Puesto que 9:00 am - 8:45 am = 15 minutos

# Nos piden P(X > x) = P(X > 15) = 1 - P(X < 15)
 
# Se calcula primero la Función de distribución acumulada
distr_acum2 = 1 - pnorm (15,24,3.8)
distr_acum2
# Tambien se puede hacer distr_acum = pnorm(15,24,3.8,FALSE)

# Pasandolo a porcentaje
prob_procentaje2 = distr_acum2*100
prob_procentaje2

## RESPUESTA: 
## El porcentaje de las veces que llegará tarde al trabajo sera de 99.10679%
##-------------------------------------------------------------

## c) Si sale de su casa a las 8:35 a.m. y el café se sirve en la oficina de 8:50 a.m. a 9:00 a.m.,
## ¿cuál es la probabilidad de que se pierda el café?

# La idea es calcular la probabilidad de que el tiempo sea mayor a 
# 25 minutos. Puesto que 9:00 am - 8:35 am = 25 minutos

# Nos piden P(X > x) = P(X > 25) = 1 - P(X < 25)

# Se calcula primero la Función de distribución acumulada
distr_acum3 = 1 - pnorm (25,24,3.8)
distr_acum3
# Tambien se puede hacer distr_acum = pnorm(25,24,3.8,FALSE)

# Pasandolo a porcentaje
prob_procentaje3 = distr_acum3*100
prob_procentaje3

## RESPUESTA: 
## La probabilidad de que se pierda el cafe es de 0.3962144 con una
## posibilidad de 39.62144%

##-------------------------------------------------------------
# d) Calcule la duración mayor en la que se encuentra el 15% de los viajes mas lentos. 

# Para esto se puede usar la funcion de R qnorm dejando el lower.tail=TRUE (default)
# qnorm es la funcion quantil de la distribucion normal

# Puesto que se quiere encontrar P(X<=x), ya que el procentaje es 
# 15% entonces queremos encontrar el quantil 0.15
qnorm(0.15, 24, 3.8)

## RESPUESTA: 
#La duracion mayor en la que se encuentran el 15% de los viajes mas lentos es 20.06155

##-------------------------------------------------------------
# e) Calcule la probabilidad de que 2 de los siguientes 3 viajes tomen al menos 1/2 hora.
# En el documento

