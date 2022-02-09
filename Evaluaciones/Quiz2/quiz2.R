## Juan Sebastian Velasquez Acevedo - 1744936
##---------------------------------------------------------------------------
### Punto 4.A
## Funcion binomial
## P(x=3)

binom = function(x,n,p){
  prob=choose(n,x)*p^x*(1-p)^(n-x)
  return(prob)
}

binom(3,15,0.4)

## Implemetacion con funcion predefinida de R
dbinom(3,15,0.4) ## p(X=x)

## Grafica
intervalo4a= 2:15
prob=binom(intervalo4a,15,0.4)

plot(intervalo4a,prob,type="h", col="red", lwd=3)


##---------------------------------------------------------------------------
### Punto 4.B
##La probabilidad de que 12 viajeros no tengan computadora portátil es igual
## a que 3 viajeros tengan una computadora portatil. Por lo tanto, es igual al 
## punto anterior . P(x=3)

##---------------------------------------------------------------------------
### Punto 4.c
## Usando la función Binomial

## P(x>=3) = 1 - P(x<3)
answer<- 1 - (binom(0,15,0.4)+ binom(1,15,0.4) + binom(2,15,0.4))
answer

## Otra forma usando funciones de R
pbinom(2,15,0.4,lower.tail=F) ## P(x>=3)

##Grafica
intervalo4c= 0:2
prob4c=binom(intervalo4c,15,0.4)

plot(intervalo4c,prob4c,type="h", col="red", lwd=3)


##---------------------------------------------------------------------------
### Punto 5

## Funcion poisson
poisson = function(x,lambda){
  prob=(exp(-lambda)*((lambda)^(x)))/factorial(x)
  return(prob)
}

poisson(3,0.93)

## Distribucion de Poisson por default en R
dpois(3,0.93)


## Grafica
intervaloXPoisson= 0:5
prob=poisson(intervaloXPoisson,0.93)

plot(intervaloXPoisson,prob,type="h", col="red", lwd=3)

