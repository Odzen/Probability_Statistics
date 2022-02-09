## Juan Sebastian Velasquez Acevedo - 1744936
##---------------------------------------------------------------------------
### Punto 4.A
##Funcion binomial
## P(x=3)

binom = function(x,n,p){
  prob=choose(n,x)*p^x*(1-p)^(n-x)
  return(prob)
}

binom(3,15,0.4)

## Grafica
intervaloX= 3:15
prob=binom(intervaloX,15,0.4)

plot(x,prob,type="h", col="red", lwd=3)

## Implemetacion con R,ya viene implementado
dbinom(3,15,0.4) ## p(X=x)

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

##---------------------------------------------------------------------------
### Punto 5
## Distribucion de Poisson
dpois(3,0.93)

