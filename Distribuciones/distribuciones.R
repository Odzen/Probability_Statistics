#Distribuciones de probabilidad

##Bernoulli es una binomial con n=1

##Random con size=1 que me indica que es binomial, y la probabilidad the flip coin
##Generar Bernoulli con  diferentes p
##100=muestras
##0.5= probabilidad
random=rbinom(100,1,0.5)
## return [1] 1 0 0 0 1 1 1 1 1 1, ejemplo sello=1, cara=0

##Media de random es igual a la probabilidad, o aprox
mean(random) ##p

##Varianza
var(random)## p(1-p)


##Tabla que me cuenta los numeros de 1 y 0 del random

tablaRandom=table(random)

##Plot
##Para que valor de p la varianza es máx?? Para P = 0.5

##Generar un vector de p
## Calcular v= p*(1-p)
##Graficar p vs v
p= seq(0,1,0.1)
v=p*(1-p)

##Plot wilmar----
plot(p,v,type='p', ## plot circles
     xlab= 'Probabilidad de exito', ylab='varianza')
segments(p,0,p,v) ##Lines

##Plot David-------
plot(p,v,type="h", col="royal blue", lwd=3)

barplot(tablaRandom)

##Entre mas numeros generes, más equilibrados se verán los gráficos 


####Distribucion Binomial

##Generacion de números aleatorios

##Ejercicio 1
muestra = rbinom(100,10,0.08)

table(muestra)

##Funcion propia
binomial = function(x,n,p){
  prob=choose(n,x)*p^x*(1-p)^(n-x)
  return(prob)
}

binomial(0,10,0.08)

x= 0:10
prob=binomial(x,10,0.08)

plot(x,prob,type="h", col="royal blue", lwd=3)

## Implemetacion con R,ya viene implementado
dbinom(0,10,0.08) ## p(X=x)

pbinom(2,10,0.08)## p(x<=2)

pbinom(2,10,0.08,lower.tail=F) ## p(X>=3)

10*0.8 ## E(X)
10*0.8*0.2 ## V(X)

##Distribucion Poison
m1 = rpois(100,2.5) ##Numero de lesiones mensuales, 100 meses observados
barplot(table(m1))

dpois(0,2.5) ## Funcion de R, cuando es 0

ppois(0,2.5,lower.tail = F) ## Cuando es >= 1


##Como la media y varianza depende de lambda, 
##a medida que aumenta la media, aumenta la varianza
m1=rpois(10000,2.5)
m2=rpois(10000,5)

x11() ##crear una ventana a parte, no es necesario
par(mfrow=c(2,1)) ##Matriz de graficos
barplot(table(m1),xlim= c(0,10))
barplot(table(m2),xlim= c(0,10))
