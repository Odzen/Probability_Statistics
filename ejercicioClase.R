coin<-c('head', 'tail', 'head')

set.seed(546378)# Semilla para reproducibilidad, ya que es una operacion aleatoria

#Crear vector vacio donde guardare posteriormente los resultados de cada lanzamiento
vectorFlips <- character() 

#Crear vector vacio donde guardare posteriormente los resultados de cada calculo de frecuencia
relativeFrequencyVector<- vector()


#Asigno acumulador para contar cada vez que salga head o tail, como queramos
acum<-0

#For donde recorro de 1 a 1000 y luego asigno valores a mi vectorFlips y mi relativeFrequencyVector
for(numberFlip in 1:1000){
  #Lanzo la moneda y crear la sample con el valor de uno, puesto que vamos a tomar solo un valor del vector coin
  flip  <- sample(coin,1)
  #Test 1
  print(flip)
  #Guardo el valor en vectorFlips
  vectorFlips<-append(vectorFlips,numberFlip)
  #Calculo la frecuencia, si me sale el valor que estoy buscando p=1 y si no p=0
  #Luego lo guardo en el vector relativeFrequencyVector
  if(flip=='head'){
    acum<-acum+1
  }
  #Test 2
  print(acum/numberFlip)
  relativeFrequencyVector<-append(relativeFrequencyVector, acum/numberFlip)
  
}

#Representar todo con un grafico de dispersion para ver el comportamiento
plot(x=vectorFlips,y=relativeFrequencyVector,xlab = "Number of flips", ylab = "Relative frequency")
abline(h=0.66,col="blue",lty=1)

#Test 3
#print(vectorFlips)
#print(relativeFrequencyVector)

#Vaciar vectores
#vectorFlips <- character() 
#relativeFrequencyVector<- vector()
#acumulator<-0

