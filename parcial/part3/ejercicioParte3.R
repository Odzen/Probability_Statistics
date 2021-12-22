install.packages('pracma')
library('pracma')

coin<-c('head', 'tail')

#set.seed(546378)# Semilla para repoducibilidad, ya que es una operacion aleatoria

#Crear vector vacio donde guardare posteriormente los resultados de cada lanzamiento
valuesArray <- character() 
#Crear vector vacio donde guardare los lanzamientos
flipsArray <- numeric() 
#Crear vector vacio donde guardare posteriormente los resultados de cada calculo de frecuencia
relativeFrequencyVector<- vector()

acum<-0
oldFlip<-' '
relativeFreq<-0
n<-6

experimento <- function() {
  for(numberFlip in 1:n){
    flip  <- sample(coin,1)
    if(!(strcmp(oldFlip, ' ')))
    {
      if(!(strcmp(flip, oldFlip))){
        acum<-acum+1
      }
    }
    oldFlip<- flip
  }
  relativeFreq<-acum/n
  return(acum)
}
experimento()


#For donde recorro de 1 a 1000 y luego asigno valores a mi vectorFlips y mi relativeFrequencyVector
for(numberFlip in 1:10000){
  relativeFrequencyVector<-append(relativeFrequencyVector, experimento()/numberFlip)
  flipsArray<-append(flipsArray,numberFlip)
}

#Representar todo con un grafico de dispersion para ver el comportamiento
plot(x=flipsArray,y=relativeFrequencyVector,xlab = "Number of flips", ylab = "Relative frequency")
abline(h=0.03125,col="blue",lty=1)
