##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
## Creado por Juan Sebastian Velasquez Acevedo
## Codigo: 1744936-3743
## Parcial de Estadística - Ejercicio - Parte 3
## Universidad del Valle - Cali - Colombia -Periodo Nov-Mar
## Correo: juan.velasquez.acevedo@correounivalle.edu.co
##---------------------------------------------
## Last modification: Dic 22

coin<-c('head', 'tail')

#set.seed(546378)# Semilla para repoducibilidad, ya que es una operacion aleatoria
##---------------------------------------------
##VARIABLES Y CONSTANTES PARA EXPERIMENTO
##---------------------------------------------
acum<-0
oldFlip<-' '
n<-6

##---------------------------------------------
##FUNCION EXPERIMENTO QUE REPRESENTA EL PROBLEMA DADO DE PROBABILIDAD
## PROBABILIDAD DE QUE EN 6 LANZAMIENTOS DE UNA MONEDA, NO ME SALGAN 2 CARAS
## O 2 SELLOS SEGUIDOS
## Esta funcion simplemente ayuda a la simulacion, ya que devuelve el acum
## acum es la cantidad de veces que el experimento fue exitoso, lo podriamos llamar una Frecuencia absoluta
## void() -> return int = acum
##---------------------------------------------
experimento <- function() {
  for(numberFlip in 1:n){
    flip  <- sample(coin,1)
    if(!(strcmp(oldFlip, ' '))) #Solo para la 1era repeticion, para que el acum no me cuente
    {
      if(!(strcmp(flip, oldFlip))){ #Compara el lanzamiento actual con el anterior
        acum<-acum+1
      }
    }
    oldFlip<- flip #Asigna el valor del lanzamiento anterior para el proximo ciclo 
  }
  return(acum)
}


##---------------------------------------------
##CREO FOR EN DONDE HAGO N SIMULACIONES DEL EXPERIMENTO ANTERIOR
##---------------------------------------------

##Puede ser cualquier numero de repeticiones
numeroSimulaciones<-10000

#Crear vector vacio donde guardare los lanzamientos
numSim <- numeric() 
#Crear vector vacio donde guardare posteriormente los resultados de cada calculo de frecuencia
relativeFrequencyVector<- vector()

#For donde recorro de 1 a 1000 y luego asigno valores a mi vectorFlips y mi relativeFrequencyVector
#Por cada ciclo ejecuta el experimento y lo divide por el numero actual de simulacion
#La anterior operacion es lo mismo que la frecuencia relativa en ese momento de la simulacion

for(numberFlip in 1:numeroSimulaciones){
  relativeFrequencyVector<-append(relativeFrequencyVector, experimento()/numberFlip)
  numSim<-append(numSim,numberFlip)
}

##---------------------------------------------
##REPRESENTO TODO EN UN GRAFICO, CON EJE X= NUMERO DE SIMULACIONES
## Y EJE Y= LAS FRECUENCIAS RELATIVAS POR CADA SIMULACION
## Grafico demuestra que el valor se acerca a 0.03125 que es el valor dado por el enfoque clásico
##---------------------------------------------
#Representar todo con un grafico de dispersion para ver el comportamiento
plot(x=numSim,y=relativeFrequencyVector,xlab = "Number of simulations", ylab = "Relative frequency")
abline(h=0.03125,col="blue",lty=1)
