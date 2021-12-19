
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
## Creado por Juan Sebastian Velasquez Acevedo
## Codigo: 1744936-3743
## Parcial de Estadística - Ejercicio 2
## Universidad del Valle - Cali - Colombia -Periodo Nov-Mar
## Correo: juan.velasquez.acevedo@correounivalle.edu.co
##---------------------------------------------
## Last modification: Dic 19



# instalar paquete readr
install.packages("readr")
install.packages('jtools')
install.packages('tidyverse')

# cargar paquete readr
library("readr")

# cargar paquete tidyverse
library(tidyverse)

data <- read_csv('Reporte_Delito_Violencia_Intrafamiliar_Polic_a_Nacional.csv')

# mirar datos
head(data)


##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##Formatear data solo por año y luego agrego nueva columna de solo año
##Usando mutate, format y group_by
##---------------------------------------------
fecha<-as.Date(data$`FECHA HECHO`,format = "%d/%m/%Y",na.rm=TRUE)
dataFormat<- data%>% 
  mutate(YEAR=format(fecha,"%Y"))%>%
  group_by(YEAR)

#Borrando valores NA
dataFormat<-na.omit(dataFormat)

#Transformo los años a numeros por facilidad luego al graficar la serie de tiempo
#
dataFormat<-transform(dataFormat,YEAR = as.numeric(YEAR))


##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
###GRAFICAS
##---------------------------------------------------------------------------
##Gráfico 1 - Serie de Tiempo #--------------------------------------------
##Este dataframe me saca el numero total de victimas por año
dataGraphic1 <- dataFormat %>% 
  group_by(YEAR,GENERO) %>% 
  summarise(CANTIDAD = sum(CANTIDAD))

##Numero total de victimas
sum(dataGraphic1$CANTIDAD)

#Grafico
dataGraphic1 %>% ggplot(aes(YEAR,CANTIDAD, color=factor(GENERO)))+geom_line()+
  theme_classic() +
  labs(
    x = "Años",
    y = "Victimas",
    col="Genero",
    title = paste(
      "Número de victimas en el transcurso de los años"
    )
  )

##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##Gráfico 2 - Grafico de barras

##Solo el Valle

dataFormatValle<-dataFormat[dataFormat$DEPARTAMENTO=="VALLE",]

##Con que arma se violentó en el Valle y cuantas victimas por arma
dataGraphic2 <- dataFormatValle %>% 
  group_by(ARMAS.MEDIOS,DEPARTAMENTO) %>% 
  summarise(CANTIDAD = sum(CANTIDAD))

#Total de victimas en el valle
totalDeVictimasValle<-sum(dataGraphic2$CANTIDAD)

#Agrego columna con porcentaje de victimas en el valle
dataGraphic2 <- dataGraphic2 %>% 
  mutate(PORCENTAJE = (100*(CANTIDAD/totalDeVictimasValle)))

#Grafico
ggplot(data=dataGraphic2, aes(x=DEPARTAMENTO, y=PORCENTAJE, fill=ARMAS.MEDIOS)) + 
  geom_bar(stat="identity", position="dodge") +
  theme_classic() +
  labs(
    x = "Departamento",
    y = "Porcentaje de victimas",
    col="Armas",
    title = paste(
      "Porcentaje de victimas afectadas por una determinada arma en el Valle"
    )
  )

##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##Gráfico 3  - Cajas
##Solo en el 2021

##Formateo tabla para mostrar los últimos 3 años, 2019, 2020 y 2021
dataFormat2021<-dataFormat[dataFormat$YEAR>2018,]


##Defino DataFrame con el que trabajaré
dataGraphic3 <- dataFormat2021 %>% 
  group_by(GRUPO.ETARIO,YEAR) %>% 
  summarise(CANTIDAD = sum(CANTIDAD))


## Grafico de Cajas
dataGraphic3 %>% ggplot(aes(x=GRUPO.ETARIO,y=CANTIDAD)) + geom_boxplot()+
  theme_classic() +
  labs(
    x = "Grupo Etario",
    y = "Cantidad de Victimas",
    title = paste(
      "Cantidad de victimas por grupo Etario en los últimos 3 años"
    )
  )


##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##Gráfico 4  - Barras Apiladas
##Defino DataFrame con el que trabajaré
dataGraphic4 <- dataFormat %>% 
  group_by(GRUPO.ETARIO,GENERO) %>% 
  summarise(CANTIDAD = sum(CANTIDAD))

##Elimino casos no repportados ya que no alteras demasiado los resultados y pueden ser despreciables para el análisis
dataGraphic4<-dataGraphic4[dataGraphic4$GENERO!="NO REPORTA",]


#Total de victimas 
totalDeVictimas<-sum(dataGraphic4$CANTIDAD)
#Agrego columna con porcentaje de victimas 
dataGraphic4 <- dataGraphic4 %>% 
  mutate(PORCENTAJE = (100*(CANTIDAD/totalDeVictimas)))

#Grafico
dataGraphic4 %>% 
  count(GENERO,GRUPO.ETARIO) %>% 
  ggplot(aes(x=dataGraphic4$GRUPO.ETARIO,y=dataGraphic4$PORCENTAJE,fill=GENERO))+
  geom_bar(stat = "identity")+
  theme_light() +
  labs(
    x = "Grupo Etario",
    y = "Porcentaje de Victimas",
    title = paste(
      "Porcentaje de victimas por masculinas y femeninas, según su grupo etario"
    )
  )


##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##Gráfico 5  - Barras Cruzadas

##Formateo tabla para mostrar solo los menores
dataFormatMenor<-dataFormat[dataFormat$GRUPO.ETARIO=="MENORES",]

##Defino DataFrame con el que trabajaré
dataGraphic5 <- dataFormatMenor %>% 
  group_by(ARMAS.MEDIOS,GENERO) %>% 
  summarise(CANTIDAD = sum(CANTIDAD))

##Elimino casos no reportados, solo estudiamos casos donde reportan el arma y el genero
dataGraphic5<-dataGraphic5[dataGraphic5$ARMAS.MEDIOS!="NO REPORTA",]
dataGraphic5<-dataGraphic5[dataGraphic5$GENERO!="NO REPORTA",]
dataGraphic5<-dataGraphic5[dataGraphic5$ARMAS.MEDIOS!="NO REPORTADO",]

#Grafico
ggplot(dataGraphic5, aes(x=GENERO, y=CANTIDAD)) + geom_bar(stat="identity", )  + coord_flip() + facet_wrap(~ ARMAS.MEDIOS)




