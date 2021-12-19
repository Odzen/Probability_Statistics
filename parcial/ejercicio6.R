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


#DECLARACION de Variables para posterior uso en graficos
#-----------------------------------------------------------------
#Numero de victimas totales
numeroDeVicitmasTotales<-sum(dataFormat$CANTIDAD, na.rm = TRUE)
numeroDeVicitmasTotales

#Frecuencias---------------------------------------
##Data frame en donde pongo la frecuencia absoluta por genero 
frecuenciasAbsolutasGenero<-aggregate(dataFormat$GENERO,by=list(dataFormat$GENERO),FUN=length)
colnames(frecuenciasAbsolutasGenero) <- c("GENERO", "CANTIDAD")
frecuenciasAbsolutasGenero

# Transpongo todas las columnas menos la primera, por facilidad 
frecAbsolutGeneroTransp <- data.frame(t(frecuenciasAbsolutasGenero[-1]))
colnames(frecAbsolutGeneroTransp) <- frecuenciasAbsolutasGenero[, 1]
frecAbsolutGeneroTransp
#Numero de casos donde las victimas son mujeres
frecAbsolutGeneroTransp$FEMENINO

#Numero de casos donde las victimas son hombres
frecAbsolutGeneroTransp$MASCULINO

#Numero de casos donde el genero de las victimas no es reportado 
frecAbsolutGeneroTransp$`NO REPORTA`

#DATAFRAME PARA EL GRAFICO 1 DE SERIE DE TIEMPO
#--------------------------------------------
##Este dataframe me saca el numero total de victimas por año
dataGraphic1 <- dataFormat %>% 
  group_by(YEAR,GENERO) %>% 
  summarise(CANTIDAD = sum(CANTIDAD))
#aggregate(dataFormat$CANTIDAD,by=list(dataFormat$CANTIDAD),FUN=sum)



##Numero total de victimas
sum(dataGraphic1$CANTIDAD)


##Frecuencias por genero
##--------------------------------------
#Frecuencias relativas (proporciones)
round((prop.table(tablaFAbsoluta)*100),2)

#Frecuencia Absoluta
tablaFAbsoluta<-table(data$GENERO)
tablaFAbsoluta

##Operaciones con generos
##---------------------------------------------

#fILTRO TABLA DE VICITMAS MASCULINAS
#dataMasculino<-data[data$GENERO == "MASCULINO",]

#fILTRO TABLA DE VICITMAS FEMENINAS
#dataFemenino<-data[data$GENERO == "FEMENINO",]




##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
###GRAFICAS
##---------------------------------------------------------------------------
##Gráfico 1 - Serie de Tiempo 


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
#dataGraphic5<-aggregate(dataFormatMujer$MUNICIPIO,by=list(dataFormatMujer$MUNICIPIO),FUN=length)
#colnames(dataGraphic5) <- c("MUNICIPIO", "GENERO", "VICTIMAS")


##Elimino casos no reportados, solo estudiamos casos donde reportan el arma y el genero
dataGraphic5<-dataGraphic5[dataGraphic5$ARMAS.MEDIOS!="NO REPORTA",]
dataGraphic5<-dataGraphic5[dataGraphic5$GENERO!="NO REPORTA",]
dataGraphic5<-dataGraphic5[dataGraphic5$ARMAS.MEDIOS!="NO REPORTADO",]
#Ordenar por número de victimas
#dataGraphic5 <- dataGraphic5[order(-dataGraphic5$CANTIDAD), ]

#Saco primeras 5 ciudades con head
#dataGraphic5<-head(dataGraphic5)

ggplot(dataGraphic5, aes(x=GENERO, y=CANTIDAD)) + geom_bar(stat="identity", )  + coord_flip() + facet_wrap(~ ARMAS.MEDIOS)




