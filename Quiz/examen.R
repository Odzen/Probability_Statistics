# instalar paquete readr
install.packages("readr")

# cargar paquete readr
library("readr")

data <- read_csv('fifa_eda.csv')

# mirar datos
head(data)


##A. Histograma de ela edad de los jugadores

hist(data$Age, 
     main = "Edad de Jugadores",
     xlab = "Edad",
     ylab = "Cantidad",
     col = "blue")

## B. Frecuencia entre jugadores derechos o zurdos.

library("tidyverse")

dataset <- read.csv("fifa_eda.csv")

dataBase_Fifa <- dataset

ggplot(dataBase_Fifa,aes(Preferred.Foot,fill=Preferred.Foot))+geom_bar()


## C. País tiene más jugadores en el circuito profesional

data %>% count(Nationality) %>% 
  top_n(10,n) %>% 
  ggplot(aes(x=reorder(Nationality,-n),y=n))+
  geom_bar(stat = "identity",fill="lightblue")+
  geom_text(aes(label=n))+ labs(x="Nacionalidad",y="Cantidad")
  theme_light()

## D.Quienes tienen más potencial, ¿los zurdos o los diestros?
data %>% 
  ggplot(aes(x=`Preferred Foot`,y=Potential,fill=`Preferred Foot`))+geom_bar(stat = "identity",)
