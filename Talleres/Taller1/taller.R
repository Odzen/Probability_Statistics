# instalar paquete readr
install.packages("readr")

# cargar paquete readr
library(readr)

data <- read_csv('toy_dataset.csv')

# mirar datos
head(data)

library(tidyverse)

ggplot(data, aes(Gender, Number, color=factor(Illness))) + geom_col(fill=rgb(0.2,0.2,1,0.3),color="blue") 


ggplot(data, aes(Gender, Number, color=factor(Illness))) + geom_col()

ggplot(data, aes(Illness, Age)) + geom_boxplot()


