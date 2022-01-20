## Ejercicio en clase
library(tidyverse)

x <- 1:9

logs<- log10((x+1)/x)
sum(logs)


data <- data.frame(cbind(x,logs))

data %>% ggplot(aes(x=x,y=logs))+
  geom_bar(stat = 'identity',fill='lightblue')+
  geom_text(label = paste(round(logs*100,2),'%'))+
  labs(x='Primer digito',y="probabilidades",title="Ley de Benford")+
  theme_light()
