# cargar paquete readr
library(readxl)
library(here)

# 1.Importar Excel
data_folder<-"RegresionLineal"
file_name <- "juguetesLuciana.xlsx"
file_path <- here(data_folder, file_name)
my_file <-read_excel(file_path)

# 2. Definir X e Y
x = my_file$`Cantidad de cajas`
y = as.numeric(my_file$`Tiempo de entrega`)

# 3. Grafico de Dispersión
plot(x, y, 
     main = "Dispersión",
     xlab = "Cantidad de cajas",
     ylab = "Tiempo de entrega",
     col = "blue")

#4. Coeficiente de correlación Lineal : r
r=cor(x,y)

# Desviación Estandar X e Y
sd(x)
sd(y)

# Promedio X e Y
mean(x)
mean(y)

##β1=rSy/Sx
B1=(r*sd(y))/sd(x)
print(B1)

##β0= promedio(y)− β1* promedio(x)
B0= mean(y) - B1 * mean(x)
print(B0)

## Con las funciones de R preestablecidas
mod = lm(y~x)
summary(mod)
##Dibujo pendiente
abline(mod, col="blue")
##Sacar Sigma^2 del modelo
summary(mod)$sigma^2

##Coeficeinte de determinación
#r^2 → Proporción de variabilidad en los tiempos de entrega 
#explicada por mi modelo de regresión.
#En este caso 87%
summary(mod)$r.square
