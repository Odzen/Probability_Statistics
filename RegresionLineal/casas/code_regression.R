## REgresion lineal

data_hause = read.csv('data.csv')

plot(data_hause$sqft_living,data_hause$price)

### Modelo simple
mod_pp = lm(data_hause$price ~ data_hause$sqft_living)
mod_pp
plot(data_hause$sqft_living,data_hause$price)
abline(mod_pp,col='red')
summary(mod_pp)

## Modelo multiple agregando #de habitaciones

mod_pp2 = lm(data_hause$price ~ data_hause$sqft_living+data_hause$bedrooms)
mod_pp2
plot(data_hause$sqft_living,data_hause$price)
abline(mod_pp2,col='red')
summary(mod_pp2)

e <- residuals(mod_pp2)
hist(e)
