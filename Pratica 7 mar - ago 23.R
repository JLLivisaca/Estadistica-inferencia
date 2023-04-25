
pt(0.4, 1, lower.tail = T)
pt(0.4, 1, lower.tail = F)
qt(4.9, 5, lower.tail = T)
dt(4.9, 5) # para sacar los valores puntuales ocupo la función dt. 

# Datos de la base GE 
library(readr)
datos <- read_csv("C:/Users/Juan Llivisaca/Downloads/GE.csv")
set.seed(1234)
muestra = sample(datos$`Adj Close`, 20,replace = F)

# Estadística descriptiva de la población 
summary(datos$`Adj Close`)
library(ggplot2)
ggplot(datos, aes(x= datos$Date, y= datos$`Adj Close` ))+
geom_line() 
a= mean(datos$`Adj Close`)
abline(v= a, col= "red")

# Histograma 
library(esquisse)
esquisser()

# fecha

datos$Date = as.Date(datos$Date, "%Y/%m/%D")


# Estadística descriptiva de la muestra

summary(muestra)


