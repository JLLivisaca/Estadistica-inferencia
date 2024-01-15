# Modelo de regresión Lineal

# Datos
library(readxl)
Modelo.1 <- read_excel("GitHub/estadistica-inferencia/Estadistica-inferencia/sept 23 - feb 24/Datos para regresión.xlsx", 
                                   sheet = "Modelo 1")
attach(Modelo.1)

# Modelo regresión lineal  (MLS)

modelo.regresion.1 <- lm(`Nota Prueba 3 (Y)` ~ `Nota Prueba 2 (X)`, 
                         data= Modelo.1)
summary(modelo.regresion.1)


Modelo.2 <- read_excel("GitHub/estadistica-inferencia/Estadistica-inferencia/sept 23 - feb 24/Datos para regresión.xlsx", 
                       sheet = "Modelo 2")
attach(Modelo.2)

# Modelo regresión lineal  (MLS)

modelo.regresion.2 <- lm(`Nota Prueba 4 (Y)` ~ `Nota Prueba 3 (X)`, 
                         data= Modelo.2)
summary(modelo.regresion.2)
