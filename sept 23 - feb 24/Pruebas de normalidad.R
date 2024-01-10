library(readxl)
datos <- read_excel("GitHub/estadistica-inferencia/Estadistica-inferencia/sept 23 - feb 24/Dartos-Prueba-de-normalidad.xlsx")
# Prueba de normalidad 

# Ho: Mis datos siguen una distribución Normal estándar (media = 0, sd=1)
# Ha: Mis datos NO siguen una distribución Normal estándar (media = 0, sd=1)

# Alfa = 0.05 
# Estadístico: Kolmogorov, porque n >30

library(nortest)
lillie.test(datos$Data)

# Prueba de normalidad de shapiro - wilks

datos.menores <- sample(datos$Data, 20, replace = F)
shapiro.test(datos.menores)

# Prueba de chi cuadrado

chisq.test(datos.menores)

# Métodos gráficos 

library(car)
qqPlot(datos$Data)

qqnorm(datos.menores)
qqline(datos.menores)

library(corrr)
