##### Prueba 4 #####

#### Problema 1 ####
# a) 
#Ho: No culpable
#Ha: Culpable

# Se comete un error de tipo I si rechazamos Ho cuando es cierto. 
# Es decir, no robó el celular pero fue declarado culpable y se le castiga por un crimen que en realidad no cometió

# El error de tipo II se comete si no rechazamos Ho cuando es falso. 
# En otras palabras, si el hombre sí robó pero fue declarado inocente y no fue castigado
# Si se acusa al segundo hombre se comete el Error tipo II.
# Si se no se acusa al primer hombre se comete el error tipo I.

#### Problema 2 ####

# a) 
# Ho: miu pesos = 25 
# Ha: miu pesos > 25
pesos = c(21, 23, 27, 19, 17, 18, 20, 15, 17,22)
t.test(pesos, mu= 25, alternative = "greater") # se prueba que los pesos sean mayores a 25 gramos
qt(0.05, df= length(pesos)-1, lower.tail = F) # el valor de T alfa
# t = -4.5944, df = 9, p-value = 0.9993, Por lo tanto No se Rechaza Ho
# Por lo tanto, no se tiene evidencia para decir que los pesos de los ratones sean mayores a 25 gramos

# b) 
# Ho: miu pesos = 25 
# Ha: miu pesos < 25
t.test(pesos, mu= 25, alternative = "less")
qt(0.05, df= length(pesos)-1, lower.tail = F)
# t = -4.5944, df = 9, p-value = 0.0006505

#### Problema 3 ####
# a) 
n.mirlos.experi <-  21
media.mirlos.experi <- 13.4
sd.mirlos.experi <-  2.05
n.mirlos.natural <-  9
media.mirlos.natural <- 9.7
sd.mirlos.natural <-  1.7

n.silverr.experi <-  15
media.silverr.experi <- 49.4
sd.silverr.experi <-  4.6
n.silverr.natural <-  17
media.silverr.natural <- 23.4
sd.silverr.natural <-  4

# Mirlos
# Ho: miu tiempo exper - miu tiempo natural = 0
# Ha: miu tiempo exper - miu tiempo natural > 0

T.mirlos <- 


