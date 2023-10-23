# Datos 
n1= 56 # muestra 1 independiente
n2 = 65 # muestra 2 independiente
media.1 = 1.24 # muestra 1
media.2 = 1.31 # muestra 2
varianza.muestra.1 = 0.056
varianza.muestra.2 = 0.054

# Encuentre un intervalo de confianza 90%

(do = media.1 - media.2)

# Z alfa medias para intervalo de confianza

Zalfamedios=abs(qnorm(0.05, 0,1,lower.tail = T))

# Intervalo superior

(ICS= do  + Zalfamedios *(sqrt((varianza.muestra.1/n1)+
                                 (varianza.muestra.2/n2))))
# Intervalo inferior
(ICS= do  - Zalfamedios *(sqrt((varianza.muestra.1/n1)+
                                 (varianza.muestra.2/n2))))
# Intervalo para la diferencia
# -0.1403792 
# -0.07
#  0.0003791568


# Prueba de hipótesis 
# Ho : miu 1 - miu 2 = 0 
# Ha : miu 1 - miu 2 diferente 0

# alfa = 0.05 
# Estadístico de la prueba, Z

# Cálculo estadístico 

(Zp = ((media.1 - media.2) - 0)/ (sqrt((varianza.muestra.1/n1)+
                                        (varianza.muestra.2/n2))))
# Decidir 
# Ha nos da la región de rechazo 
# Ho nos sirve para decidir

# Zalfa medios es debido a la región de rechazo de la Ha
(Zalfamedios= qnorm(0.025,0,1, lower.tail = T))
# P valor 
(pvalor = pnorm(Zp, 0,1,lower.tail = T))

# Varianzas

muestra1= c(55,68,70,66,91,78,81)
muestra2= c(73,65,74,80,76,63,82)
# Ho: var1 - var2 = 0 
# Ha: var1 > var2

# alfa = 0.05 

# F 

(Fprueba= var(muestra1)/ var(muestra2))

# F alfa, 

(falfa = qf(0.05, 6,6, lower.tail = F))


# Muestras pareadas

a = c(4,4,3,2,2,4) # muestra antes de la implementación del programa de seguridad industrial
d= c(3,5,3,7,6,6)# muestra después de la implementación del programa de seguridad industrial

# Prueba de hipótesis
# miu 1 = media de las horas perdidas por accidentes antes de implementar el programa de seguridad
# miu 2 = media de las horas perdidas por accidentes después de implementar el programa de seguridad
# 1) Ho: miu 1 - miu 2 = 0 
    #Ha: miu 1 - miu 2 > 0 

# 2) alfa = 0.05

# 3) Estadístico de prueba
  # Por el número de la muestra se elige el estadístico T*
# 4) Calculo el estadístico 
t.test(a,d,paired=T)
# EL p valor es de 0.03793
# comparado con el alfa, DEBO RECHAZAR Ho
# Conclusión 
  # Con base en los datos, se puede decir que: hay evidencia estadística para mencionar que
  # el programa de seguridad industrial es eficaz, con un nivel de significancia de 5%


a = c(4,4,3,2,2,4) # muestra antes de la implementación del programa de seguridad industrial
d= c(3,5,3,7,6,6)# muestra después de la implementación del programa de seguridad industrial

# Prueba de hipótesis
# miu 1 = media de la cantidad de sal en la funda A 
# miu 2 = media de la cantidad de sal en la funda B
# 1) Ho: miu 1 - miu 2 = 0 
#Ha: miu 1 - miu 2 < 0 

# 2) alfa = 0.05

# 3) Estadístico de prueba
# Por el número de la muestra se elige el estadístico T*
# 4) Calculo el estadístico 
t.test(a,d,paired=T)
# EL p valor es de 0.1103
# comparado con el alfa, NO RECHAZAR Ho
# Conclusión 
# Con base en los datos, se puede decir que: hay evidencia estadística para mencionar que
# el nivel de sal de las fundas son parecidos , con un nivel de significancia de 5%
# el nivel de sal de la funda a no es menor que el nivel de sal de la funda b



a = c(4,2,7,4,6,6,2,4,2,3,2,2,3,4,2,3,3,5,7,4,3,3,4,4) # muestra antes de la implementación del programa de seguridad industrial
length(a)
d= c(3,4,3,6,5,2,3,4,2,7,6,7,4,6,2,4,5,2,6,3,6,5,4,4)# muestra después de la implementación del programa de seguridad industrial
# Prueba de hipótesis
# miu 1 = media de la cantidad de sal en la funda A 
# miu 2 = media de la cantidad de sal en la funda B
# 1) Ho: miu 1 - miu 2 = 0 
#Ha: miu 1 - miu 2 < 0 

# 2) alfa = 0.05

# 3) Estadístico de prueba
# Por el número de la muestra se elige el estadístico T*
# 4) Calculo el estadístico 
t.test(a,d,paired=T)
# EL p valor es de 0.1103
# comparado con el alfa, NO RECHAZAR Ho
# Conclusión 
# Con base en los datos, se puede decir que: hay evidencia estadística para mencionar que
# el nivel de sal de las fundas son parecidos , con un nivel de significancia de 5%
# el nivel de sal de la funda a no es menor que el nivel de sal de la funda b