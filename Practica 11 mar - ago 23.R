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
