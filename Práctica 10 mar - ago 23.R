# Prueba De hipótesis

# Ho: miu = 2.3
# Ha: miu > 2.3 

# Estadístico de prueba T, porque n es menor que 50 datos 

n = 40 
miu = 2.3 
media.muestral = 2.4 
desv.estandar = 0.28

# Calculamos el estadístico de prueba 
(t.prueba = (media.muestral  - miu)/ (desv.estandar/ sqrt(n)))

# Comparar el t.prueba vs t.alfa
(t.alfa= qt(0.05, 39, lower.tail = F))
# t.prueba > t.alfa por la tanto Rechazo Ho
# En cambio si t.prueba < t.alfa por lo tanto 
# No rechazo Ho

# Decisión 
# Rechazo la Ho 

# Concluir, lo hacemos con Ha
 # Se tiene evidencia estadística para decir con un nivel de confianza
 # de 95% que la media es mayor a 2.3 

# miu < 2.7

# Prueba De hipótesis

# Ho: miu = 2.7
# Ha: miu < 2.7 


n = 40 
miu = 2.7 
media.muestral = 2.4 
desv.estandar = 0.28

# Calculamos el estadístico de prueba 
(t.prueba = (media.muestral  - miu)/ (desv.estandar/ sqrt(n)))

# Comparar el t.prueba vs t.alfa
(t.alfa= qt(0.001, 35, lower.tail = T)) # Considerar que se cambio la Ha


# Decisión 
 # t.prueba es menor a t.alfa, por lo tanto RHo 

# Conclusión
# Ha: 
# Se tiene evidencia estadística, con un 
# nivel de error de 0.05, para decir que la media < 2.7


# Problema

# Un nuevo diseño del sistema de frenos de un cierto tipo de
# carro ha sido propuesto. Para el sistema actual, se sabe que la
# distancia de frenado promedio verdadera a 40 mph en condiciones
# específicas es de 120 pies. Se propone que el nuevo diseño sea 
# implementado sólo si los datos muestrales indican
# fuertemente una reducción de la distancia de frenado promedio verdadera
# del nuevo diseño.
  
#  Defina el parámetro de interés y formule las hipótesis pertinentes
# Ho: la media de la distancia del nuevo diseño de frenado es 120 pies
# Ha: la media de la distancia del nuevo diseño de frenado MENOR 120 pies
# Ho: miu = 120 
# Ha: miu < 120 

# Suponga que la distancia de frenado del nuevo sistema está normalmente
# distribuido con sigma=10. Sea media muestral de la distancia de frenado
# promedio de una muestra de 36 observaciones.
# ¿Cuáles de las siguientes regiones de rechazo es apropiada:
# R1  {x: x>=  124.80}, R2  {x: x<= 115.20}, 
# R3 = {x: o x >= 125.13 o x <= 114.87}?

# Se selecciona la región de rechazo, x <= 115.2 por la Ha

# alfa = 0.05

# El estadístico de prueba es t. 

n = 36 
miu = 115
media.muestral = 115.2  
desv.estandar = 10

# Calculamos el estadístico de prueba 
(t.prueba = (media.muestral  - miu)/ (desv.estandar/ sqrt(n)))

# Comparar el t.prueba vs t.alfa
(t.alfa= qt(0.05, n-1, lower.tail = T)) # Considerar que se cambio la Ha
# Decisión 
 # No rechazo la Ho 

# Conclusión 
# Se tiene evidencia estadística, con un nivel de confianza de 95%, para decir
# que la distancia promedio de frenado del nuevo diseño no es menor que 115 pies


 