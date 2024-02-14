#Problema 1
# a)
punif(200,120, 360)
#0.3333333
#b) 
(desv.es= sqrt((1/12) * (360-120)^2))
#69.28203
#c) 
a = punif(210,120, 360)
b = punif(300, 120, 360)
(proba.210.300 = b-a)
#0.375

#Problema 2

x = c( 5, 12, 14, 17, 23, 30, 40, 47,
       55, 67, 72, 81, 96, 112, 127)
y= c(4, 10, 13, 15, 15, 25, 27, 46,
     38, 46, 53, 70, 82, 99, 100)

g= data.frame(x,y)
attach(g)
modelo1= lm(y~x)
summary(modelo1)
# a) -1.12830 , 0.82697
# b) 0.9753
(sqrt(0.9753))
#0.9875728
# c) No sirve ya que no cumple con significancia b0
# d)
(y.50 = -1.12830 + 0.82697*50)
# 40.2202
# e) porque se tienen dos parámetros, b1 y b0 bloqueados
# Se recomendaría la prueba de shapiro wilks
qt(0.95, 5, lower.tail = T)

# Problema 3

# A)

# d=	1.666666667
# s.2= (di-d)^2 /(n-1)	1.066666667
# s=	1.032795559
# t=	3.952847075
# talfa=	2.015048373
# pvalor=	0.005409949
# Se rechaza Ho,
# Si es eficaz la prueba
# MEjor trabajador B, porque la media es de 7.167
# La varianza de la tarea es 1.066666667

# incosistencias
# d=	0.333333333
# s^2= (di-d)^2 /(n-1)=	2.666666667
# s=	1.632993162
#t	=0.5
#talfa=	2.015048373
#pvalor	=0.319149436
# No se rechaza Ho, la prueba no fue eficaz

# Problema 4

# a) 
dhyper(x=10, m=30, k=15, n=20)
# 0.2069539
# b)
(cumsum(dhyper(x=10:15, m=30, k=15, n=20)))
# 0.3798188
# c) 
# E(x) = 9
# Var(x) = 2.571, desv= 1.6036 

  
