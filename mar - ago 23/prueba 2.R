# Prueba 2

n = 20
p = 0.3

# Literal a 
(Valor.medio = n*p)
(desv.esta= sqrt(n*p*(1-p)))

# Literal b 

#Ninguno nuevo 
dbinom(x= 0:4, 4, 0.3) # cuando la variable es 0, no se tiene ningún nuevo
dbinom(0, 4, 0.3)

#Literal c

#c.	¿Cuál es la probabilidad de que los que se venda, sean exactamente 5 libros?

dbinom(5,20,0.3)# Probabilidad de exactamente cinco personas compren un libro nuevo
dbinom(5,20,0.7)# Probabilidad de exactamente cinco personas compren un libro usado
(prob.comprar.5=dbinom(5,20,0.3)+dbinom(5,20,0.7))

# Literal d
#d.	¿Cuál es la probabilidad que al quinto intento de sacar 2 libros sean todos estos usados?
dnbinom(3,2,0.7)

# Literal e

# La función del valor esperado es la que determina el valor esperado del ingreso total
# h(x)= ingreso de ventas
# X = variable que represent venta libros nuevos
# Y = variable que representa venta de libros usados
# Y = (20 - X) son las personas que no compran libros nuevos
X= 0:20
Y= (20-X)
(hx = 130 * X + Y * 90 ) # función de los ingresos

# Ahora con la función se puede aplicar las reglas del valor esperado
# E[h(X)] = E[X * 130 + (20 - X) * 90]
# E[h(X)] = E[X] * 130 + E[(20 - X) * 90]
# E[h(X)] = 130 * E[X] + 90 * E[20 - X]
# E[h(X)] = 130 * np + 90 * nq
n= 10 
p = 30/50
q = 20/50
(E.hx = 130 * n*p + 90 *n*q)
# USados
n= 10 
p = 20/50
q = 30/50
(E.hx1 = 90 * n*p + 130 *n*q)


# Ejercicio 2
#2.	El número de personas que llegan para tratamiento a una sala de urgencias puede ser modelado mediante una razón de cinco personas por hora. 
# a.	¿Cuál es la probabilidad de que ocurran exactamente cuatro arribos durante una hora particular? 
lamda=5 
(dpois(4,5))
# Si se consideraba una exponencial
(pexp(4,1/5))

# b.	¿Cuál es la probabilidad de que por lo menos cuatro personas arriben durante una hora particular? 
lamda=5 
1- sum(dpois(x=0:3,5))

lamda=5 
1- sum(dpois(x=0:4,5))

#c.	¿Cuántas personas espera que arriben durante un periodo de 45 min?
(lamda1=5*0.75)
# d.	Hay una nueva propuesta de que la sala de urgencia se puede ver como un área limitada por dos salas de 4 m de ancho por 6m de longitud, si se da un parámetro α = 8 pacientes por área, ¿cuál es el número esperado de pacientes en la sala de urgencias?
alfa = 8
ancho = 4
largo = 6
(Area = ancho * largo)
(lamda = alfa * Area) 