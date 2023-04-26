
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


################ Práctica ejercicios distribuciones #######

# Ejercicio 52

n = 25
p = 0.3

# Literal a 
(Valor.medio = n*p)
(desv.esta= sqrt(n*p*(1-p)))

# Literal b 
(valor = Valor.medio +2*desv.esta)
(valor = Valor.medio -2* desv.esta)
# P(3 <= X <= 12) # valores cercanos
(Prob.2desv = pbinom(c(3,12), 25, 0.3))
(Prob.2desv[2]- Prob.2desv[1])  

# Literal c
x= 0:25
# Distribución para los libros nuevo, p cambia ya que cambia la muetra ahora es 
# 0.5, que es de 15 libros nuevos / 30 libros en total 
(ditribu.nuevos = pbinom(x, 25, 0.5))
# Distribución para los libros usados
(ditribu.nuevos = pbinom(x, 25, 0.5))
#  para conoce ¿Con qué valores de X obtendrán las 15 lo que desean?, vamos a 
# calcular la probabilidad de qué  al menos 1 persona obtenga el tipo de libro que desea es:
# P(X >= 1) = 1 - P(X = 0) = 1 - pbinom(0, 15,0.5)
(Prob.almenos1 = 1 - pbinom(0,25,0.5))

# Literal d
# como ahora hay 50 libros de cada tipo, se puede calcular p como: p=50/100
# La función del valor esperado es la que determina el valor esperado del ingreso total
# h(x)= ingreso de ventas
# X = variable que represent venta libros nuevos
# Y = variable que representa venta de libros usados
# Y = (25 - X) son las personas que no compran libros nuevos
X= 0:25
Y= (25-X)
(hx = 100 * X + Y * 70 ) # función de los ingresos

# Ahora con la función se puede aplicar las reglas del valor esperado
# E[h(X)] = E[X * 100 + (25 - X) * 70]
# E[h(X)] = E[X] * 100 + E[(25 - X) * 70]
# E[h(X)] = 100 * E[X] + 70 * E[25 - X]
# E[h(X)] = 100 * np + 70 * nq
n= 25 
p = 0.5
q = 0.5
(E.hx = 100 * n*p + 70 *q)


# Ejercicio 71

# Literal a # granito
roca.basaltica = 10 
roca.granito = 10
n = 15

# Distribución hypergeométrica
m <- 10; n <- 10; k <- 15 # m es el número de éxitos, n es el número de fracasos, y k es la muestra aleatoria
x <- 0:(k)
dhyper(x, m, n, k) # función de distribución 

# Literal b algunos sea, como los dos tienen 10 como muestra, da lo mismo 

# Todos los especímenes es P (X<=10) 
x=0:10
(P15 = cumsum(dhyper(x, m, n, k))) # es 0.9837

# Valor medio 
# Ex = k * m/ n+m
(Ex = k*(m/(n+m)))

#Varianza 
# Var(x) = Ex * (1- m/(n+m))*(m+ n- k)/(m+n - 1)
(Var.X = Ex * (1- m/(n+m))*((m+ n- k)/(m+n - 1)))
desv.estan = sqrt(Var.X)
(Prob.1desv = Ex + desv.estan)
(Prob1desv = Ex - desv.estan)
 # P ( 7 <= X <= 9)
m <- 10; n <- 10; k <- 15 # m es el número de éxitos, n es el número de fracasos (resto del total), y k es la muestra aleatoria
x <- 0:(k)

(h= dhyper(x= 7:9, m, n, k))
h[1]-h[3] # Probabilidad de los valores

m <- 5; n <- 20; k <- 10 # m es el número de éxitos, n es el número de fracasos , y k es la muestra aleatoria
x <- 0:(k)

(h= sum(dhyper(x= 0:2, m, n, k)))


# Ejercicio 110 
# P(por lo menos una langosta) = 1 - P(ninguna langosta)
# X = número de langostas
# P(X>= 1) = 1- P(X<1)
# fdp de poisson = e^ - lamb  * lamb ^x / factorial(x)
# lambda = alfa *t 
# t es una variable que cambia con el tiempo, si lo colocamos con área sería
# lambda = alfa * unidad de área
# Area = pi * radio ^2, usamos Radio por la pregunta: ¿Qué tan grande deberá ser el
# radio R de una región de muestreo circular para que la probabilidad de 
# hallar por lo menos una en la región sea igual a 0.99?
alfa = 2
radio = 0:10
(Area = pi * radio^2)
(lamda = alfa * Area) # son los valores de lamda a diferentes valores del radio



     