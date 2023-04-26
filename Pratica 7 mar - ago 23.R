
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
# P(3 <= X <= 12) # al menor lo subimos al inmediato superior, al mayor bajamos al imediato superior
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
m <- 10; n <- 10; k <- 15 # m es el número de fracasos, n es el número de éxito, y k es la muestra aleatoria
x <- 0:(k)
dhyper(x, m, n, k)


