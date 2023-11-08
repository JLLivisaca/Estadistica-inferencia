
dbinomNegativa.propia <- function(x,
                                  k,
                                  p){
  choose(x-1,k-1)*(p^k)*((1-p)^(x-k))

  }

dbinomNegativa.propia(5,3,0.3)

# P (X=2) cuando X tiende a una binomial (15,0.3), esto es f(x)
# X = 2
# n = 15, 
# p =0.3
dbinom(2,15,0.3)

# P(2< X < 6) X tiende a una binomial (15,0.3)
# P(2< X < 6) = P( 3 <= x <= 5 ) 
# dbinom(x, n, p) = se usa cuando debo sacar la P(X=x)

#pbinom(q= valor hasta donde acumula la probabilidad, 
# n, p), esto es F(x)

# P(2< X < 6) = P( 3 <= x <= 5 ) = F(5) - F(2)

pbinom(5,15,0.3) - pbinom(2,15,0.3)

# P(x<=3) X tiende a una binomial (10,0.2)
pbinom(3,10,0.2)

# P(2< X <= 6) = P( 3 <= x <= 6 ) = F(6) - F(2)
pbinom(6,15,0.3) - pbinom(2,15,0.3)
x= c(2,6)
h = pbinom(x,15,0.2)
h[2] - h[1]

# Ejercicio 54 del libro de devore, pp 114
# Un tipo particular de raqueta de tenis viene en tamaño me-
# diano y en tamaño extragrande. El 60% de todos los clientes
# en una tienda desean la versión extragrande. 
# a. Entre diez clientes seleccionados al azar que desean este
# tipo de raqueta, ¿cuál es la probabilidad de que por lo
# menos seis deseen la versión extragrande? 
  #b. Entre diez clientes seleccionados al azar, ¿cuál es la proba-
  #bilidad de que el número que desea la versión extragrande
#  esté dentro de una desviación estándar del valor medio? 
  #c. La tienda dispone actualmente de siete raquetas de cada
#   versión. ¿Cuál es la probabilidad de que los siguientes
#   diez clientes que desean esta raqueta puedan obtener la
#   versión que desean de las existencias actuales? 

#Literal a

# P(X>=6) = 1- P(X<6) = 1 - P(X<=5)
1-pbinom(5,10,0.6)
#Literal b
# Valor medio 
(Ex = 10*0.6)
# Varianza
(var.x= 10*0.6*0.4)
# desv. estándar
(desv.x = sqrt(var.x))
# P(4.45 <= X <= 7.55)
# P(5 <= X <= 7) = F(7) - F(4)
pbinom(7,10,0.6) - pbinom(4,10,0.6)

# Literal c 

pbinom(7,10,0.5)

# Distribución binomial negativa
# Ejercicicio 3.38, libro Devore, pp: 119

# Un pediatra desea reclutar cinco parejas, cada una de las cuales espera a su primer hijo, 
#para participar en un nuevo régimen de alumbramiento natural. Sea p  P(una pareja selec-
#cionada al azar está de acuerdo en participar). Si p  0.2, ¿cuál es la probabilidad de que 15
# parejas tengan que ser entrevistadas antes de encontrar cinco que estén de acuerdo en parti-
  #cipar? Es decir, E  {está de acuerdo en participar}, ¿cuál es la probabilidad de que ocurran
# 10 fallas antes del quinto éxito?

dnbinom(x=10, size=5, prob=0.2)

# Distribución de Poisson

dpois(1,5) # esto representa el f(x)
ppois(2,5) # esto representa el F(x)


m <- 4; n <- 5; k <- 3 # m es el número de éxitos, n es el número de fracasos , y k es la muestra aleatoria tomada
x <- 0:(4) # es el valor de la probabilidad que me piden dentro del problema
dhyper(x, m, n, k) 
# probabilidad no más de una defectuosa
phyper(0, m, n, k)
# al menos una defectuosa 

(g= 1 - phyper(0, m, n, k))

# Ejercicio hipergeométrica

#Un tipo de cámara digital viene en una versión de 3 megapi-
  #xeles o una versión de 4 megapixeles. Una tienda de cáma-
  #ras recibió un envío de 15 de estas cámaras, de las cuales 6
#tienen una resolución de 3 megapixeles. Suponga que se 
#seleccionan al azar 5 de estas cámaras para guardarlas detrás
#del mostrador; las otras 10 se colocan en una bodega. Sea 
#X  el número de cámaras de 3 megapixeles entre las 5 se-
  #leccionadas para guardarlas detrás del mostrador. 
# a. ¿Qué distribución tiene X (nombre y valores de todos los
                              # parámetros)? 
  # b.Calcule P(X= 2), P(X <= 2) y P(X  >= 2). 
m <- 6; n <- 9; k <- 5 # m es el número de éxitos, n es el número de fracasos , y k es la muestra aleatoria tomada
x <- 0:(6) # es el valor de la probabilidad que me piden dentro del problema
dhyper(x, m, n, k) 
# probabilidad no más de una defectuosa
phyper(2, m, n, k, lower.tail = T)


# c. Calcule el valor medio y la desviación estándar de X. 


# Ejercicio 82 pp: 125 
#Considere escribir en un disco de computadora y luego enviar-
 # lo a través de un certificador que cuenta el número de pulsos
#faltantes. Suponga que este número X tiene una distribución
#de Poisson con parámetro   0.2. (Sugerido en “Average
 #                                  Sample Number for Semi-Curtailed Sampling Using the Pois-
  #                                   son Distribution”, J. Quality Technology, 1983: 126–129.)
#a. ¿Cuál es la probabilidad de que un disco tenga exacta-
 # mente un pulso faltante? 
dpois(1,0.2)

  #b. ¿Cuál es la probabilidad de que un disco tenga por lo me-
  #nos dos pulsos faltantes? 
1- ppois(1,0.2)

  #c. Si seleccionan dos discos independientemente, ¿cuál es la
#probabilidad de que ninguno contenga un pulso faltante?

ppois(0,0.2)
