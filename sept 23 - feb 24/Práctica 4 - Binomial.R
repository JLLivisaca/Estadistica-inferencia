
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
