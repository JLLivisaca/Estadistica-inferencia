#### Examen Interciclo ####

#----------------Problema 1 -----------------------

# Literal a
dist.negativa = function(x,r,p){
  choose (x+r-1, r-1)* p^r *(1-p)^x
}
# La binomial negativa usa las combinaciones con repetición, se puede usar.

dist.negativa(4,2.5,0.1) # por el redondeo qué hace la fórmula la Probabilidad es más elevada
dnbinom(4,2.5, 0.1) # es una propuesta pero en este caso la función toma el mismo redondeo y las combinaciones con repetición

# literal b 
# P(X >= 1) = 1- P(X<1)
(1 - dist.negativa(0,2.5,0.1))

#----------------Problema 2 -----------------------
# Literal a

# Para esto se usa la fórmula de R que es la de pf, que da la probabilidad de la distribución F

pf(0.167, 3,8, lower.tail = T) # Es la probabilidad acumulada para P(X<= 0.167)
pf(4.74, 3,8, lower.tail = T) # Es la probabilidad acumulada para P(X<= 4.74)
(pf(4.74, 3,8)-pf(0.167, 3,8 )) # Es la probabilidad entre los dos valores
 # Literal b
(qf(0.07, 5,8, lower.tail = F))

# Literal C
library(Rcmdr)
x11()
local({
  .x <- seq(0.005, 19.387, length.out=1000)  
  plotDistr(.x, df(.x, df1=3, df2=8), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("F Distribution:  Numerator df = 3, Denominator df = 8"), 
            regions=list(c(0, 0.167), c(4.74, Inf)), col=c('#BEBEBE', '#BEBEBE'), 
            legend.pos='topright')
})
# Grafica con F0.07 
local({
  .x <- seq(0.005, 19.387, length.out=1000)  
  plotDistr(.x, df(.x, df1=3, df2=8), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("F Distribution:  Numerator df = 3, Denominator df = 8"), 
            regions=list(c(3.202003, Inf)), col=c('#BEBEBE', '#BEBEBE'), 
            legend.pos='topright')
})


#----------------Problema 3 -----------------------

# Para este caso se tiene que la probabilidad para que un ingeniero sea entevistado es
# 7/11 y para que un arquitecto sea entrevistado es de 4/11
# Las entrevistas ha sido dividas en 2 días. El primero tendrá 6 entrevistas
# mientras que en el segundo día se tendra 5 entrevistas.
# Por las preguntas estamos en una distribución geométrica.

# Literal a
# a.	¿Cuál es la probabilidad de que se requiera, en el primer día,
# entrevistar a 2 personas para tener la primera entrevista con un ingeniero? 
dgeom(x=1, prob=7/11)

# Literal a

# a.	¿Cuál es la probabilidad de que se requiera, en el primer día,
# entrevistar a 2 personas para tener la primera entrevista con un ingeniero? 
dgeom(x=1, prob=7/11) # El valor de 1 es solo porque así se lo ingresa en R

# Literal b

# b.	¿Cuál es la probabilidad de que se requiera, en el segundo día, 
# entrevistar a 5 personas para tener la primera entrevista con un arquitecto?  
dgeom(x=4, prob=4/11) # El valor de 3 es solo porque así se lo ingresa en R

# Literal c
# Valor esperado 
# Ingenieros 
(Ex= 1/(7/11)) 
# Arquitectos 
(Ex1= 1/(4/11)) 
# Las desviaciones tándar son para una distribución geométrica




