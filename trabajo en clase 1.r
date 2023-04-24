fx.esperado = function(x){
  x * exp(-x)
}
x=0:2
fx.esperado(x)

esperado.2= integrate(fx.esperado, 0,2)

# Varianza 
# Vx = E(x^2) - [E(x)]^2
# Valores esperados de X^2
fx.esperado.X.cuadrados = function(x){
  x^2 * exp(-x)
}
esperado.X.cuadrados = integrate(fx.esperado.X.cuadrados, 0,2)
# Varianza
(Varianza.esperado.2 = esperado.X.cuadrados$value -(esperado.2$value)^2)

# P(X<= x) = P(X<= 4)
pexp(4, 1)

# P(2 <= X <= 5)

H= pexp(c(5,2), 1)
H[1]-H[2]

# P(X>=4)
#1-P(X<=4)
1-pexp(4,1)
# Forma directa

pexp(4,1,lower.tail = F)
