# Funciones 
library(NLRoot)
# Función 

fx <-  function(x){
  log(x)/ sqrt(x^3)
}

curve(fx,-1,4) # dibujar funciones
abline(h=0, col="red", lwd=2)
raiz.funcion <-  BFfzero(fx,0.1,6) # con esto encuentro el limíte a
# para el límite b 
(probabilidad <-  integrate(fx,1,6.82))

# Valor esperado de una variable continua 
fx1 <-  function(x){
  (log(x)/ sqrt(x^3))*x
}
  
(valor.esperado <- integrate(fx1, 1,6.82))

# Varianza de una variable continua 
fx2 <-  function(x){
  (log(x)/ sqrt(x^3))*x^2
}

(varianza.1 <- integrate(fx2, 1,6.82))



