(pbinom(5,25,0.25,lower.tail = F)) #0.6217215
((dbinom(0:5,25,.25)))
sum(dhyper(x=5:25, m=12, k=8, n=13)) #0.2860412
# Valor esperado 6 y desv 2.44949
#lambda es 12 * t = 12* 2.5=30
ppois(20,30,lower.tail = F) #0.9647154
fx <- function(x){
  0.09375*(4-x^2) 
}
# Valor integral [0.09375*(4x-x^3/3)] 
(t <- integrate(fx,-2,2)) # Si
(t1 <- integrate(fx,1,2)) #0.15625
#	P(-2.50 ≤ Z ≤ 2.50), P(Z  ≤ 1.37), P(1.75  ≤ Z), P(0  ≤ Z  ≤ 1)
(pronn <- pnorm(2.5,0,1,lower.tail = T)-pnorm(-2.5,0,1,lower.tail = T))#0.9875807
(pronn <- pnorm(1.37,0,1,lower.tail = T))#0.9146565
(pronn <- pnorm(1.75,0,1,lower.tail = F)) #0.04005916
(pronn <- pnorm(1,0,1,lower.tail = T)-pnorm(0,0,1,lower.tail = T))#0.3413447

