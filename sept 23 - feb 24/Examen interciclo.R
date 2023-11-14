# Problema 3
#Literal a
x=3
1- (choose(7,x)*choose(11-7,6-x))/ choose(11,6) #0.6969697
#Literal b
#negativa
x=4
k=1:3
p=5/11
(cumsum((choose(x-1,k-1)*(p^k)*(1-p)^(x-k)))) #0.4118571
#Literal c
# Binomial
(1- pbinom(2,11,7/100)) #0.03697929

# Problema 4
#Literal a
(Z1= (63-70)/3)
(Z2= (75-70)/3)
(fit <- pnorm(Z2,mean=0,sd=1,lower.tail = T) - pnorm(Z1,mean=0,sd=1,lower.tail = T))#0.9423943
#literal b
(Z=qnorm(.95,mean=0,sd=1,lower.tail = T)) # es el 90% de probabilidad encerrada en dos lados
(c= 3*Z+10) #14.93456
#Literal c
d=70
(Zc = (d-70)/3)
(probabilidad = pnorm(Zc, mean=0, sd=1, lower.tail = T))
pbinom(8,10,probabilidad) #0.9892578

#Problema 5
#Literal a
n=500;p=0.5/100
(lamda= n*p) 
(1-ppois(4,lamda)) #0.108822
#Literal b
(p=150/400)
dgeom(9,p) #0.005456968
