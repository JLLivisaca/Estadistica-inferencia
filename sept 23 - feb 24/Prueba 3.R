#### Problema 1 ####
#Literal a
viscosidad <- c(418, 421, 421, 422, 425, 427, 431, 434, 
                437, 439, 446, 447, 448, 453, 454, 463, 465)
(sd1 <- sd(viscosidad))
(media <- mean(viscosidad))
(zc <- qnorm(0.035,mean = 0,sd=1))
(media + zc*(sd1/sqrt(17)))
library(asbio)
ci.mu.z(viscosidad, conf = 0.93, sigma = sd1) #
#93% z Confidence interval for population mean 
#Estimate     3.5%    96.5% 
  #438.2941 431.6390 444.949
ci.mu.t(viscosidad, conf = 0.93) #
#93% z Confidence interval for population mean 
#Estimate     3.5%    96.5% 
#438.2941 431.1624 445.4258 

#### Problema 3 ####
#Literal a
agua <-  c(26.7, 25.8, 24.0, 24.9, 26.4, 25.9, 24.4,
           21.7, 24.1, 25.9, 27.3, 26.9, 27.3, 24.8, 23.6)
ci.sigma(agua,conf = 0.74)
#87% Confidence interval for population variance 
#Estimate      13%      87% 
# 2.492667 1.744475 4.176610  (2.043676)
agua.1 <-c(26.7, 25.8, 24.0, 24.9, 26.4, 25.9, 24.4) 
agua.2 <- c(21.7, 24.1, 25.9, 27.3, 26.9, 27.3, 24.8, 23.6)
ci.sigma(agua.1,conf = 0.74)
#74% Confidence interval for population variance 
#Estimate       13%       87% 
  #1.0495238 0.6376622 2.5327571 (1.591464)
ci.sigma(agua.2,conf = 0.74)
#74% Confidence interval for population variance 
#Estimate      13%      87% 
  #4.054286 2.533217 8.986370 (2.997727)
# Valor de F
(qf(c(0.13), df1=6, df2=7, lower.tail=TRUE)) #0.380776
#### Problema 4 ####

h=pexp(c(20000,30000), rate=1/25000, lower.tail = F)
h[1]-h[2] # R: 0.1481348 (0.4493290, 0.3011942)


