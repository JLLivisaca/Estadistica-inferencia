# Anova de un factor 
#Comparación de cuatro tipos de cuero. Un fabricante de calzado desea mejorar 
#la calidad de las suelas, las cuales se pueden hacer con uno de los cuatro tipos de 
#cuero A, B, C y D disponibles en el mercado. Para ello, prueba los cueros con una 
#máquina que hace pasar los zapatos por una superficie abrasiva; la suela de éstos se 
#desgasta al pasarla por dicha superficie. Como criterio de desgaste se usa la pérdida 
#de peso después de un número fijo de ciclos. Se prueban en orden aleatorio 24 zapa-
#  tos, seis de cada tipo de cuero. Al hacer las pruebas en orden completamente al azar 
#se evitan sesgos y las mediciones en un tipo de cuero resultan independientes de las 
#demás. Los datos (en miligramos) sobre el desgaste de cada tipo de cuero se mues-
# tran en la tabla.

# Código para anova
AnovaModel.1 <- aov(Peso ~ TipoCuer, data=Cueros)
summary(AnovaModel.1)
with(Cueros, numSummary(Peso, groups=TipoCuer, statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.1, linfct = mcp(TipoCuer = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.99)) # confidence intervals
  print(cld(.Pairs, level=0.01)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
oneway.test(Peso ~ TipoCuer, data=Cueros) # Welch test
