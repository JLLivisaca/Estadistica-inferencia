# Pruebas de bondad de ajuste
  # Prueba de normalidad 
    # Procedimiento para la prueba
    # 1) Ho: f(x)= N(miu, desv)
    # 2) Ha: lo contrario a Ho
    # 3) alfa = 0.05
    # 4) Estadístico de la prueba 
    # X = es la aaltura de los estudiantes de 4to ciclo de IE.
    altura = c(1.58,1.67,1.56,1.67,1.69,1.67,1.77,1.63,1.75,1.77,
               1.55,1.61,1.58,1.67,1.92,1.75,1.73,1.73)
    length(altura)
     # Vamos a determinar si la altura de los estudiantes de 4to ciclo de IE
      # sigue una distribución normal (miu=0,desv=1)
      # Ho: fx = N(0,1)
      # Ha: lo contrario a Ho
      # Por tener un n menor a 50, se elige el estadístico de Shapiro- Wilk
    #5) Calcular el estadístico
      shapiro.test(altura)
      # Decidir 
      # Si el p valor es menor que el alfa, Rechazo Ho
      # Si el p valor es mayor que el alfa, No rechazo Ho
      # p valor es de 0.2751 y el alfa es de 0.05, por lo tanto 
      # No rechazo la Ho
    #6) Concluir 
      # Se tiene evidencia estadística para decir que las alturas de los estudiantes
      # de 4to ciclo de IE, siguen una distribución N(0,1), con un nivel de significancia
      # de 0.05
      
      
      
        
    
    
    
    
    
  