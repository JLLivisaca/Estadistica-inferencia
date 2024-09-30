#### Datos ####
T_gas_in <- 300 # °C (entrada de gases calientes)
#T_gas_out <- 230 # °C (salida de gases calientes, igual a la temperatura de la pared del tubo)
T_superficie <- 80 # °C (entrada del agua)
D <- 0.021 # m (diámetro exterior del tubo)
L <- 1 # m (longitud de cada tubo)
filas <- 16
tubos_por_fila <- 8
n_tubos <- filas * tubos_por_fila
cp_gas <- 1033 # J/kgK (calor específico del gas, típico del aire)
mu_gas <- 2.76e-5 # Pa.s (viscosidad dinámica del gas a temperatura media)
Pr_gas <- 0.6946 # Número de Prandtl para el gas
k_gas <- 0.04104 # W/mK (conductividad térmica del gas)
rho_gas <- 0.6746 # kg/m^3 (densidad del gas)
V <- 4.5 # m/s (velocidad del gas)
St=0.08
Vmax <- (St /(St-D))*V
cp_agua <- 4184 # J/kgK (calor específico del agua)
m_dot <- rho_gas*V*tubos_por_fila*St*L # kg/s (flujo másico del agua)

# Área de transferencia de calor
A <- pi * D * L * n_tubos

# Calcular el Número de Reynolds
Re <- (rho_gas * Vmax * D) / mu_gas #3131.894

# Constantes para la correlación de Zhukauskas (banco de tubos en alineación in-line)
C <- 0.27
m <- 0.63
n <- 0.36
Pr_superficie <- 0.7154
# Calcular el Número de Nusselt
Nu <- C * Re^m * Pr_gas^(1/3) *(Pr_gas/Pr_superficie)#36.99585

# Calcular el coeficiente de transferencia de calor
h <- Nu * k_gas / D

# Inicialización de la temperatura de salida del agua
T_gas_out_suposicion <- 250 # °C, suposición inicial

# Tolerancia y error inicial
tolerancia <- 0.03
error <- 1
iteracion <- 0

# Ciclo while para ajustar la suposición de la temperatura de salida del agua
while (error > tolerancia) {
  iteracion <- iteracion + 1
  
  # Calcular la diferencia de temperatura media logarítmica (LMTD)
  delta_T1 <- T_superficie - T_gas_in
  delta_T2 <- T_superficie - T_gas_out_suposicion
  LMTD <- (delta_T1 - delta_T2) / log(delta_T1 / delta_T2)
  
  # Calcular la transferencia de calor usando LMTD
  Q <- h * A * LMTD
  
  # Calcular la temperatura de salida del agua usando la fórmula proporcionada
  T_agua_out_calculada <- T_gas_out - (T_gas_out - T_gas_in) * exp(-A * h / (m_dot * cp_agua))
  
  # Calcular el error relativo
  error <- abs((T_agua_out_calculada - T_agua_out_suposicion) / T_agua_out_suposicion)
  
  # Imprimir información de la iteración
  cat("Iteración:", iteracion, "\n")
  cat("Temperatura de salida del agua supuesta:", T_agua_out_suposicion, "°C\n")
  cat("Temperatura de salida del agua calculada:", T_agua_out_calculada, "°C\n")
  cat("Error relativo:", error, "\n\n")
  
  # Actualizar la suposición
  T_agua_out_suposicion <- T_agua_out_calculada
}

# Mostrar resultados finales
cat("Temperatura de salida del agua final calculada:", T_agua_out_calculada, "°C\n")
cat("Razón de transferencia de calor por unidad de longitud de los tubos:", Q, "W\n")
cat("Error relativo final:", error, "\n")
cat("Coeficiente de transferencia de calor h:", h, "W/m^2K\n")


##########optimización

# Datos
T_gas_in <- 300 # °C (entrada de gases calientes)
T_gas_out <- 80 # °C (salida de gases calientes, igual a la temperatura de la pared del tubo)
T_agua_in <- 25 # °C (entrada del agua)
D <- 0.021 # m (diámetro exterior del tubo)
L <- 1 # m (longitud de cada tubo)
filas <- 16
tubos_por_fila <- 8
n_tubos <- filas * tubos_por_fila
cp_gas <- 1005 # J/kgK (calor específico del gas, típico para el aire)
mu_gas <- 2.05e-5 # Pa.s (viscosidad dinámica del gas a temperatura media)
Pr_gas <- 0.7 # Número de Prandtl para el gas
k_gas <- 0.0262 # W/mK (conductividad térmica del gas)
rho_gas <- 1.177 # kg/m^3 (densidad del gas)
v <- 4.5 # m/s (velocidad del gas)
cp_agua <- 4184 # J/kgK (calor específico del agua)
m_dot <- 6 # kg/s (flujo másico del agua)

# Área de transferencia de calor
A <- pi * D * L * n_tubos

# Calcular el Número de Reynolds
Re <- (rho_gas * v * D) / mu_gas

# Constantes para la correlación de Zhukauskas (banco de tubos en alineación in-line)
C <- 0.27
m <- 0.63
n <- 0.36

# Calcular el Número de Nusselt
Nu <- C * Re^m * Pr_gas^(1/3)

# Calcular el coeficiente de transferencia de calor
h <- Nu * k_gas / D

# Inicialización de la temperatura de salida del agua
T_agua_out_suposicion <- 50 # °C, suposición inicial

# Tolerancia y error inicial
tolerancia <- 0.03
error <- 1
iteracion <- 0
max_iterations <- 100 # Limitar el número de iteraciones para evitar bucles infinitos
relaxation_factor <- 0.5 # Factor de relajación inicial
prev_error <- error

# Ciclo while para ajustar la suposición de la temperatura de salida del agua
while (error > tolerancia && iteracion < max_iterations) {
  iteracion <- iteracion + 1
  
  # Calcular la diferencia de temperatura media logarítmica (LMTD)
  delta_T1 <- T_gas_in - T_agua_out_suposicion
  delta_T2 <- T_gas_out - T_agua_in
  LMTD <- (delta_T1 - delta_T2) / log(delta_T1 / delta_T2)
  
  # Calcular la transferencia de calor usando LMTD
  Q <- h * A * LMTD
  
  # Calcular la temperatura de salida del agua usando la fórmula proporcionada
  T_agua_out_calculada <- T_gas_out - (T_gas_out - T_agua_in) * exp(-A * h / (m_dot * cp_agua))
  
  # Calcular el error relativo
  error <- abs((T_agua_out_calculada - T_agua_out_suposicion) / T_agua_out_suposicion)
  
  # Ajustar el factor de relajación si el error no está disminuyendo
  if (error > prev_error) {
    relaxation_factor <- relaxation_factor * 0.5
  }
  
  # Imprimir información de la iteración
  cat("Iteración:", iteracion, "\n")
  cat("Temperatura de salida del agua supuesta:", T_agua_out_suposicion, "°C\n")
  cat("Temperatura de salida del agua calculada:", T_agua_out_calculada, "°C\n")
  cat("Error relativo:", error, "\n")
  cat("Factor de relajación:", relaxation_factor, "\n\n")
  
  # Actualizar la suposición usando el factor de relajación
  T_agua_out_suposicion <- relaxation_factor * T_agua_out_calculada + (1 - relaxation_factor) * T_agua_out_suposicion
  
  # Actualizar el error anterior
  prev_error <- error
}

# Mostrar resultados finales
cat("Temperatura de salida del agua final calculada:", T_agua_out_calculada, "°C\n")
cat("Razón de transferencia de calor por unidad de longitud de los tubos:", Q, "W\n")
cat("Error relativo final:", error, "\n")
cat("Coeficiente de transferencia de calor h:", h, "W/m^2K\n")


#### con método númerico ####
# Datos
T_gas_in <- 300 # °C (entrada de gases calientes)
#T_gas_out <- 230 # °C (salida de gases calientes, igual a la temperatura de la pared del tubo)
T_superficie <- 80 # °C (entrada del agua)
D <- 0.021 # m (diámetro exterior del tubo)
L <- 1 # m (longitud de cada tubo)
filas <- 16
tubos_por_fila <- 8
n_tubos <- filas * tubos_por_fila
cp_gas <- 1033 # J/kgK (calor específico del gas, típico del aire)
mu_gas <- 2.76e-5 # Pa.s (viscosidad dinámica del gas a temperatura media)
Pr_gas <- 0.6946 # Número de Prandtl para el gas
k_gas <- 0.04104 # W/mK (conductividad térmica del gas)
rho_gas <- 0.6746 # kg/m^3 (densidad del gas)
V <- 4.5 # m/s (velocidad del gas)
St=0.08
Vmax <- (St /(St-D))*V
cp_agua <- 4184 # J/kgK (calor específico del agua)
m_dot <- rho_gas*V*tubos_por_fila*St*L # kg/s (flujo másico del agua)

# Área de transferencia de calor
A <- pi * D * L * n_tubos

# Calcular el Número de Reynolds
Re <- (rho_gas * V * D) / mu_gas

# Constantes para la correlación de Zhukauskas (banco de tubos en alineación in-line)
C <- 0.27
m <- 0.63
n <- 0.36
Pr_superficie <- 0.7154
# Calcular el Número de Nusselt
Nu <- C * Re^m * Pr_gas^(1/3) *(Pr_gas/Pr_superficie)

# Calcular el coeficiente de transferencia de calor
h <- Nu * k_gas / D

# Inicialización de la temperatura de salida del agua
T_agua_out_suposicion <- 50 # °C, suposición inicial

# Tolerancia y error inicial
tolerancia <- 0.03
error <- 1
iteracion <- 0
max_iterations <- 100 # Limitar el número de iteraciones para evitar bucles infinitos

# Función de error
error_function <- function(T_agua_out_guess) {
  delta_T1 <- T_superficie - T_gas_in
  delta_T2 <- T_superficie - T_gas_out_suposicion
  LMTD <- (delta_T1 - delta_T2) / log(delta_T1 / delta_T2)
  Q <- h * A * LMTD
  T_agua_out_calculada <- T_gas_out - (T_gas_out - T_gas_in) * exp(-A * h / (m_dot * cp_agua))
  return(T_agua_out_calculada - T_agua_out_guess)
}

# Derivada numérica de la función de error
error_function_derivative <- function(T_agua_out_guess, epsilon = 1e-6) {
  (error_function(T_agua_out_guess + epsilon) - error_function(T_agua_out_guess - epsilon)) / (2 * epsilon)
}

# Ciclo Newton-Raphson
while (error > tolerancia && iteracion < max_iterations) {
  iteracion <- iteracion + 1
  
  # Calcular el valor de la función de error y su derivada en la suposición actual
  f_x <- error_function(T_agua_out_suposicion)
  f_prime_x <- error_function_derivative(T_agua_out_suposicion)
  
  # Actualizar la suposición usando el método de Newton-Raphson
  T_agua_out_nueva <- T_agua_out_suposicion - f_x / f_prime_x
  
  # Calcular el error relativo
  error <- abs((T_agua_out_nueva - T_agua_out_suposicion) / T_agua_out_suposicion)
  
  # Imprimir información de la iteración
  cat("Iteración:", iteracion, "\n")
  cat("Temperatura de salida del agua supuesta:", T_agua_out_suposicion, "°C\n")
  cat("Nueva temperatura de salida del agua:", T_agua_out_nueva, "°C\n")
  cat("Error relativo:", error, "\n\n")
  
  # Actualizar la suposición
  T_agua_out_suposicion <- T_agua_out_nueva
}

# Calcular la transferencia de calor usando la suposición final
delta_T1 <- T_gas_in - T_agua_out_suposicion
delta_T2 <- T_gas_out - T_agua_in
LMTD <- (delta_T1 - delta_T2) / log(delta_T1 / delta_T2)
Q <- h * A * LMTD

# Mostrar resultados finales
cat("Temperatura de salida del agua final calculada:", T_agua_out_suposicion, "°C\n")
cat("Razón de transferencia de calor por unidad de longitud de los tubos:", Q, "W\n")
cat("Error relativo final:", error, "\n")
cat("Coeficiente de transferencia de calor h:", h, "W/m^2K\n")
#### Problema 3 ####
# Librerías necesarias
library(pracma)

# Datos del problema
L <- 5 # in
D <- 0.8 # in
rho <- 61.2 # lbm/ft³
cp <- 0.93 # Btu/lbm°F
k <- 0.44 # Btu/h·ft°F
alpha <- 0.0077 # ft²/h
T_i <- 40 # °F
T_inf <- 212 # °F
h <- 120 # Btu/h·ft²°F

# Conversión de unidades
D_ft <- D / 12 # ft
r0 <- D_ft / 2 # Radio del cilindro en ft

# Números de Biot y Fourier
Bi <- h * r0 / k

# Función para calcular la temperatura en el centro del cilindro infinitamente largo
temperature_cylinder_inf <- function(t) {
  Fo <- alpha * t / (r0^2)
  theta_star <- 2 * exp(-Bi^2 * Fo / (1 + Bi * sqrt(Fo))) / sqrt(pi * Fo)
  T_center <- T_inf + (T_i - T_inf) * theta_star
  return(T_center)
}

# Tiempos a evaluar (en horas)
times <- c(5, 10, 15) / 60

# Cálculo de temperaturas para un cilindro infinitamente largo
temperatures_inf <- sapply(times, temperature_cylinder_inf)

# Imprimir resultados
cat("Temperaturas en el centro de la salchicha (cilindro infinitamente largo):\n")
for (i in 1:length(times)) {
  cat(sprintf("Tiempo: %d min, Temperatura: %.2f °F\n", times[i] * 60, temperatures_inf[i]))
}

# Resultados
temperatures_inf


