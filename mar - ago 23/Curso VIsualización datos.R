#install.packages("remotes")
#remotes::install_github("cienciadedatos/datos")
library(tidyverse)
library(datos)
df= datos::millas
library(ggplot2)
ggplot(data=df, fill= autopista)+
  geom_point(aes(x=cilindrada, y=autopista))
# usar plantillas son los temas
ggplot(data=df)+ 
  geom_point(mapping= aes(x= cilindros, y= autopista, 
                          colour=transmision))+ 
  theme_classic()

# con alpha controla la estética de los puntos
ggplot(data=df)+ geom_point(mapping = aes(x=cilindros,
                                          y= autopista,
                                          color=clase,
                                          alpha=clase))
# Clases 
class(df)
summary(df)
# variable stroke
ggplot(data=df)+ 
  geom_point(mapping = aes(color = cilindrada < 5 
                           ,x=cilindros,
                           y=autopista))
# se coloca esta condición para que grafique según la condición



# colocar graficas múltiples
ggplot(data=df)+ geom_point(mapping=aes(x=cilindros,y=autopista))+
  facet_wrap(traccion ~ combustible, nrow=4, ncol=3)

# colocar grid
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(autopista~ .)
# opiciones
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(traccion ~ .)

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(. ~ cilindros)


# geom línea de ajuste
ggplot(data=df)+
  geom_point(mapping=aes(x=cilindrada, y=autopista, color=
                           traccion))+
geom_smooth(mapping= 
                              aes(x=cilindrada, y=autopista, 
                                  linetype=traccion, 
                                  color=traccion))
#por grupos
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, group = traccion))

# Datos filtrados
ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point(mapping = aes(color = clase)) +
  geom_smooth(data = filter(millas, clase == "subcompacto"), se = FALSE)

ggplot(data=df, aes(x=cilindrada, y=autopista)) + geom_boxplot()

ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista, color = traccion)) +
  geom_point() +
  geom_smooth(se = T)

## Transformación de datos
vuelos  ##base de datos con los vuelos de NY en 2013
view(vuelos)
library(dplyr)
 #filtrar datos
filter(vuelos, mes==1,dia==1)
filter(vuelos, mes==10 | mes==5)
## CON OPERADORES LÓGICOS
filter(vuelos, !(atraso_llegada > 120 | atraso_salida > 120))
filter(vuelos  , atraso_llegada <= 120, atraso_salida <= 120)


### VALORES CON NA
is.na(vuelos)
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

## Barras
ggplot(data = diamantes) +
  stat_count(mapping = aes(x = corte))
demo <- tribble(
  ~corte,     ~freq,
  "Regular",   1610,
  "Bueno",     4906,
  "Muy Bueno", 12082,
  "Premium",   13791,
  "Ideal",     21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = corte, y = freq), stat = "identity")

# Dastos 
ggplot(data = diamantes) +
  stat_summary(
    mapping = aes(x = corte, y = profundidad),
    fun.min = min,
    fun.max = max,
    fun = median
  )
data=diamantes

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = corte))

# Datos
ggplot(diamantes)+ geom_bar(aes(x=corte, fill=corte))

# Boxplot
ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
  geom_boxplot() +
  coord_flip()

#Mapas
nz <- map_data("world2")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()


seq(1,10)
#Presiona Alt + Shift + K
#Presiona 

#### Manejo de datos con DPLRY####
# Para esto se usa la libreria y el conjunto de datos 'vuelos'
# Este conjunto de datos contiene los 336, 776 vuelos que partieron de la ciudad de Nueva York durante el 2013

library(dplyr)
library(datos) # en esta librería está VUELOS
df=vuelos

# Filtros 
df= filter(df, mes ==3,dia==1, atraso_salida <125)
df= filter(df, llegada_programada >430 & llegada_programada<=856)

# NA = valores faltantes
df = is.na(df$anio)
# Ordernar columnas
df= arrange(df, horario_salida)
# De forma descendente
df <- arrange(df, desc(salida_programada), horario_salida)
# Mutar variables
(df = mutate(df, 
            hora = horario_llegada - horario_salida))

(df= transmute(df,hora = horario_llegada - horario_salida )) # solo conserva la variable creada
# Pra variables como horas
transmute(vuelos,
          horario_salida,
          hora = horario_salida %/% 100,
          minuto = horario_salida %% 100
)

# Uso de summarise #
summarise(vuelos, atraso = mean(atraso_salida, na.rm = T))
# summarise siempre se usa con group_by
por_dia <- group_by(vuelos, anio, mes, dia)
summarise(por_dia, atraso = mean(atraso_salida, na.rm = TRUE))
# Se da elación entre la distancia y el atraso promedio para cada ubicación
por_destino <- group_by(vuelos, destino)
atraso <- summarise(por_destino,
                    conteo = n(),
                    distancia = mean(distancia, na.rm = TRUE),
                    atraso = mean(atraso_llegada, na.rm = TRUE)
)
atraso <- filter(atraso, conteo > 20, destino != "HNL")
atraso <- filter(atraso, atraso >=0) # lo coloqué

# Parece que las demoras aumentan con las distancias hasta ~ 750 millas
# y luego disminuyen. ¿Tal vez a medida que los vuelos se hacen más 
# largos, hay más habilidad para compensar las demoras en el aire?
library(ggplot2)
ggplot(data = atraso, mapping = aes(x = distancia, y = atraso)) +
  geom_point(aes(size = conteo), alpha = 1/3) +
  geom_smooth(se = T)

# otra forma de realizar

atrasos <- vuelos %>% 
  group_by(destino) %>% 
  summarise(
    conteo = n(),
    distancia = mean(distancia, na.rm = TRUE),
    atraso = mean(atraso_llegada, na.rm = TRUE)
  ) %>% 
  filter(conteo > 20, destino != "HNL")
ggplot(data = atraso, mapping = aes(x = distancia, y = atraso)) +
  geom_point(aes(size = conteo), alpha = 1/3) +
  geom_smooth(se = T)
# agrupapos con pipe

df <- vuelos
df %>% group_by(anio,mes,dia)%>% 
  summarise(mean= mean(atraso_salida, na.rm=T))

ggplot(df)+ 
  geom_point(aes(x=mean, y = anio))

# otro ejemplo 
no_cancelados <- vuelos %>% 
  filter(!is.na(atraso_salida), !is.na(atraso_llegada))

no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(mean = mean(atraso_salida))
# COnteos
atrasos <- no_cancelados %>% 
  group_by(codigo_cola) %>% 
  summarise(
    atraso = mean(atraso_llegada)
  )

ggplot(data = atrasos, mapping = aes(x = atraso)) + 
  geom_freqpoly(binwidth = 10)
# diagrama de dispersión 
atrasos <- no_cancelados %>% 
  group_by(codigo_cola) %>% 
  summarise(
    atraso = mean(atraso_llegada),
    n = n()
    
  ) %>% filter(n<25)

ggplot(data = atrasos, mapping = aes(x = n, y = atraso)) + 
  geom_point(alpha = 1/10)
# Datos para bateadores
#Veamos cómo el rendimiento promedio de los bateadores en el béisbol
#está relacionado con el número de veces que les toca batear. Aquí utilizaremos
#el conjunto de datos de bateadores para calcular el promedio de bateo 
#(número de bateos / número de intentos) de cada jugador de béisbol de las Grandes Ligas.

# Convierte a tibble para puedas imprimirlo de una manera legible
bateo <- as_tibble(datos::bateadores)

rendimiento_bateadores <- bateo %>% 
  group_by(id_jugador) %>% 
  summarise(
    pb = sum(golpes, na.rm = TRUE) / sum(al_bate, na.rm = TRUE),
    ab = sum(al_bate, na.rm = TRUE)
  )

rendimiento_bateadores %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = pb)) +
  geom_point() +
  geom_smooth(se = FALSE)

# # ¿Cuándo salen los primeros y los últimos vuelos cada día?
no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(
    primero = min(horario_salida),
    ultimo = max(horario_salida)
  )
no_cancelados %>% 
  count(destino)

# ¿Cuántos vuelos salieron antes de las 5 am? 
# (estos generalmente son vuelos demorados del día anterior)
no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(n_temprano = sum(horario_salida < 500))

# ¿Qué proporción de vuelos se retrasan más de una hora?
no_cancelados %>% 
  group_by(anio, mes, dia) %>% 
  summarise(hora_prop = mean(atraso_llegada > 60))

#Encuentra los peores miembros de cada grupo:

vuelos_sml <- vuelos 
vuelos_sml %>%
  group_by(anio, mes, dia) %>%
  filter(rank(desc(atraso_llegada)) < 10)

# vuelos populares
destinos_populares <- vuelos

destinos_populares %>% group_by(destino) %>% 
  filter(n() > 365)%>% filter(atraso_llegada > 0) %>% 
  mutate(prop_atraso = atraso_llegada / sum(atraso_llegada)) %>% 
  select(anio:dia, destino, atraso_llegada, prop_atraso)
# hy %>%  colocar control shift m, y para <-  es alt -


library(datos)
ggplot(data=diamantes)+geom_bar(mapping = aes(x=corte))

diamantes %>% count(corte) %>% filter( corte=="Bueno"|corte=="Muy bueno") %>% mutate(i=corte)

# Probar datos
library(dplyr)
library(ggplot2)
diamantes_2 <- diamantes %>% filter(y>3 & y<20)

diamantes_2_1 <- diamantes %>% mutate(y=ifelse(y<3 |y>20, NA, y))
ggplot(data=diamantes, mapping= aes(x=precio))+ 
  geom_freqpoly(mapping = aes(colour = corte),bindwidth=500)

ggplot(data = diamantes, mapping = aes(x = precio)) + 
  geom_freqpoly(mapping = aes(colour = corte), binwidth = 500)

# Boxplot
ggplot(data= diamantes) +
  geom_boxplot(mapping=aes(x=corte, y = precio))
ggplot(data= diamantes )+ 
  geom_histogram(mapping=aes(x=precio))
# con reorder se puede ordenar los boxplot
ggplot(diamantes, mapping=aes(x=reorder(corte, precio, FUN=median), y = precio)) +
  geom_boxplot()+ coord_flip()
#coord_flip es para girar 90 

# Variación entre dos variables categóricas

ggplot(diamantes)+ 
  geom_count(mapping = aes(x=reorder(corte, claridad),y= claridad, na.rm=T))

# Patrones 

library(modelr)

mod <- lm(log(precio) ~ log(quilate), data = diamantes)

diamantes2 <- diamantes %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamantes2) + 
  geom_point(mapping = aes(x = quilate, y = resid))
ggplot(data = diamantes2) + 
  geom_boxplot(mapping = aes(x = corte, y = resid))

diamantes %>% 
  count(corte, claridad) %>% 
  ggplot(aes(claridad, corte, fill = n)) + 
  geom_tile()
