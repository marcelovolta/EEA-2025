rm(list = ls())

# Librerias

library(tidyverse)

# 1) -----------------

tiburones <- read.csv("datos/ataques_tiburones.csv")
glimpse(tiburones)

tiburones <- tiburones %>% select(Case.Number, Date, Country)

# 2) -----------------

tiburones %>% 
  select(Country) %>% # Seleccionamos la variable del pais
  arrange(Country) %>% # La ordenamos
  distinct() %>% # Nos quedamos con los valores unicos
  as.list()

# Hay varios problemas, pero veamos dos:
#   
# - Hay algunos nombres que no están en mayúscula
# - Hay algunos nombres que tienen un "?" al final


# 3) -----------------

## a) ----------------------------

tiburones <- tiburones %>% 
  mutate(Country = str_to_upper(Country))

head(tiburones, 10)

## b) ----------------------------

tiburones <- tiburones %>% 
  mutate(Country = str_trim(Country, side = "both"))

## c) ----------------------------

tiburones <- tiburones %>% 
  mutate(Country = str_replace_all(Country, pattern = "\\?", replacement = ""))

# corroboro

tiburones %>% 
  select(Country) %>% # Seleccionamos la variable del Country
  arrange(Country) %>% # La ordenamos
  distinct() %>% # Nos quedamos con los valores unicos
  as.list()


# 4) -----------------

tiburones %>% select(Date) %>% distinct()

# Hay varios problemas, el mas frecuente es que la variable Date 
# viene en muchos casos con la palabra “Reported”
# lo cual nos impide que podamos tratar esa expresión como fecha.

# 5) -----------------

tiburones <- tiburones %>% 
  mutate(Date = str_replace_all(string = Date, pattern =  "Reported ", replacement = ""))

# chequeamos

tiburones %>% select(Date) %>% distinct()
glimpse(tiburones)
# vemos que Date es de tipo character.

# 6) -----------------

# convertimos Date a datetime
tiburones <- tiburones %>% mutate(Date = dmy(Date))

# Caused by warning:
#   !  891 failed to parse. 

# El mensaje indica que no pudo convertir a fecha 891 registros (porque falta limpiar mas esa variable).

# corroboramos que ahora Date es de tipo datetime
glimpse(tiburones)

# creamos las variables año, mes y dia
tiburones <- tiburones %>% 
  mutate(año = year(Date), # Obtener el año
         mes = month(Date, label = TRUE), # Obtener el nombre del mes
         dia = day(Date))# Obtener el día

glimpse(tiburones)

# 7) --------------------------------

# Quedarse con los ataques desde el año 1800 en adelante

tiburones_filtro <- tiburones %>% filter(año >=1800)
glimpse(tiburones_filtro)

# corroboramos

summary(tiburones_filtro$Date)

# Min.      1st Qu.       Median         Mean      3rd Qu. 
# "1804-02-26" "1950-04-14" "1983-06-07" "1971-07-14" "2005-05-11" 
# Max. 
# "2016-03-10" 

# 8) --------------------------------

# Crear un dataframe con la cantidad de ataques totales por región desde el 1800 en adelante

ataques_por_pais <- tiburones_filtro %>% 
  group_by(Country) %>% 
  count() %>% 
  ungroup() # Evitar que mantenga el grupo para otras operaciones

# 9) --------------------------------

# Realizar un gráfico de barras para los 20 países con mayor cantidad de ataques
ataques_por_pais_top20 <- ataques_por_pais %>%
  top_n(20) # la función top_n nos permite seleccionar los n registros con más ataques 

ggplot(ataques_por_pais_top20, 
       aes(x = reorder(Country, -n), y = n, fill = n)) + # la función reorder permite reordenar las regiones de mayor a menor por ataques
  geom_bar(stat = 'identity') + 
  theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_text(angle = 90)) +
  labs(title = 'Ataques por país', y = 'Ataques', x = '')

# 10) --------------------------------

# Realizar un gráfico de la evolución de ataques por año desde 1800

# primero creo un data frame con la cantidad de ataques por año
ataques_por_año <- tiburones_filtro %>% 
  group_by(año) %>% 
  count() %>% 
  ungroup() # Evitar que mantenga el grupo para otras operaciones

glimpse(ataques_por_año)

ggplot(ataques_por_año, aes(x = año, y = n)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title='Evolucion ataques de tiburones', x = 'Año', y = 'Ataques')

# 11) --------------------------------

# Realizar un gráfico del total de ataques por mes para AUSTRALIA y USA (Estados Unidos) para construir el ciclo anual de ataques en estos países

# me quedo con los datos de AUSTRALIA y USA
tiburones_aus_usa <- tiburones_filtro %>% filter(Country %in% c("AUSTRALIA","USA")) 

# creo un data frame con la cantidad de ataques por mes en AUSTRALIA y USA
ataques_por_mes <- tiburones_aus_usa %>% 
  group_by(Country, mes) %>% 
  count() %>% 
  ungroup() # Evitar que mantenga el grupo para otras operaciones

ggplot(ataques_por_mes, aes(x = mes, y = n, color = Country, group = Country)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title='Evolucion ataques de tiburones', x = 'Mes', y = 'Ataques')

