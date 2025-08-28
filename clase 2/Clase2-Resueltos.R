
# Ejercicio 1: precios y dolar -------------------------------------

rm(list = ls())

#Librerias
library(tidyverse)
library(lubridate)

# dolar ------------

## 1) leer el data set ------------

dolar <- read.csv("datos/dolar_oficial_ambito.csv")

# miramos la estructura
glimpse(dolar)

## 2) ------------------

dolar <- dolar %>% mutate(cotizacion_promedio = (compra + venta) / 2)

## 3) ----------------------------------------

# Convertir la variable fecha de string a datetime. 

dolar <- dolar %>% mutate(fecha = dmy(fecha))
glimpse(dolar)

# Crear las variables de año, mes y día
dolar <- dolar %>% mutate(año = year(fecha), mes = month(fecha), dia = day(fecha))
glimpse(dolar)

## 4) --------------------

# Graficar la evolución diaria de la cotización promedio

ggplot(dolar, aes(x = fecha, y = cotizacion_promedio)) + 
  geom_line() +
  labs(x = "Fecha", y = "Tipo de cambio oficial", title = "Evolución del tipo de cambio oficial") +
  theme_bw()

## 5) -----------------------
# Crear un dataframe con la mediana de la cotización promedio 
# para cada año y mes (esta será la cotización mensual del dólar oficial)

dolar_mensual <- dolar %>% group_by(año, mes) %>%
  summarise(cotizacion_mensual = median(cotizacion_promedio)) %>% 
  ungroup() # Evitar que mantenga el grupo para otras operaciones

glimpse(dolar_mensual)

# IPC -------------------

## 1) -------------

ipc <- read.csv("datos/ipc-mensual.csv")
glimpse(ipc)

## 2) --------------------

ipc_tidy <- ipc %>% pivot_longer(cols = -Apertura, 
                                 names_to = "fecha",
                                 values_to = "indice")
glimpse(ipc_tidy)

## 3) ----------------------------

# pruebo

ipc_tidy %>% 
  mutate(fecha2 = parse_date_time(fecha, orders = "my")) %>% 
  glimpse()

# chequeo que la funcion fecha2 este bien creada, ahora hago el cambios 
# (pero antes guardo el ipc_tidy en ipc_tidy_original para poder comparar luego)

ipc_tidy_orginal <- ipc_tidy

ipc_tidy <- ipc_tidy %>% mutate(fecha = parse_date_time(fecha, orders = "my"),
                                año = year(fecha), 
                                mes = month(fecha))
glimpse(ipc_tidy)

## 4) ----------------------

ipc_tidy %>% filter(Apertura == "Nivel general") %>% 
  ggplot(., aes(x = fecha, y = indice)) + 
  geom_point() + 
  geom_line() +
  labs(x = "Fecha", y = "IPC", title = "Evolución del IPC") +
  theme_bw()


# con fecha como character

ipc_tidy_orginal %>% filter(Apertura == "Nivel general") %>% 
  ggplot(., aes(x = fecha, y = indice)) + 
  geom_point() + 
  geom_line() +
  labs(x = "Fecha", y = "IPC", title = "Evolución del IPC") +
  theme_bw()

# se ve mal porque (al estar fecha como character) no toma en cuenta el orden de las fechas

## 5) ----------------------

# Graficar la evolución de los 4 grupos

# con color por apertura

ipc_tidy %>% ggplot(., aes(x = fecha, y = indice, color = Apertura)) + 
  geom_point() + 
  geom_line() +
  labs(x = "Fecha", y = "IPC", title = "Evolución del IPC") +
  theme_bw()

# otra forma: en facetas

ipc_tidy %>% ggplot(., aes(x = fecha, y = indice)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ Apertura) + 
  labs(x="Fecha", y="IPC", title = "Evolución del IPC") +
  theme_bw()

# ipc y dolar ----------------------

## 1) -------------------------------------------

ipc_dolar <- inner_join(ipc_tidy, dolar_mensual, by = c('año', 'mes'))

glimpse(ipc_dolar)

## 2) ----------------

cotizacion_mes_base <- dolar_mensual %>%
  filter(mes == 12, año == 2016) %>%
  pull(cotizacion_mensual) # Pull nos permite seleccionar el valor como un número, select lo traería como un dataframe 

cotizacion_mes_base
# 15.93

ipc_dolar <- ipc_dolar %>% mutate(indice_dolar = cotizacion_mensual * 100 / cotizacion_mes_base)

glimpse(ipc_dolar)

## 3) ------------------------

# Calcular la media, el desvío estándar, la mediana y el rango
# del indice del dólar y del IPC General para cada uno de los años

resumen <- ipc_dolar %>% filter(Apertura == "Nivel general") %>%
  group_by(año) %>% 
  summarise(dolar_promedio = mean(indice_dolar),
            desvio_dolar = sd(indice_dolar),
            mediana_dolar = median(indice_dolar),
            rango_dolar = max(indice_dolar) - min(indice_dolar),
            ipc_promedio = mean(indice),
            desvio_ipc = sd(indice),
            mediana_ipc = median(indice),
            rango_ipc = max(indice)-min(indice))

resumen


## 4) ------------------------------------------

# Graficar la evolución del indice del dólar y 
# del nivel general del IPC

ipc_dolar_gral <- ipc_dolar %>% 
  filter(Apertura == "Nivel general") %>% 
  mutate(IPC_nivel_general = indice)


ggplot() +
  geom_line(data = ipc_dolar_gral,
            aes(x = fecha, y = IPC_nivel_general, color = "IPC Nivel general")) +
  geom_line(data = ipc_dolar,
            aes(x = fecha, y = indice_dolar, color = "Dolar")) +
  labs(x = "Fecha", y = "Indice", title = "Evolución del IPC y Dólar",
       color = "Variable") +        # título de la leyenda
  scale_color_manual(values = c("IPC Nivel general" = "steelblue",
                                "Dolar" = "darkgreen")) +
  theme_bw()

# otra forma

## Paso 1: Transformar a formato largo

ipc_tidy_gral <- ipc_dolar_gral %>%
  pivot_longer(
    cols = c(IPC_nivel_general, indice_dolar),
    names_to = "tipo_indice",
    values_to = "valor"
  )

## Paso 2: Graficar con ggplot y colores según tipo_indice

ggplot(ipc_tidy_gral, aes(x = fecha, y = valor, color = tipo_indice)) +
  geom_line() +
  labs(x = "Fecha", y = "Indice", color = "Variable", title="Evolución del IPC y Dolar")+
  scale_colour_manual(values=c("darkgreen", "steelblue"))+
  theme_bw()

## 5) ------------------------------------------

# Graficar la evolución del indice del dólar y 
# de los 4 grupos de IPC

ipc_dolar <- ipc_dolar %>% rename(indice_ipc = indice)

ggplot(ipc_dolar, aes(x = fecha)) +
  geom_line(aes(y = indice_ipc,   color = "IPC")) +
  geom_line(aes(y = indice_dolar, color = "Dólar")) +
  facet_wrap(~ Apertura) +
  labs(x = "Fecha", y = "Índice",
       title = "Evolución del IPC y Dólar por grupo",
       color = "Variable") +
  scale_color_manual(values = c("IPC" = "steelblue",
                                "Dólar" = "darkgreen")) +
  theme_bw()


# otra opcion

## Paso 1: Transformar a formato largo

ipc_dolar_tidy <- ipc_dolar %>%
  pivot_longer(
    cols = c(indice_ipc, indice_dolar),
    names_to = "tipo_indice",
    values_to = "valor"
  )

## Paso 2: Graficar con ggplot y colores según tipo_indice
ggplot(ipc_dolar_tidy, aes(x = fecha, y = valor, color = tipo_indice)) +
  geom_line() +
  labs(x="Fecha", y="Indice", color = "Variable", title="Evolución del IPC y Dolar")+
  scale_colour_manual(values=c("darkgreen", "steelblue"))+
  theme_bw() +
  facet_wrap(~Apertura)


# Ejercicio 2: trabajo con string y fechas -------------------------------------

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




