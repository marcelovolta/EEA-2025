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


