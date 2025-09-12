---
cover:
    title: Hola

templates: [nice, custom_template]

config:
  add_footer: false

  text.font_size: 35
  math.font_size: 35

  errors.verbose: false
  only_calculate_new_slides: true
---

# Tratamiento de datos
## Encuesta Permanente de Hogares

>! add image ("figures/eph_pdf.png", width="100%")

[`https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos`](color=DARKER_GRAY,font_size=28)

# notitle

Antes de empezar, una aclaración:
>! pause

Todas las conclusiones que se puedan llegar a desprender de este análisis deben interpretarse con cautela. Se trata únicamente de un ejercicio pedagógico cuyo propósito es ilustrar un posible flujo de trabajo con datos reales. [-](font_size=30,color="RED")

# notitle

Ahora sí, **empecemos**

# Tratamiento de datos
## Librerías

```r (numbers=False)
 # para hacer todo lo que aprendieron la clase pasada
library(tidyverse)
```

# Tratamiento de datos
## Librerías

```r (numbers=False)
 # para hacer todo lo que aprendieron la clase pasada
library(tidyverse)

# para hacer todo lo que aprendieron la clase pasada,
# aún con objetos de R nativo
library(broom)
```

# Tratamiento de datos
## Lectura de datos
```r
df <- read_delim(
  file = "usu_individual_T125.txt",
  delim = ";",
)
```
>! add image ("figures/table_raw.pdf", width="100%")
```r
nrow(df)
```
```fortran  (numbers=False)
[1] 45.425
```

# Tratamiento de datos
## Variables de interés
```r
df <- df %>%
  rename(
    edad = CH06,
    salario = P21
    # ...
  )
```
>! `self.add_image("figures/edad_ch06.png", width=8, box="floating").shift(2.3*RIGHT+0.7*DOWN)`

# Tratamiento de datos
## Filtros
Nos quedamos con la gente que tiene algún trabajo (formal o informal)
```r
df_filt <- df %>%
  filter(
    ESTADO == 1,
    salario > 0,
  )
```
>! vspace (0.6)

Podemos ver cuánto filtramos comparando
```r
nrow(df)
```
```fortran (numbers=False, color="RED")
[1] 45425
```
```r
nrow(df_filt)
```
```fortran (numbers=False, color="RED")
[1] 15802
```

# notitle
**Tenemos los datos, ahora queremos [graficarlos](color=RED)**

# Gráficos
## GGPlot
>! vspace
```r
ggplot(df_filt,                 # dataframe con los datos
  aes(x = edad, y = salario))   # qué vars del df quiero mapear
```

# Gráficos
## GGPlot
>! vspace
```r
ggplot(df_filt,                 # dataframe con los datos
  aes(x = edad, y = salario)) + # qué vars del df quiero mapear
  geom_point(alpha = 0.2)       # representa las vars con puntos
```

# Gráficos
## GGPlot
>! vspace
```r
ggplot(df_filt,                 # dataframe con los datos
  aes(x = edad, y = salario)) + # qué vars del df quiero mapear
  geom_point(alpha = 0.2) +     # representa las vars con puntos
  labs(                         # modificá los labels
    x = "Edad",
    y = "Salario",
  )
```

# Gráficos con GGPlot
## Introducción
>! vspace
```r
ggplot(df_filt,                 # dataframe con los datos
  aes(x = edad, y = salario)) + # qué vars del df quiero mapear
  geom_point(alpha = 0.2) +     # representa las vars con puntos
  labs(                         # modificá los labels
    x = "Edad",
    y = "Salario",
  ) +
  theme_minimal()               # que sea más limpio
```


# Gráficos con GGPlot
## Edad vs Salario
>! add image ("figures/edad_salario_raw.pdf", width="100%")

# Gráficos con GGPlot
## Exploración gráfica
>! vspace
```r
ggplot(df_filt,                 # dataframe con los datos
  aes(x = edad, y = salario)) + # qué vars del df quiero mapear
  geom_point(alpha = 0.2) +     # representa la vars con puntos
  labs(                         # modificá los labels
    x = "Edad",
    y = "Salario",
  ) +
  ylim(0, 5000000) +            # solo salarios menores a 5M
  theme_minimal()               # que sea más limpio
```

# Graficos con GGPlot
## Exploración gráfica
>! add image ("figures/edad_salario_ylim.pdf", width="100%")

# En busca de la correlación
## Edad vs salario promedio
```r
edad_salario <- df_filt %>%
```

# En busca de la correlación
## Edad vs salario promedio
```r
edad_salario <- df_filt %>%
  # Agrupo todas las filas por edades
  group_by(edad) %>%
```

# En busca de la correlación
## Edad vs salario promedio
```r
edad_salario <- df_filt %>%
  # Agrupo todas las filas por edades
  group_by(edad) %>%

  # Calculo el salario promedio de cada grupo
  summarise(salario_prom = mean(salario)) %>%
```

# En busca de la correlación
## Edad vs salario promedio
```r
edad_salario <- df_filt %>%
  # Agrupo todas las filas por edades
  group_by(edad) %>%

  # Calculo el salario promedio de cada grupo
  summarise(salario_prom = mean(salario)) %>%

  # Desarmo el grupo
  ungroup() %>%
```

# En busca de la correlación
## Edad vs salario promedio
```r
edad_salario <- df_filt %>%
  # Agrupo todas las filas por edades
  group_by(edad) %>%

  # Calculo el salario promedio de cada grupo
  summarise(salario_prom = mean(salario)) %>%

  # Desarmo el grupo
  ungroup() %>%

  # Ordeno el df por edad de forma creciente
  arrange(edad)
```

# En busca de la correlación
## Edad vs salario promedio
```r
# Calculo el salario promedio por edad
edad_salario <- df_filt %>%
  group_by(edad) %>%
  summarise(salario_prom = mean(salario)) %>%
  ungroup() %>%
  arrange(edad)
```

# En busca de la correlación
## Edad vs salario promedio
Graficamos igual que antes:
>! vspace (0.4)

```r
ggplot(edad_salario, aes(x = edad, y = salario_prom)) +
  geom_point(alpha = 0.8) +
  labs(
    x = "Edad",
    y = "Salario promedio",
  ) +
  theme_minimal()
```

# En busca de la correlación
## Edad vs salario promedio
>! add image ("figures/edad_salario_prom.pdf", width="100%")

# En busca de la correlación
## Edad vs salario promedio
Filtrando por edad nos queda... [-](color="BLUE")
>! vspace (0.3)

```r
# Me quedo las personas entre 18 y 40 años que sean asalariadas
df_filt_18_40 <- df %>%
  filter(
    salario > 0,
    ESTADO == 1,
    between(edad, 18, 40)
  )
# Calculo el promedio de su salario por edad
edad_salario_18_40 <- df_filt_18_40 %>%
  group_by(edad) %>%
  summarise(salario_prom = mean(salario)) %>%
  ungroup() %>%
  arrange(edad)
```

# En busca de la correlación
## Edad vs salario promedio
```r
# Grafico el salario promedio vs la edad
ggplot(edad_salario_18_40, aes(x = edad, y = salario_prom)) +
  geom_point(alpha = 0.8) +
  labs(
    x = "Edad",
    y = "Salario promedio",
  ) +
  theme_minimal()
```

# En busca de la correlación
## Edad vs salario promedio
>! add image ("figures/edad_salario_prom_18_40.pdf", width="100%")

# Modelo lineal simple
## Introducción al modelo lineal simple
Viendo los datos resulta razonable proponer un modelo lineal:
$$
Y = \beta_0 + \beta_1 X + \epsilon
$$
donde $\beta_i$ son los parámetros de nuestro modelo y $\epsilon$ el error.

O bien $Y$ es una var. aleatoria tal que su esperanza condicionada a $X$ está data por 
$$
\text{E}(Y|X) = \beta_0 + \beta_1 X
$$
>! vspace (0.7)

En cualquier caso, dados pares de datos $(x_i,y_i)$ , los _mejores_ estimadores para $\beta_0$ y $\beta_1$ que podemos construir son

$$
\hat\beta_1 = \frac{\sum_{i=1}^n (x_i - \overline x) (y_i - \overline y)}{\sum_{i=1}^n(x_i - \overline x)^2}, \quad
\hat\beta_0 = \overline y - \beta_1\\,\overline x
$$

# Modelo lineal simple
## Modelo lineal a mano
En nuestro código esta cuenta
>! vspace (0.4)

$$
\hat\beta_1 = \frac{\sum_{i=1}^n (x_i - \overline x) (y_i - \overline y)}{\sum_{i=1}^n(x_i - \overline x)^2}, \quad
\hat\beta_0 = \overline y - \beta_1\\,\overline x
$$
se escribe
>! vspace (0.4)
```r
x_i <- edad_salario_18_40$edad         # [18, 19, ..., 40]
y_i <- edad_salario_18_40$salario_prom # [400K, 300K, ..., 800K]
# ...
```

# Modelo lineal simple
## Modelo lineal a mano
En nuestro código esta cuenta
>! vspace (0.4)

$$
\hat\beta_1 = \frac{\sum_{i=1}^n (x_i - \overline x) (y_i - \overline y)}{\sum_{i=1}^n(x_i - \overline x)^2}, \quad
\hat\beta_0 = \overline y - \beta_1\\,\overline x
$$
se escribe
>! vspace (0.4)
```r
x_i <- edad_salario_18_40$edad         # [18, 19, ..., 40]
y_i <- edad_salario_18_40$salario_prom # [400K, 300K, ..., 800K]
x_prom <- mean(x_i)
y_prom <- mean(y_i)
# ...
```

# Modelo lineal simple
## Modelo lineal a mano
En nuestro código esta cuenta
>! vspace (0.4)

$$
\hat\beta_1 = \frac{\sum_{i=1}^n (x_i - \overline x) (y_i - \overline y)}{\sum_{i=1}^n(x_i - \overline x)^2}, \quad
\hat\beta_0 = \overline y - \beta_1\\,\overline x
$$
se escribe
>! vspace (0.4)
```r
x_i <- edad_salario_18_40$edad         # [18, 19, ..., 40]
y_i <- edad_salario_18_40$salario_prom # [400K, 300K, ..., 800K]
x_prom <- mean(x_i)
y_prom <- mean(y_i)
b1 <- sum((x_i - x_prom) * (y_i - y_prom))/sum((x_i - x_prom)^2)
b0 <- y_prom - b1 * x_prom
```

# Modelo lineal simple
## Modelo lineal a mano
En nuestro código esta cuenta
>! vspace (0.4)

$$
\hat\beta_1 = \frac{\sum_{i=1}^n (x_i - \overline x) (y_i - \overline y)}{\sum_{i=1}^n(x_i - \overline x)^2}, \quad
\hat\beta_0 = \overline y - \beta_1\\,\overline x
$$
se escribe
>! vspace (0.4)
```r
x_i <- edad_salario_18_40$edad         # [18, 19, ..., 40]
y_i <- edad_salario_18_40$salario_prom # [400K, 300K, ..., 800K]
x_prom <- mean(x_i)
y_prom <- mean(y_i)
b1 <- sum((x_i - x_prom) * (y_i - y_prom))/sum((x_i - x_prom)^2)
b0 <- y_prom - b1 * x_prom
cat(b0, b1)
```
```fortran (numbers=False, color="RED")
>>> 14446  21726
```

# Modelo lineal simple
## Modelo lineal en R

Podemos pedirle `R` _que haga la cuenta por nosotros_
>! pause
```r
mod <- lm(
  formula = salario_prom ~ edad,
  data = edad_salario_18_40
)
mod
```
>! pause
```fortran (numbers=False, color="RED")
Coefficients:
(Intercept)         edad  
      14446        21726  
```
>! vspace (0.5)
>! pause

```r
b0 = mod$coefficients[1]
b1 = mod$coefficients[2]
cat(b0, b1)
```
```fortran (numbers=False, color="RED")
>>> 14446  21726
```

# Modelo lineal simple
## Datos + LM
```r
ggplot(edad_salario_18_40, aes(x = edad, y = salario_prom)) +
  geom_point(alpha = 0.8) +
  labs(
    x = "Edad",
    y = "Salario promedio",
  ) +
  geom_function(fun = \(x) b0 + b1*x, # Agregamos una recta
    linewidth = 1.5, col = "#C95E61"  # b0 + b1*edad
  ) +
  theme_minimal()
```

# Modelo lineal simple
## Datos + LM
```r
ggplot(edad_salario_18_40, aes(x = edad, y = salario_prom)) +
  geom_point(alpha = 0.8) +
  labs(
    x = "Edad",
    y = "Salario promedio",
  ) +
  geom_abline(intercept = b0, slope = b1,  # También se puede
    linewidth=1.5, col = "#C95E61"         # hacer así
  ) +
  theme_minimal()
```


# Modelo lineal simple
## Datos + LM
>! add image ("figures/edad_salario_prom_and_lm.pdf", width="100%")

# Modelo lineal simple
## Datos + LM
>! add image ("figures/edad_salario_prom_and_lm_ext_lims.pdf", width="100%")

# Modelo lineal simple
## Predicciones del modelo
Recordemos que 
$$
E(Y|X) = \beta_0 + \beta_1 X
$$
>! vspace
>! pause

Lo que quiere decir que el modelo me puede decir cuánto predice que es el sueldo promedio (Y) para una persona de determinada edad (X):
$$
E(~\text{sueldo}~|~\text{edad}~) = 14446 + 21726 ~\text{edad}
$$
>! vspace
>! pause

_ej:_ $E(~\text{sueldo}~|~35~) = 14446 + 21726 \cdot 35 = \\$774844$

# Modelo lineal simple
## Predicciones (y limitaciones) del modelo
>! vspace (0.6)
```r (font_size=38)
library(glue)

glue("Alguien con 35 años gana en promedio ${round(b0 + 35 * b1)}")
glue("Alguien recién nacido gana en promedio ${round(b0 + 0 * b1)}")
glue("Alguien con 90 años gana en promedio ${round(b0 + 90 * b1)}")
```
```fortran (numbers=False, color="RED",font_size=38)
>>> Alguien con 35 años gana en promedio $774844
```

# Modelo lineal simple
## Predicciones (y limitaciones) del modelo
>! vspace (0.6)
```r (font_size=38)
library(glue)

glue("Alguien con 35 años gana en promedio ${round(b0 + 35 * b1)}")
glue("Alguien recién nacido gana en promedio ${round(b0 + 0 * b1)}")
glue("Alguien con 90 años gana en promedio ${round(b0 + 90 * b1)}")
```
```fortran (numbers=False, color="RED",font_size=38)
>>> Alguien con 35 años gana en promedio $774844
>>> Alguien recién nacido gana en promedio $14446
>>> Alguien con 90 años gana en promedio $1969754
```

# notitle
**Esto es todo.**
