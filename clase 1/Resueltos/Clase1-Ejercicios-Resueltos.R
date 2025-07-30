#### Resolucion de Ejercicios - Clase 1 ####

rm(list = ls()) # borramos todos los objetos del entorno de trabajo

# Ejercicio 1 ------------------------------
 
## 1) Crear un dataframe con el dataset de R: state.x77 ---

class(state.x77)
?state.x77

df <- as.data.frame(state.x77)
class(df)


### a) ¿Cual es la poblacion total de Estados Unidos? --------

names(df)

# [1] "Population" "Income"     "Illiteracy" "Life.Exp"  
# [5] "Murder"     "HS.Grad"    "Frost"      "Area" 

sum(df$Population)

### b) ¿Cual es la media de la expectativa de vida? ------------

mean(df$Life.Exp)

### c) ¿Cual es la mediana del ingreso en pesos argentinos? ------------

median(df$Income) * 1308
# el resultado dependera de la cotizacion del dolar que se emplee



## 2) Crear el dataset con las dos columnas --------------

df_indice <- df[,c(3, 5)]
df_indice

### a) Crear el indice ----------------

ilit_murd <- df_indice$Illiteracy + df_indice$Murder

### b) Buscar los maximos y minimos ----------------


# valor maximo de ilit_murd

max(ilit_murd) 

# valor minimo de ilit_murd

min(ilit_murd) 

# si quiero saber a qué estado/s corresponden

row.names(df)[ilit_murd == max(ilit_murd)]

row.names(df)[ilit_murd == min(ilit_murd)]

## 3) Objetos ------------------------

### a) Un VALOR llamado _OBJETO_ definido como el resultado de la suma: 5+6 -----

OBJETO <- 5+6
OBJETO

### b) un VECTOR _VEC0_ que contenga una muestra aleatoria de numeros del 1 al 10. -------

VEC0 <- sample(1:10)
VEC0

### c) 3 vectores ( _VEC1_, _VEC2_, _VEC3_) que sean transformaciones del anterior. --------

VEC1 <- VEC0*2 
VEC2 <- VEC0^2 
VEC3 <- VEC0-2 

### d) 3 vectores con la misma cantidad de elementos que VEC0, pero con variables string (texto) ( _VEC4_, _VEC5_, _VEC6_) -----

# una opción posible es...

VEC4 <- c(rep("NO", 5), rep("SI", 5))
VEC5 <- c(rep("PAGO", 7), rep("LIBRE", 3))
VEC6 <- c(rep("SAS", 2), rep("SPSS", 2), rep("R", 6))

### e) Un dataframe _DFRAME_ como combinacion de todos los vectores creados previamente -------
DFRAME <- data.frame(VEC0, VEC1, VEC2, VEC3, VEC4, VEC5, VEC6)
DFRAME

### f) Una lista _LA_LISTA_ con el OBJETO, uno de los vectores y el DFRAME ------

LALISTA <- list(OBJETO, VEC0, DFRAME) 
LALISTA

## 4) Loops --------------------

### a) ---------------------------

# Para todos los valores del vector _VEC0_, imprimir mediante un loop 
# el triple de dichos valores

for(i in VEC0){
  print(i*3)
}


### b) -------------------------------------------


for (i in unique(DFRAME$VEC6)){
  texto <- paste(i, DFRAME$VEC0[DFRAME$VEC6 == i])
  print(texto)
}

### c) ---------------------------------


n <- length(DFRAME$VEC1)

for (i in 1:n){ 
  if (DFRAME$VEC0[i] > 2){
    DFRAME$VEC1[i] <- DFRAME$VEC0[i] * 2
  } else {
    DFRAME$VEC1[i] <- DFRAME$VEC0[i]
  }
}

# otra opcion

DFRAME$VEC1 <- ifelse(DFRAME$VEC0 > 2, DFRAME$VEC0 * 2, DFRAME$VEC0)


## 5) Funciones --------------------------------------

### a) --------------------------------------


Hola_Mundo <- function(){
  print("Hola mundo")
}

# la probamos

Hola_Mundo()

### b) --------------------------------------

Sumatoria_enteros <- function(x){
  y <- 1:x 
  sum(y)
}

# la probamos

Sumatoria_enteros(3)
Sumatoria_enteros(10)

### c) --------------------------------------

# Crear una funcion primer_elem_matriz cuyo parametro/input X sea una matrix
# y que devuelva: 
# la dimension de la matriz en cuestion y un texto que diga 
# "El primer elemento es par" en caso de que asi lo fuera o 
# "El primer elemento no es par" en caso contrario.


primer_elem_df <- function(X) {
  # Obtengo las dimensiones
  print(dim(df))
  # Chequeo si el primer elemento es par
  primer_elem <- X[1,1]
  # indico que imprima el texto en caso de cumplirse la condicion
  if (primer_elem %% 2 == 0) {
    print('El primer elemento es par')
  }
  else{print('El primer elemento no es par')}
}

# evaluo la funcion sobre el dataframe creado previamente
primer_elem_matriz(DFRAME)



# Ejercicio 2 --------------------------------------

## 1) --------------------------------------

individual_T122 <- read.table("datos/usu_individual_T122.txt", 
                              sep=";", 
                              dec=",",
                              header = TRUE)


### b) Visualizar el contenido --------------------------------------

str(individual_T122)


## 2) ------------------------------------------------------------------

sumar_columna <- function(df, x) {
  if (!is.numeric(df[[x]])) {
    stop("La columna especificada no es numérica.")
  }
  suma_total <- sum(df[[x]], na.rm = TRUE)
  return(suma_total)
}


### a)  -----------------------

sumar_columna(individual_T122, "PONDERA")
# 29073215

# chequeo el resultado
sum(individual_T122$PONDERA)
# 29073215 (bien)

# veamos qué pasa si le paso como segundo argumento una variable que no es numerica
sumar_columna(individual_T122, "CODUSU")

### b) -----------------------

df_m <- individual_T122[individual_T122$CH04 == 2,]
df_v <- individual_T122[individual_T122$CH04 == 1,]

sumar_columna(df_m, "PONDERA")
sumar_columna(df_v, "PONDERA")


### c) -----------------------

resumen_columna <- function(df, variable) {
  # Verificamos que la columna sea numérica
  if (!is.numeric(df[[variable]])) {
    stop("La columna especificada no es numérica.")
  }
  
  suma_columna <- sum(df[[variable]], na.rm = TRUE)
  n_observaciones <- nrow(df)
  
  return(data.frame(suma_columna, n_observaciones))
}

### d)  --------------

resumen_columna(individual_T122, "PONDERA")
#    suma_variable n_observaciones
# 1      29073215           49706

# chequeos

sum(individual_T122$PONDERA)
# 29073215

nrow(individual_T122)
# 49706

# todo bien!

### e) ------------

resumen_columna(df_m, "PONDERA")

resumen_columna(df_v, "PONDERA")

