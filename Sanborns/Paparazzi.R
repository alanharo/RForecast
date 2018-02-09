#############################################################
# PROCESO AJUSTE Y PRONÓSTICO CON EFECTO CALENDARIO         #
# PARA Líneas de la División  67-FOTOGRAFÍA                 #
# Autor: Alan Haro Escandón                                 #
# Fecha: 09/02/2018                                         #
# Sanborns                                                  #
#############################################################

## Se importan los datos para los Grupos, historia desde Enero 2016 hasta
## Diciembre 2017.
#
# Se agregan los paquetes que se van a utilizar:
t1 <- Sys.time()
library(matlib)
library(readr)
library(data.table)
library(lubridate)
## Matriz de Diseño para Día-Mes:

setwd("~/SANBORNS/Merchandise Analytics")
Datos_Cam <- read_excel("CamarasDatos.xlsx", col_types = c("numeric", "date", "numeric"))

# Transformamos nuestros datos de tal forma que se puedan manejar de una manera más 
# fácil. 
# 
# Primero transformamos los datos a tipo Data.Table, lo cual nos permitirá 
# manejarlos de manera más sencilla:

Datos <- as.data.table(Datos_Cam)

# Los ordenamos de acuerdo a la sección y después de acuerdo a la fecha.

Datos_sort <- Datos[order(Id_Linea, Id_Fecha)]

# Una vez tenemos los datos ordenados, obtenemos las distintas secciones en un
# vector:

Id_Linea <- unique(Datos_sort$Id_Linea)

# Repetimos el proceso para las fechas:

Fechas <- unique(Datos_sort$Id_Fecha)

# Agregamos las semanas a la tabla:

Datos_Linea <- Datos_sort # Creamos una nueva tabla

Datos_Linea[,Semana := as.numeric(format(Datos_sort$Id_Fecha, "%w"))] # Agregamos semanas
Datos_Linea$Semana[Datos_Linea$Semana == 0] <- 7  # Ponemos los domingos = 7

# Creamos una tabla para guardar los resultados de los Datos Ajustados:

Datos_ajustados <- Datos[1,]
Datos_ajustados$Id_Linea <- 0 # Le ponemos 0 al grupo para identificar 
# el grupo que después quitaremos.

# Creamos otra tabla para guardar los pronósticos (de Enero por el momento)

Datos_pronosticados <- Datos[1,]
Datos_pronosticados$Id_Linea <- 0 # Le ponemos 0 a la sección para identificar
# la sección que después quitaremos.

# Agregamos los indicadores para las fechas de Pronóstico (de Enero por 
# el momento)
fechas_FC <- as.POSIXlt(seq(as.Date("2018/1/1"),to = as.Date("2018/1/1")+364, 
                            by = "days"))
fechas_FC_dia <-  format(fechas_FC, format = "%m-%d")
fechas_semana <- as.numeric(format(fechas_FC, "%w"))
fechas_semana[fechas_semana == 0] <- 7


# prueba <- rbindlist(list(Datos_ajustados, Datos))
# Comenzamos con las iteraciones para cada Sección:



# Agregamos una versión simplificada de la tabla:
# Ajustamos la k manual aquí... sino ponemos un for...
# La k es para el Grupo


for (k in Id_Linea){
  
  # Agregamos una versión simplificada de la tabla:
  
  Datos_iter <- Datos_Linea[Id_Linea %in% k, .(Id_Fecha, Unidades, Semana)]
  
  # Nos aseguramos que las fechas se encuentren completas, esto es, en el caso en donde 
  # la venta fue igual a cero no se registró venta y desde SQL no se trae dicha fecha
  # por lo que hay que agregar la fecha y asignarle Unidades = 0.
  
  Fechas_faltantes <- Fechas[! Fechas %in% Datos_iter$Id_Fecha]
  Unidades_faltantes <- rep(0, length(Fechas_faltantes))
  Semanas_faltantes <- as.numeric(format(Fechas_faltantes, "%w")) # Agregamos semanas
  Semanas_faltantes[Semanas_faltantes == 0] <- 7
  
  # Juntamos las observaciones en una misma tabla, la tabla puede ser vacía:
  
  Datos_faltantes <- data.table(Fechas_faltantes, Unidades_faltantes, Semanas_faltantes)
  
  # Juntamos ambas tablas:
  
  Datos_iter <- rbindlist(list(Datos_iter, Datos_faltantes))
  
  # Volvemos a acomodar los datos:
  
  
  Datos_iter <- Datos_iter[order(Id_Fecha)]
  
  # Definimos la matriz de diseño a partir de los datos de Unidades Vendidas:
  
  X <- matrix(0, nrow = 731, ncol = 367)
  
  # La primera columna toma en cuenta el nivel y se llena con 1's.
  
  X[,1] <- 1
  
  # La segunda columna es una secuencia para la serie de tiempo 1:731
  
  X[,2] <- 1:length(X[,2])
  
  # Las demás columnas se llenan con 1's o 0's dependiendo del día al que pertenezcan
  # Para evitar la trampa de colinealidad en el diseño de la matriz, se omite el 29 de feb.
  # esto significa que el nivel estándar corresponde a dicho día.
  # Las fechas se deben transformar para considerar solamente el mes y el día, excluyendo 
  # el año.
  
  Fechas_md <- format(Fechas, format = "%m-%d")
  
  # Vamos a omitir el 29 de febrero, este será la fecha estándar, i. e., no se le va a 
  # sumar un ajuste en dicha fecha.
  
  fecha_om <- "02-29"
  fechas_col <- Fechas_md[! Fechas_md %in% fecha_om]
  
  # Creamos la matriz de diseño.
  for (i in 1:(length(X[2,])-2)) {
    
    X[,i+2] <- as.numeric(Fechas_md == fechas_col[i])
    
  }
  
  # Ahora se repite el proceso pero para los días de la semana. El domingo será el valor 
  # estándar para evitar la trampa de la colinealidad.
  
  S <- matrix(0, nrow = 731, ncol = 6)
  
  
  semana <- Datos_iter$Id_Semana
  
  
  for (i in 1:(length(S[1,]))) {
    
    S[,i] <- as.numeric(semana == i)
    
  }
  
  # Juntamos las matrices de Diseño para tener una matriz final.
  
  X_d <- cbind(X, S)
  
  # Calculamos los factores de ajuste: (X'*X)^-1*X'*y
  
  y <- Datos_iter$Unidades
  
  X_spd <- t(X_d)%*%X_d   #X'*X
  
  # Sacamos la descomposición de Cholesky para invertir (computacionalmente más estable).
  L <- t(chol(X_spd))
  L_inv <- solve(L)
  # Obtenemos la inversa de X'*X
  X_inv <- t(L_inv)%*%L_inv
  
  Beta <- X_inv%*%t(X_d)
  # Finalmente obtenemos los valores de los parámetros.
  
  Beta <- Beta%*%y
  
  # Los valores ajustados son los siguientes:
  
  y_hat <- X_d%*%Beta
  
  # Costeamos las unidades, tomando el Costo como el cociente de Inventarios.
  y_hat_costo <- y_hat*Inv_iter$Costo
  
  # Creamos una tabla para guardar los valores obtenidos del ajuste:
  
  Ajuste <- data.table(Id_Fecha = Fechas,
                       Id_Seccion = rep(k, length(Fechas)),
                       Unidades = y_hat,
                       CVenta = y_hat_costo)
  
  Datos_ajustados <- rbindlist(list(Datos_ajustados, Ajuste))
  
  # Ahora falta pronosticar los valores siguientes, en este caso solo lo haremos
  # para Enero.
  
  FC <- matrix(0, nrow = 59, ncol = 373)
  
  # Agregamos los valores para cada día:
  
  FC[,1] <- 1 # 1's para la primera columna
  FC[,2] <- (max(X[,2])+1):(max(X[,2])+length(FC[,2])) # Secuencia para la segunda
  # columna
  
  for (i in 1:(length(FC[2,])-(2+6))) {
    
    FC[,i+2] <- as.numeric(fechas_FC_dia == fechas_col[i])
    
  }
  
  
  # Agregamos los indicadores para el día de la Semana
  
  
  for (i in 1:6) {
    
    FC[,i+(length(FC[2,])-6)] <- as.numeric(fechas_semana == i)
    
  }
  
  # FC ya tiene los valores necesarios para pronosticar. Ahora solo falta 
  # obtener los valores del pronóstico como Y_fc <- FC*Beta
  
  y_fc <- FC%*%Beta
  
  
  flag <- sum(as.numeric(y_fc >= 0))
  
  if (flag == 0){
    y_fc <- exp(y_fc)
  } else {
    y_fc[y_fc < 0] <- 0
  }
  
  part_fc <- y_fc/sum(y_fc)
  
  y_fc[y_fc < 0.0001] <- 0
  part_fc[part_fc < 0.0001] <- 0
  y_fc_costo <- y_fc*Inv_iter$Costo
  # Creamos una tabla para guardar los valores obtenidos del pronóstico:
  
  Forecast <- data.table(Id_Fecha = fechas_FC,
                         Id_Seccion = rep(k, length(fechas_FC)),
                         Unidades = y_fc,
                         Participacion = part_fc,
                         CVenta = y_fc_costo)
  
  Datos_pronosticados <- rbindlist(list(Datos_pronosticados, Forecast))
  
  
}


write.csv(Datos_pronosticados, file = "Cigarros_cuadr.csv")

t2 <- Sys.time()

tiempo_proceso <- t2 - t1

tiempo_proceso

