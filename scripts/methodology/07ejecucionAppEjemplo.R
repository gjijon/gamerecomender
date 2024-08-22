################################################################################
# Ejecución de la aplicación
#
# 
#
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 27/12/2021
# Actualizacion: 04/12/2021
################################################################################

## importanción de datos
library(ggplot2)
library(plyr)
library(dplyr)
library(cluster)
library(recommenderlab)
library(pander)
library(shiny)
library(data.table)

set.seed(1)

setwd("../../data/processed_data")
dfCalificaciones <- read.csv("06_dataset_calificaciones.csv")
dfJuegos <- read.csv("07_dataset_juegos.csv")

dfCalificaciones$uid <- gsub('uid_', '', dfCalificaciones$uid)
dfCalificaciones$uid <- as.integer(dfCalificaciones$uid)

dfCalificaciones$id <- gsub('gid_', '', dfCalificaciones$id)
dfCalificaciones$id <- as.integer(dfCalificaciones$id)

dfJuegos$id <- gsub('gid_', '', dfJuegos$id)
dfJuegos$id <- as.integer(dfJuegos$id) 

setwd("../utils")
source("01funciones.R")

#
# GENERACION DE DF DE JUEGOS
#
dfJuegos$precioMasAltoFecha <- as.numeric(as.POSIXct(as.Date(dfJuegos$precioMasAltoFecha, "%Y-%m-%d")))
dfJuegos$precioMasBajoFecha <- as.numeric(as.POSIXct(as.Date(dfJuegos$precioMasBajoFecha, "%Y-%m-%d")))
dfJuegos$primerPrecioFecha <- as.numeric(as.POSIXct(as.Date(dfJuegos$primerPrecioFecha, "%Y-%m-%d")))

dfJuegos = dfJuegos %>% mutate_if(is.character, as.factor)

dfJuegos <- ddply(dfJuegos, c("id", "nombre", "precio", "win", "lin", "mac", "historia", "rapido", 
                              "ajustable", "metacritic", "precioMasAlto", "precioMasAltoFecha", "precioMasBajo",
                              "precioMasBajoFecha", "primerPrecio", "primerPrecioFecha", "contadorRebajas"), 
                  function(x) colSums(x[c("spanish","english","german","latam", "si_recomendado", "no_recomendado")]))

dfJuegos$metacritic[is.na(dfJuegos$metacritic)] <- 0
dfJuegos$metacritic[is.nan(dfJuegos$metacritic)] <- 0

dfJuegos$win <- as.factor(dfJuegos$win)
dfJuegos$lin <- as.factor(dfJuegos$lin)
dfJuegos$mac <- as.factor(dfJuegos$mac)
dfJuegos$historia <- as.factor(dfJuegos$historia)
dfJuegos$rapido <- as.factor(dfJuegos$rapido)
dfJuegos$ajustable <- as.factor(dfJuegos$ajustable)

#
# GENERACION DATOS NUEVOS
#

set.seed(1)
juegosAleatorios <- sample_n(dfJuegos, 2)
juegosAleatorios <- juegosAleatorios[, -(22:23)]

nombresJuegos <- getNombresJuegos(dfJuegos, juegosAleatorios$id)
pander(nombresJuegos)

juegosAleatorios <- juegosAleatorios[rep(seq_len(nrow(juegosAleatorios)), each = 2), ]

recc_data_new_users <- dfCalificaciones[0,]
recc_data_new_users <- merge(recc_data_new_users, juegosAleatorios, 
                             by=c("id","nombre", "precio", "win", "lin", "mac", "historia", "rapido", "ajustable", "metacritic", 
                                  "precioMasAlto", "precioMasAltoFecha", "precioMasBajo", "precioMasBajoFecha", "primerPrecio", 
                                  "primerPrecioFecha", "contadorRebajas", "spanish", "english", "german", "latam"), all=TRUE)

recc_data_new_users[1, c("uid", "fecha", "recomendado", "spanish", "english", "german", "latam")] <- c(500, "2021-12-27", 1, 0, 0, 0, 1)
recc_data_new_users[2, c("uid", "fecha", "recomendado", "spanish", "english", "german", "latam")] <- c(501, "2021-12-27", 0, 0, 0, 0, 1)
recc_data_new_users[3, c("uid", "fecha", "recomendado", "spanish", "english", "german", "latam")] <- c(500, "2021-12-27", 0, 0, 0, 0, 1)
recc_data_new_users[4, c("uid", "fecha", "recomendado", "spanish", "english", "german", "latam")] <- c(501, "2021-12-27", 1, 0, 0, 0, 1)


recc_data_new_users <- recc_data_new_users %>% select(X,id,uid,nombre,precio,win,lin,mac,historia,rapido,ajustable,metacritic,urlMetacritic,fecha,precioMasAlto,precioMasAltoFecha,precioMasBajo,precioMasBajoFecha,primerPrecio,primerPrecioFecha,contadorRebajas,recomendado,comunidad,comprado,contadorJuegosAdicional,tiempoJuego,tiempoJuegoReciente,spanish,english,german,latam)

dfCalificaciones <- rbind(recc_data_new_users, dfCalificaciones)
#
# GENERACION DE DF DE CALIFICACIONES
#

dfCalificaciones = dfCalificaciones %>% mutate_if(is.character, as.factor)

#
# Análisis
#
dfCalificaciones$win <- as.factor(dfCalificaciones$win)
dfCalificaciones$lin <- as.factor(dfCalificaciones$lin)
dfCalificaciones$mac <- as.factor(dfCalificaciones$mac)
dfCalificaciones$historia <- as.factor(dfCalificaciones$historia)
dfCalificaciones$rapido <- as.factor(dfCalificaciones$rapido)
dfCalificaciones$ajustable <- as.factor(dfCalificaciones$ajustable)
dfCalificaciones$spanish <- as.factor(dfCalificaciones$spanish)
dfCalificaciones$english <- as.factor(dfCalificaciones$english)
dfCalificaciones$german <- as.factor(dfCalificaciones$german)
dfCalificaciones$latam <- as.factor(dfCalificaciones$latam)

dfCalificaciones$metacritic[is.na(dfCalificaciones$metacritic)] <- 0
dfCalificaciones$metacritic[is.nan(dfCalificaciones$metacritic)] <- 0


calificacionesMatrix = crearMatrizReal(dfCalificaciones)
calificacionesMatrix %>% getRatings %>% hist(main = "Ratings")
calificacionesMatrix %>% normalize %>% getRatings %>% hist(main = "Rating normalizados por usuario")

#
# recomendadores
#
# ratings_juegos <- calificacionesMatrix[rowCounts(calificacionesMatrix) > 10,
#                                        colCounts(calificacionesMatrix) > 20]
ratings_juegos <- calificacionesMatrix
which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_juegos), replace = TRUE, prob = c(1, 0))
head(which_train)

recc_data_train <- ratings_juegos[which_train, ]
recc_data_test <- ratings_juegos[!which_train, ]

# which_set <- sample(x = 1:5, size = nrow(ratings_juegos), replace = TRUE)
# 
# for(i_model in 1:5) {
#   which_train <- which_set == i_model
#   recc_data_train <- ratings_juegos[which_train, ]
#   recc_data_test <- ratings_juegos[!which_train, ]
#   recc_data_train
# }

#
# IBCF
#

recc_modelIBCF <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 30))

dist_ratings <- as(recc_modelIBCF@model$sim, "matrix")

##calculo de peso
# primero juegos a datatable
dfJuegosTabla <- data.frame(dfJuegos)
dfJuegosTabla <- setDT(dfJuegosTabla)
dist_precio_30 <- dfJuegosTabla[, 1 - dist(precio <= 2000)]
dist_precio_30 <- as(dist_precio_30, "matrix")

dist_precio_60 <- dfJuegosTabla[, 1 - dist(precio > 2000)]
dist_precio_60 <- as(dist_precio_60, "matrix")

dist_historia <- dfJuegosTabla[, 1 - dist(historia == 1)]
dist_historia <- as(dist_historia, "matrix")

dist_rapido <- dfJuegosTabla[, 1 - dist(rapido == 1)]
dist_rapido <- as(dist_rapido, "matrix")

dist_ajustable <- dfJuegosTabla[, 1 - dist(ajustable == 1)]
dist_ajustable <- as(dist_ajustable, "matrix")

dist_rebajas <- dfJuegosTabla[, 1 - dist(contadorRebajas < 10)]
dist_rebajas <- as(dist_rebajas, "matrix")

dist_fecha <- dfJuegosTabla[, 1 - dist(primerPrecioFecha > as.numeric(as.POSIXct(as.Date("2016-01-01", "%Y-%m-%d"))))]
dist_fecha <- as(dist_fecha, "matrix")

rownames(dist_precio_30) <- dfJuegosTabla[, id]
colnames(dist_precio_30) <- dfJuegosTabla[, id]

rownames(dist_precio_60) <- dfJuegosTabla[, id]
colnames(dist_precio_60) <- dfJuegosTabla[, id]

rownames(dist_historia) <- dfJuegosTabla[, id]
colnames(dist_historia) <- dfJuegosTabla[, id]

rownames(dist_rapido) <- dfJuegosTabla[, id]
colnames(dist_rapido) <- dfJuegosTabla[, id]

rownames(dist_ajustable) <- dfJuegosTabla[, id]
colnames(dist_ajustable) <- dfJuegosTabla[, id]

rownames(dist_rebajas) <- dfJuegosTabla[, id]
colnames(dist_rebajas) <- dfJuegosTabla[, id]

rownames(dist_fecha) <- dfJuegosTabla[, id]
colnames(dist_fecha) <- dfJuegosTabla[, id]


#comprobacion nombres
vector_items <- rownames(dist_ratings)
dist_precio_30 <- dist_precio_30[vector_items, vector_items]
dist_precio_60 <- dist_precio_60[vector_items, vector_items]
dist_historia <- dist_historia[vector_items, vector_items]
dist_rapido <- dist_rapido[vector_items, vector_items]
dist_ajustable <- dist_ajustable[vector_items, vector_items]
dist_rebajas <- dist_rebajas[vector_items, vector_items]
dist_fecha <- dist_fecha[vector_items, vector_items]


#calculo de relacion de distancia
weight_precio_alto <- 0.04
weight_precio_bajo <- 0.06
weight_tipo_juego_historia  <- 0.05
weight_tipo_juego_rapido  <- 0.10
weight_tipo_juego_ajustable  <- 0.15
weight_rebajas  <- 0.10
weight_fecha  <- 0.05

dist_tot <- 
  dist_precio_30 * weight_precio_bajo + 
  dist_precio_60 * weight_precio_alto + 
  dist_historia  * weight_tipo_juego_historia +
  dist_rapido    * weight_tipo_juego_rapido + 
  dist_ajustable * weight_tipo_juego_ajustable + 
  dist_rebajas   * weight_rebajas + 
  dist_fecha     * weight_fecha + 
  dist_ratings * (1 - 0.55)

recc_modelIBCF@model$sim <- as(dist_tot, "dgCMatrix")

#
# UBCF
#

recc_modelUBCF <- Recommender(data = recc_data_train, method = "UBCF")

#
# EJECUTAR PREDICTORES
#

## <- crearMatrizReal(recc_data_new_users)

n_recommended <- 6

recc_predictedIBCF <- predict(object = recc_modelIBCF, 
                              newdata = recc_data_train, n = n_recommended)
recc_predictedUBCF <- predict(object = recc_modelUBCF, 
                              newdata = recc_data_train, n = n_recommended)

table_new_user_IBCF_500 <- presentaResultados(dfJuegos, recc_predictedIBCF, "500")
table_new_user_IBCF_501 <- presentaResultados(dfJuegos, recc_predictedIBCF, "501")

table_new_user_UBCF_500 <- presentaResultados(dfJuegos, recc_predictedUBCF, "500")
table_new_user_UBCF_501 <- presentaResultados(dfJuegos, recc_predictedUBCF, "501")

panderOptions("table.split.table", Inf) 
pander(table_new_user_IBCF_500)
pander(table_new_user_UBCF_500)
pander(table_new_user_IBCF_501)
pander(table_new_user_UBCF_501)


################################################################################
#
# nuevos juegos elegidos para usuario 500
#
################################################################################

nuevosJuegos <- c(595430, 1230660)

juegosCompradosRevisados <- retornaJuegoPorId(dfJuegos, nuevosJuegos)

nombresJuegos <- getNombresJuegos(dfJuegos, juegosCompradosRevisados$id)
pander(nombresJuegos)

recc_data_new_users <- dfCalificaciones[0,]
recc_data_new_users <- merge(recc_data_new_users, juegosCompradosRevisados, 
                             by=c("id","nombre", "precio", "win", "lin", "mac", "historia", "rapido", "ajustable", "metacritic", 
                                  "precioMasAlto", "precioMasAltoFecha", "precioMasBajo", "precioMasBajoFecha", "primerPrecio", 
                                  "primerPrecioFecha", "contadorRebajas", "spanish", "english", "german", "latam"), all=TRUE)

recc_data_new_users[1, c("uid", "fecha", "recomendado", "spanish", "english", "german", "latam")] <- c(500, "2021-12-27", 1, 0, 0, 0, 1)
recc_data_new_users[2, c("uid", "fecha", "recomendado", "spanish", "english", "german", "latam")] <- c(500, "2021-12-27", 1, 0, 0, 0, 1)


recc_data_new_users <- recc_data_new_users %>% select(X,id,uid,nombre,precio,win,lin,mac,historia,rapido,ajustable,metacritic,urlMetacritic,fecha,precioMasAlto,precioMasAltoFecha,precioMasBajo,precioMasBajoFecha,primerPrecio,primerPrecioFecha,contadorRebajas,recomendado,comunidad,comprado,contadorJuegosAdicional,tiempoJuego,tiempoJuegoReciente,spanish,english,german,latam)

dfCalificaciones <- rbind(recc_data_new_users, dfCalificaciones)

dfCalificaciones = dfCalificaciones %>% mutate_if(is.character, as.factor)

#
# Análisis
#
dfCalificaciones$win <- as.factor(dfCalificaciones$win)
dfCalificaciones$lin <- as.factor(dfCalificaciones$lin)
dfCalificaciones$mac <- as.factor(dfCalificaciones$mac)
dfCalificaciones$historia <- as.factor(dfCalificaciones$historia)
dfCalificaciones$rapido <- as.factor(dfCalificaciones$rapido)
dfCalificaciones$ajustable <- as.factor(dfCalificaciones$ajustable)
dfCalificaciones$spanish <- as.factor(dfCalificaciones$spanish)
dfCalificaciones$english <- as.factor(dfCalificaciones$english)
dfCalificaciones$german <- as.factor(dfCalificaciones$german)
dfCalificaciones$latam <- as.factor(dfCalificaciones$latam)

dfCalificaciones$metacritic[is.na(dfCalificaciones$metacritic)] <- 0
dfCalificaciones$metacritic[is.nan(dfCalificaciones$metacritic)] <- 0


calificacionesMatrix = crearMatrizReal(dfCalificaciones)
calificacionesMatrix %>% getRatings %>% hist(main = "Ratings")
calificacionesMatrix %>% normalize %>% getRatings %>% hist(main = "Rating normalizados por usuario")

#
# recomendadores
#
# ratings_juegos <- calificacionesMatrix[rowCounts(calificacionesMatrix) > 10,
#                                        colCounts(calificacionesMatrix) > 20]
ratings_juegos <- calificacionesMatrix
which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_juegos), replace = TRUE, prob = c(1, 0))
head(which_train)

recc_data_train <- ratings_juegos[which_train, ]
recc_data_test <- ratings_juegos[!which_train, ]


#
# IBCF
#

recc_modelIBCF <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 30))

dist_ratings <- as(recc_modelIBCF@model$sim, "matrix")

##calculo de peso
# primero juegos a datatable
dfJuegosTabla <- data.frame(dfJuegos)
dfJuegosTabla <- setDT(dfJuegosTabla)
dist_precio_30 <- dfJuegosTabla[, 1 - dist(precio <= 2000)]
dist_precio_30 <- as(dist_precio_30, "matrix")

dist_precio_60 <- dfJuegosTabla[, 1 - dist(precio > 2000)]
dist_precio_60 <- as(dist_precio_60, "matrix")

dist_historia <- dfJuegosTabla[, 1 - dist(historia == 1)]
dist_historia <- as(dist_historia, "matrix")

dist_rapido <- dfJuegosTabla[, 1 - dist(rapido == 1)]
dist_rapido <- as(dist_rapido, "matrix")

dist_ajustable <- dfJuegosTabla[, 1 - dist(ajustable == 1)]
dist_ajustable <- as(dist_ajustable, "matrix")

dist_rebajas <- dfJuegosTabla[, 1 - dist(contadorRebajas < 10)]
dist_rebajas <- as(dist_rebajas, "matrix")

dist_fecha <- dfJuegosTabla[, 1 - dist(primerPrecioFecha > as.numeric(as.POSIXct(as.Date("2016-01-01", "%Y-%m-%d"))))]
dist_fecha <- as(dist_fecha, "matrix")

rownames(dist_precio_30) <- dfJuegosTabla[, id]
colnames(dist_precio_30) <- dfJuegosTabla[, id]

rownames(dist_precio_60) <- dfJuegosTabla[, id]
colnames(dist_precio_60) <- dfJuegosTabla[, id]

rownames(dist_historia) <- dfJuegosTabla[, id]
colnames(dist_historia) <- dfJuegosTabla[, id]

rownames(dist_rapido) <- dfJuegosTabla[, id]
colnames(dist_rapido) <- dfJuegosTabla[, id]

rownames(dist_ajustable) <- dfJuegosTabla[, id]
colnames(dist_ajustable) <- dfJuegosTabla[, id]

rownames(dist_rebajas) <- dfJuegosTabla[, id]
colnames(dist_rebajas) <- dfJuegosTabla[, id]

rownames(dist_fecha) <- dfJuegosTabla[, id]
colnames(dist_fecha) <- dfJuegosTabla[, id]


#comprobacion nombres
vector_items <- rownames(dist_ratings)
dist_precio_30 <- dist_precio_30[vector_items, vector_items]
dist_precio_60 <- dist_precio_60[vector_items, vector_items]
dist_historia <- dist_historia[vector_items, vector_items]
dist_rapido <- dist_rapido[vector_items, vector_items]
dist_ajustable <- dist_ajustable[vector_items, vector_items]
dist_rebajas <- dist_rebajas[vector_items, vector_items]
dist_fecha <- dist_fecha[vector_items, vector_items]


#calculo de relacion de distancia
weight_precio_alto <- 0.04
weight_precio_bajo <- 0.06
weight_tipo_juego_historia  <- 0.05
weight_tipo_juego_rapido  <- 0.10
weight_tipo_juego_ajustable  <- 0.15
weight_rebajas  <- 0.10
weight_fecha  <- 0.05

dist_tot <- 
  dist_precio_30 * weight_precio_bajo + 
  dist_precio_60 * weight_precio_alto + 
  dist_historia  * weight_tipo_juego_historia +
  dist_rapido    * weight_tipo_juego_rapido + 
  dist_ajustable * weight_tipo_juego_ajustable + 
  dist_rebajas   * weight_rebajas + 
  dist_fecha     * weight_fecha + 
  dist_ratings * (1 - 0.55)

recc_modelIBCF@model$sim <- as(dist_tot, "dgCMatrix")

#
# UBCF
#

recc_modelUBCF <- Recommender(data = recc_data_train, method = "UBCF")

#
# EJECUTAR PREDICTORES
#

## <- crearMatrizReal(recc_data_new_users)

n_recommended <- 6

recc_predictedIBCF <- predict(object = recc_modelIBCF, 
                              newdata = recc_data_train, n = n_recommended)
recc_predictedUBCF <- predict(object = recc_modelUBCF, 
                              newdata = recc_data_train, n = n_recommended)

table_new_user_IBCF_500 <- presentaResultados(dfJuegos, recc_predictedIBCF, "500")
table_new_user_UBCF_500 <- presentaResultados(dfJuegos, recc_predictedUBCF, "500")

panderOptions("table.split.table", Inf) 
pander(table_new_user_IBCF_500)
pander(table_new_user_UBCF_500)



#promedios
library(ggplot2)
library(funModeling)
library(plyr)
library(dplyr)
library("Hmisc")
library(tidyr)

mean(dfJuegos$precio)

str(dfJuegos)
names(dfJuegos)
status(dfJuegos)
profiling_num(dfJuegos)
freq(dfJuegos)
plot_num(dfJuegos)
