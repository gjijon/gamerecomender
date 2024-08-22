################################################################################
# Datos
#
# Generación de dataframes
#
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 11/11/2021
# Actualizacion: 26/09/2021
################################################################################

library(ggplot2)
library(funModeling)
library(plyr)
library(dplyr)

setwd("../utils")
source("01funciones.R")

setwd("../../data/processed_data")
dataset_raw <- read.csv("05_dataset.csv")

colsLogicas <- sapply(dataset_raw, is.logical)
dataset_raw[,colsLogicas] <- lapply(dataset_raw[,colsLogicas], as.numeric)
dataset_raw$precioMasAltoFecha <- as.Date(dataset_raw$precioMasAltoFecha, "%Y-%m-%d")
dataset_raw$precioMasBajoFecha <- as.Date(dataset_raw$precioMasBajoFecha, "%Y-%m-%d")
dataset_raw$primerPrecioFecha <- as.Date(dataset_raw$primerPrecioFecha, "%Y-%m-%d")


#
# GENERACION DF DE JUEGOS SIMPLIFICADO
#
dfJuegos <- dataset_raw
dfJuegos <- subset( dfJuegos, select = -c(1, 23:27))
nuevasColumnas <- c("spanish","english","german","latam", "si_recomendado", "no_recomendado")
dfJuegos [ , nuevasColumnas] <- NA

dfJuegos$spanish <- ifelse(dfJuegos$idioma == "spanish", 1, 0)
dfJuegos$english <- ifelse(dfJuegos$idioma == "english", 1, 0)
dfJuegos$german <- ifelse(dfJuegos$idioma == "german",  1, 0)
dfJuegos$latam <- ifelse(dfJuegos$idioma == "latam",   1, 0)
dfJuegos$si_recomendado <- ifelse(dfJuegos$recomendado > 0, 1, 0)
dfJuegos$no_recomendado <- ifelse(dfJuegos$recomendado < 1, 1, 0)

dfJuegos <- subset( dfJuegos, select = -c(11:13, 21))
dfJuegos = dfJuegos %>% mutate_if(is.character, as.factor)


dfJuegos <- ddply(dfJuegos, c("id", "nombre", "precio", "win", "lin", "mac", "historia", "rapido", 
                              "ajustable", "metacritic", "precioMasAlto", "precioMasAltoFecha", "precioMasBajo",
                              "precioMasBajoFecha", "primerPrecio", "primerPrecioFecha", "contadorRebajas"), 
                  function(x) colSums(x[c("spanish","english","german","latam", "si_recomendado", "no_recomendado")]))

#
# CORRELACION
#
correlacion = cor(select(dfJuegos, 3:11, 13, 15, 17:23), use = "pairwise.complete.obs")
corrplot::corrplot(correlacion, method = "color", type = "upper")

dfJuegos$win <- as.factor(dfJuegos$win)
dfJuegos$lin <- as.factor(dfJuegos$lin)
dfJuegos$mac <- as.factor(dfJuegos$mac)
dfJuegos$historia <- as.factor(dfJuegos$historia)
dfJuegos$rapido <- as.factor(dfJuegos$rapido)
dfJuegos$ajustable <- as.factor(dfJuegos$ajustable)

dfJuegos$id = paste0("gid_", dfJuegos$id)


#
# GENERACION DE DF DE CALIFICACIONES
#
dfCalificaciones <- dataset_raw
dfCalificaciones <- subset( dfCalificaciones, select = -c(1))
nuevasColumnas <- c("spanish","english","german","latam", "uid")
dfCalificaciones [ , nuevasColumnas] <- NA

dfCalificaciones$spanish <- ifelse(dfCalificaciones$idioma == "spanish", 1, 0)
dfCalificaciones$english <- ifelse(dfCalificaciones$idioma == "english", 1, 0)
dfCalificaciones$german <- ifelse(dfCalificaciones$idioma == "german",  1, 0)
dfCalificaciones$latam <- ifelse(dfCalificaciones$idioma == "latam",   1, 0)

dfCalificaciones = dfCalificaciones %>% mutate_if(is.character, as.factor)

dfCalificaciones <- subset( dfCalificaciones, select = -c(12))
dfCalificaciones <- agregaIdsAleatoriosDeUsuario(dfCalificaciones)

#
# An�lisis
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


write.csv(dfCalificaciones, "06_dataset_calificaciones.csv", row.names = TRUE)
write.csv(dfJuegos, "07_dataset_juegos.csv", row.names = TRUE)



