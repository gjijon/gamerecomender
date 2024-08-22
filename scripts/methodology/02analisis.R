################################################################################
# Análisis de datos
#
# 
#
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 26/09/2021
# Actualizacion: 26/09/2021
################################################################################

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

dataset_2_calificaciones <- dataset_raw
dataset_2_calificaciones <- subset( dataset_2_calificaciones, select = -c(1, 23:27))
nuevasColumnas <- c("spanish","english","german","latam", "si_recomendado", "no_recomendado")
dataset_2_calificaciones [ , nuevasColumnas] <- NA

dataset_2_calificaciones$spanish <- ifelse(dataset_2_calificaciones$idioma == "spanish", 1, 0)
dataset_2_calificaciones$english <- ifelse(dataset_2_calificaciones$idioma == "english", 1, 0)
dataset_2_calificaciones$german <- ifelse(dataset_2_calificaciones$idioma == "german",  1, 0)
dataset_2_calificaciones$latam <- ifelse(dataset_2_calificaciones$idioma == "latam",   1, 0)
dataset_2_calificaciones$si_recomendado <- ifelse(dataset_2_calificaciones$recomendado > 0, 1, 0)
dataset_2_calificaciones$no_recomendado <- ifelse(dataset_2_calificaciones$recomendado < 1, 1, 0)

dataset_2_calificaciones <- subset( dataset_2_calificaciones, select = -c(11:13, 21))
dataset_2_calificaciones = dataset_2_calificaciones %>% mutate_if(is.character, as.factor)


dataset_2_calificaciones <- ddply(dataset_2_calificaciones, c("id", "nombre", "precio", "win", "lin", "mac", "historia", "rapido", 
                                                              "ajustable", "metacritic", "precioMasAlto", "precioMasAltoFecha", "precioMasBajo", "precioMasBajoFecha", "primerPrecio", "primerPrecioFecha", "contadorRebajas"), function(x) colSums(x[c("spanish","english","german","latam", "si_recomendado", "no_recomendado")]))

dataset_2_calificaciones$win <- as.factor(dataset_2_calificaciones$win)
dataset_2_calificaciones$lin <- as.factor(dataset_2_calificaciones$lin)
dataset_2_calificaciones$mac <- as.factor(dataset_2_calificaciones$mac)
dataset_2_calificaciones$historia <- as.factor(dataset_2_calificaciones$historia)
dataset_2_calificaciones$rapido <- as.factor(dataset_2_calificaciones$rapido)
dataset_2_calificaciones$ajustable <- as.factor(dataset_2_calificaciones$ajustable)

dataset_2_calificaciones$id = paste0("Id_", dataset_2_calificaciones$id)

str(dataset_2_calificaciones)
names(dataset_2_calificaciones)
status(dataset_2_calificaciones)
profiling_num(dataset_2_calificaciones)
freq(dataset_2_calificaciones)
plot_num(dataset_2_calificaciones)

glimpse(dataset_raw, width = 70)


plotar(dataset_2_calificaciones, input = "precio", target = "si_recomendado", plot_type = "boxplot")
plotar(dataset_2_calificaciones, input = "historia", target = "si_recomendado", plot_type = "boxplot")

correlacion = cor(dataset_2_calificaciones [, 3:23], use = "pairwise.complete.obs")
corrplot::corrplot(correlacion, method = "ellipse", type = "upper")

importancia_variables = var_rank_info(dataset_2_calificaciones, "si_recomendado")

