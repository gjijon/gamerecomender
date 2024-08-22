################################################################################
# Completar datos
#
# Script para unir todos los datos
#
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 26/09/2021
# Actualizacion: 26/09/2021
################################################################################

library(dplyr)

setwd("../utils")
source("00bfunciones.R")

setwd("../../data/rawdata")

aplicacionesFinal <- read.csv("02_apps_details.csv")
revisionesFinal <- read.csv("04_apps_reviews_final.csv")
nuevasColumnas <- c("precioMasAlto","precioMasAltoFecha","precioMasBajo","precioMasBajoFecha","primerPrecio","primerPrecioFecha", "contadorRebajas")
revisionesFinal [ , nuevasColumnas] <- NA

for(row in 1:nrow(aplicacionesFinal)) {
  revisionesFinal$metacritic[revisionesFinal$id == aplicacionesFinal[row,2]] <- aplicacionesFinal[row, 11]
  revisionesFinal$urlMetacritic[revisionesFinal$id == aplicacionesFinal[row,2]] <- aplicacionesFinal[row, 12]
  
  datosDeJson <- procesaJsonPrecio(aplicacionesFinal[row, 2])
  
  if(!is.null(datosDeJson)) {
    revisionesFinal$precioMasAlto[revisionesFinal$id == aplicacionesFinal[row,2]] <- datosDeJson[[1]]
    revisionesFinal$precioMasAltoFecha[revisionesFinal$id == aplicacionesFinal[row,2]] <- datosDeJson[[2]]
    revisionesFinal$precioMasBajo[revisionesFinal$id == aplicacionesFinal[row,2]] <- datosDeJson[[3]]
    revisionesFinal$precioMasBajoFecha[revisionesFinal$id == aplicacionesFinal[row,2]] <- datosDeJson[[4]]
    revisionesFinal$primerPrecio[revisionesFinal$id == aplicacionesFinal[row,2]] <- datosDeJson[[5]]
    revisionesFinal$primerPrecioFecha[revisionesFinal$id == aplicacionesFinal[row,2]] <- datosDeJson[[6]]
    revisionesFinal$contadorRebajas[revisionesFinal$id == aplicacionesFinal[row,2]] <- datosDeJson[[7]]
  }
}

revisionesFinal <- mutate(revisionesFinal, fecha_review = as.Date(as.POSIXct(fecha, origin="1970-01-01")))

revisionesFinal <- mutate(revisionesFinal, precioMasAltoFecha = as.Date(as.POSIXct(precioMasAltoFecha/1000, origin="1970-01-01")))
revisionesFinal <- mutate(revisionesFinal, precioMasBajoFecha = as.Date(as.POSIXct(precioMasBajoFecha/1000, origin="1970-01-01")))
revisionesFinal <- mutate(revisionesFinal, primerPrecioFecha = as.Date(as.POSIXct(primerPrecioFecha/1000, origin="1970-01-01")))

revisionesFinal <- subset(revisionesFinal, select = c(1:13,28,14:27))

revisionesFinal <- subset(revisionesFinal, select = -c(1,15))

revisionesFinal <- subset(revisionesFinal, select = c(1:13,20:26,14:19))

names(revisionesFinal)[13] <- "fecha"


setwd("../../data/processed_data")

write.csv(revisionesFinal, "05_dataset.csv", row.names = TRUE)
