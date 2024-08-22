################################################################################
# Datos
#
# An·lisis b·sico
#
# Autor: Gabriel Jij√≥n, Quito - Ecuador
# Fecha: 11/11/2021
# Actualizacion: 26/09/2021
################################################################################

library(ggplot2)
library(funModeling)
library(plyr)
library(dplyr)
library("Hmisc")
library(tidyr)

setwd("../utils")
source("01funciones.R")

setwd("../../data/processed_data")
dfCalificaciones <- read.csv("06_dataset_calificaciones.csv")

dfCalificacionesNumeric <- data.frame(dfCalificaciones)
dfCalificacionesNumeric$id <- gsub('gid_', '', dfCalificacionesNumeric$id)
dfCalificacionesNumeric$id <- as.integer(dfCalificacionesNumeric$id)


dfJuegos <- read.csv("07_dataset_juegos.csv")

dfJuegos$precioMasAltoFecha <- as.numeric(as.POSIXct(as.Date(dfJuegos$precioMasAltoFecha, "%Y-%m-%d")))
dfJuegos$precioMasBajoFecha <- as.numeric(as.POSIXct(as.Date(dfJuegos$precioMasBajoFecha, "%Y-%m-%d")))
dfJuegos$primerPrecioFecha <- as.numeric(as.POSIXct(as.Date(dfJuegos$primerPrecioFecha, "%Y-%m-%d")))

dfJuegos = dfJuegos %>% mutate_if(is.character, as.factor)

dfJuegos <- ddply(dfJuegos, c("id", "nombre", "precio", "win", "lin", "mac", "historia", "rapido", 
                              "ajustable", "metacritic", "precioMasAlto", "precioMasAltoFecha", "precioMasBajo",
                              "precioMasBajoFecha", "primerPrecio", "primerPrecioFecha", "contadorRebajas"), 
                  function(x) colSums(x[c("spanish","english","german","latam", "si_recomendado", "no_recomendado")]))

str(dfJuegos)
names(dfJuegos)
status(dfJuegos)
profiling_num(dfJuegos)
freq(dfJuegos)
plot_num(dfJuegos)

str(dfJuegos)
summary(dfJuegos$win)
summary(dfJuegos$lin)
summary(dfJuegos$si_recomendado)
summary(dfJuegos$no_recomendado)


dfJuegos$metacritic[is.na(dfJuegos$metacritic)] <- 0
dfJuegos$metacritic[is.nan(dfJuegos$metacritic)] <- 0

grep(c("id","recomendado"), colnames(dfCalificaciones))

#
# CORRELACION
#
correlacionWin = cor(select(dfJuegos, 3:11, 13, 15, 17:23), use = "pairwise.complete.obs")
correlacionSinPlataforma = cor(select(dfJuegos, 3:3,7:11, 13, 15, 22:23), use = "pairwise.complete.obs")
correlacionUsuarios = cor(select(dfCalificacionesNumeric, 2:2, 22:31), use = "pairwise.complete.obs")

#cor_2 <- rcorr(as.matrix(dfJuegos[,-c(1:2)]), type = c("pearson","spearman"))
p.mat <- calculaFactoresCorrelacion(dfJuegos[,-c(1:2)])

corrplot::corrplot(correlacionWin, method = "color", type = "upper", tl.col="black", tl.srt=45, tl.cex = .70 )
corrplot::corrplot(correlacionSinPlataforma, method = "color", type = "upper", tl.col="black", tl.srt=45, tl.cex = .70)
corrplot::corrplot(correlacionUsuarios, method = "color", type = "upper", tl.col="black", tl.srt=45, tl.cex = .60)

dfJuegos$win <- as.factor(dfJuegos$win)
dfJuegos$lin <- as.factor(dfJuegos$lin)
dfJuegos$mac <- as.factor(dfJuegos$mac)
dfJuegos$historia <- as.factor(dfJuegos$historia)
dfJuegos$rapido <- as.factor(dfJuegos$rapido)
dfJuegos$ajustable <- as.factor(dfJuegos$ajustable)


#
# GENERACION DE DF DE CALIFICACIONES
#

dfCalificaciones = dfCalificaciones %>% mutate_if(is.character, as.factor)

#
# An·lisis
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

str(dfCalificaciones)
names(dfCalificaciones)
status(dfCalificaciones)
profiling_num(dfCalificaciones)
freq(c(dfCalificaciones$spanish, dfCalificaciones$english))
plot_num(dfCalificaciones)

dfCalificaciones$metacritic[is.na(dfCalificaciones$metacritic)] <- 0
dfCalificaciones$metacritic[is.nan(dfCalificaciones$metacritic)] <- 0

correlacion = cor(dfCalificaciones [, 3:23], use = "pairwise.complete.obs")
corrplot::corrplot(correlacion, method = "ellipse", type = "upper")

c4 = c("No Recomendado", "Recomendado")

dfCalificaciones %>%
  group_by(recomendado) %>%
  summarise(cases = n()) %>%
  ggplot(aes(recomendado, cases, fill=c4)) + geom_bar(stat="identity") +
  theme_minimal() + scale_x_continuous(breaks = 0:5) +
  scale_fill_manual("legend", values = c("No Recomendado" = "#FF6666", "Recomendado" = "#33CCCC"))

ratingsSum = dfCalificaciones %>%
  group_by(uid) %>%
  count() 

summary(ratingsSum$n)




