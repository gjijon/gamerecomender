################################################################################
# Consumo de api con R PRECIOS
#
# Script para determinar aplicaciones en dataframe1
#
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 26/09/2021
# Actualizacion: 26/09/2021
################################################################################

library("stringr")
require("httr")
require("jsonlite")
require("miscTools")
library(dplyr)

setwd("../utils")
source("00bfunciones.R")

setwd("../../data/rawdata")

base <- "https://steamdb.info"
app <- "/api"
func <- "/GetPriceHistory"
query <- "/?appid="
cc <- "&cc=us"

revs <- read.csv("04_apps_reviews_final.csv")
revs <- subset(revs, select = c(2))
revs <- distinct(revs, id, .keep_all = TRUE)

carpetaJsonPrecios <- "carpetaJsonPrecios"
dir.create(carpetaJsonPrecios)

last <- 1
for(row in last:nrow(revs)) {
  
  callRev <- paste(base, app, func, query, revs[row,1], cc, sep="")
  
  listaPrecios <- GET(callRev, add_headers("User-Agent" = "user_agent"))
  
  listaPrecios_text <- content(listaPrecios, "text")

  setwd(paste("../../data/rawdata", carpetaJsonPrecios, sep = ""))
  write(listaPrecios_text, file = paste(revs[row,1],"json", sep = "."))
  
  esperaTiempoRequestSteamDB(row)
}

print(listaPrecios_text)


write.csv(getAppList_df, "01_all_apps.csv", row.names = TRUE)