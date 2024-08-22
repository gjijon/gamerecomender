################################################################################
# Consumo de api con R
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

setwd("../utils")
source("00bfunciones.R")

setwd("../../data/rawdata")

# trae todas las aplicaciones de STEAM

base <- "https://api.steampowered.com"
app <- "/ISteamApps"
func <- "/GetAppList"
ver <- "/v2"

callRev <- paste(base, app, func, ver, sep="")

getAppList <- GET(callRev, add_headers("user_agent" = "user_agent"))

getAppList_text <- content(getAppList, "text")
getAppList_json <- fromJSON(getAppList_text, flatten = TRUE)
getAppList_df <- as.data.frame(getAppList_json[["applist"]])

write.csv(getAppList_df, "01_all_apps.csv", row.names = TRUE)

# itera sobre aplicaciones y guarda juegos
# valpha: primeras 200 apps
# vbeta: 1500

base <- "https://store.steampowered.com"
app <- "/api"
func <- "/appdetails"
appBuscar <- "?appids="

tiposApps <- c("game")
generosApps <- c("Adventure", "Action", "RPG")
generosAppsComp = paste(generosApps, collapse="|")

nuevoDF <- nuevoDataFrameJuego();
agregados <- 0

set.seed(7342114)
getAppListReducido <- getAppList_df[sample(1:nrow(getAppList_df), 1500), ]
last <- 1001
for(row in last:1500) {
  
  esperaTiempoRequest(row)
  
  ids <- getAppListReducido[row, "apps.appid"]
  appBuscarIn <- paste(appBuscar, ids, sep="")
  callRev <- paste(base, app, func, appBuscarIn, sep="")
  
  getAppDet <- GET(callRev, add_headers("user_agent" = "user_agent"))
  
  getAppDet_text <- content(getAppDet, "text")
  getAppDet_json <- fromJSON(getAppDet_text, flatten = TRUE)

  if(
    !is.null(getAppDet_json) &&
    getAppDet_json[[1]][[1]] == "TRUE" && 
    str_detect(tiposApps, getAppDet_json[[1]][["data"]][["type"]], negate = FALSE) &&
    !is.null(getAppDet_json[[1]][["data"]][["price_overview"]][["final"]])
    ) {
    tipoApp <- getAppDet_json[[1]][["data"]][["type"]]

    getAppDetGeneros_json <- flatten(getAppDet_json[[1]][["data"]][["genres"]], recursive = TRUE)
    
    generoApp <- as.vector(t(getAppDetGeneros_json[2]))
    
    isGeneroApp <- any(str_detect(generosAppsComp, as.vector(t(getAppDetGeneros_json[2]))))
    if(isGeneroApp) {
      print(paste('Tomando en cuenta a', ids, "(", agregados, "entre", row, ")", sep=" "))
      juegoNuevo <- retornaRegistroJuego(getAppDet_json[[1]][["data"]], generosApps)
      nuevoDF[nrow(nuevoDF)+1,] <- juegoNuevo
      agregados <- agregados + 1
    }
  }
}

print(paste("Agregados:", agregados, sep = " "))

write.csv(nuevoDF, "02_apps_details.csv", row.names = TRUE)
