################################################################################
# Consumo de api con R
#
# Script para determinar aplicaciones en dataframe1 restado del anterior
#
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 26/09/2021
# Actualizacion: 26/09/2021
################################################################################

library("stringr")
require("httr")
require("jsonlite")
require("miscTools")
require("data.table")

setwd("../utils")
source("00bfunciones.R")

setwd("../../data/rawdata")

appsTodas <- read.csv("01_all_apps.csv", colClasses=c("NULL", NA, NA))

appsActuales <- fread("02_apps_details.csv", select = c(3:4))
colnames(appsActuales) <- c('apps.appid','apps.name')

base <- "https://store.steampowered.com"
app <- "/api"
func <- "/appdetails"
appBuscar <- "?appids="

tiposApps <- c("game")
generosApps <- c("Adventure", "Action", "RPG")
generosAppsComp = paste(generosApps, collapse="|")

nuevoDFIntersecado <- nuevoDataFrameJuego();
agregados <- 0

set.seed(5369007)
getAppListReducido <- appsTodas[sample(1:nrow(appsTodas), 50), ]
last <- 1

getAppListReducido <- anti_join(getAppListReducido, appsActuales)


for(row in last:nrow(getAppListReducido)) {
  
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
      nuevoDFIntersecado[nrow(nuevoDFIntersecado)+1,] <- juegoNuevo
      agregados <- agregados + 1
    }
  }
}

print(paste("Agregados:", agregados, sep = " "))

write.csv(nuevoDFIntersecado, "02_apps_details_temp.csv", row.names = TRUE)

appsDetallesOriginal <- read.csv("02_apps_details.csv")

appsDetallesOriginal <- subset(appsDetallesOriginal, select = -c(1))

appsDetallesNuevo <- read.csv("02_apps_details_temp.csv")
appsDetallesNuevo <- rbind(appsDetallesOriginal, appsDetallesNuevo)
appsDetallesNuevo <- subset(appsDetallesNuevo, select = -c(1))

write.csv(appsDetallesNuevo, "02_apps_details.csv", row.names = TRUE)
