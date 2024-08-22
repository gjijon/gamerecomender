################################################################################
# Consumo de api con R
#
# Instalación de paquetes
#
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 26/09/2021
# Actualizacion: 26/09/2021
################################################################################

install.packages("httr")
install.packages("jsonlite")
install.packages("stringr")
install.packages("miscTools")
install.packages("dplyr")
install.packages("sqldf")
install.packages("data.table")
install.packages("RJSONIO")
install.packages("rjson")
install.packages("funModeling")
install.packages("car")
install.packages("stargazer")
install.packages("skimr")
install.packages("caret") #instalado nuevo
install.packages("e1071") #instalado nuevo
install.packages("expss") #instalado nuevo
install.packages("ggplot2") #instalado nuevo
install.packages("tidyverse")
install.packages("corrplot")

if(!"recommenderlab" %in% rownames(installed.packages())){
  install.packages("recommenderlab")}

setwd("../utils")