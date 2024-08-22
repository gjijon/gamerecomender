################################################################################
# Consumo de api con R
################################################################################


setwd("../utils")
source("00bfunciones.R")
source("02getrevsfunc.R")

setwd("../../data/rawdata")

apps <- read.csv("02_apps_details.csv")
langs <- c("spanish","english","german","latam") #"french",

dfr <- nuevoDataFrameCompleto()
colId <- 2 #3 para version 1 o 2 para version 2

for (rowdf in 1:nrow(apps)) {
  p <- rowdf * 100 / nrow(apps)
  p <- paste(p, "%", sep = "")
  print(paste("*********************************************************************************** fila", rowdf, "de", nrow(apps), p, sep=" "))
  for(rowlang in 1:length(langs)) {
    print(paste("Definiendo cursor para", langs[rowlang], "de", apps[rowdf,colId], sep=" "))
    cursor <- retornaCursorPorIdioma(apps[rowdf,colId], langs[rowlang], rowdf)

    #print(paste(apps[rowdf,colId],langs[rowlang], cursor, sep=" "))
    rvs <- retornaDatosPorIdioma(apps[rowdf,colId], langs[rowlang], cursor, rowdf)
    if(nrow(rvs) > 0) {
      datosNuevos <- retornaRegistrosCompletos(apps[rowdf,], rvs)
      dfr <- rbind(dfr, datosNuevos)
    }
  }
}

write.csv(dfr, "03_apps_reviews.csv", row.names = TRUE)
