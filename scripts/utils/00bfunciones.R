################################################################################
# Consumo de api con R
#
# definicion de funciones
#
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 26/09/2021
# Actualizacion: 26/09/2021
################################################################################


require(jsonlite) 


#
# id:     id de la aplicacion
# nombre: nombre del juego
# precio:  precio final=double(),
# win:     plataforma windows
# lin:     plataforma linux
# mac:     plataforma MAC
# historia:   tipo de juego historia
# rapido:     tipo de juego rapido
# ajustable:  tipo de juego ajustable de caracter
# metacritic:  Calificacion en metacritic
# urlMetacritic: URL a la calificacion
#
nuevoDataFrameJuego <- function() {
  df <- data.frame(id=integer(),
                   nombre=character(),
                   precio=double(),
                   win=logical(),
                   lin=logical(),
                   mac=logical(),
                   historia=logical(),
                   rapido=logical(),
                   ajustable=logical(),
                   metacritic=integer(),
                   urlMetacritic=character())
  
  return(df)
}

# id:     id de la aplicacion
# nombre: nombre del juego
# precio:  precio final=double(),
# win:     plataforma windows
# lin:     plataforma linux
# mac:     plataforma MAC
# historia:   tipo de juego historia
# rapido:     tipo de juego rapido
# ajustable:  tipo de juego ajustable de caracter
# idioma
# fecha
# recomendado
# comunidad
# comprado
# contadorJuegosAdicional
# tiempoJuego
# tiempoJuegoReciente
#
nuevoDataFrameCompleto <- function() {
  df <- data.frame(
                   id=integer(),
                   nombre=character(),
                   precio=double(),
                   win=logical(),
                   lin=logical(),
                   mac=logical(),
                   historia=logical(),
                   rapido=logical(),
                   ajustable=logical(),
                   metacritic=integer(),
                   urlMetacritic=character(),
                   idioma=character(),
                   fecha=character(),
                   recomendado=logical(),
                   comunidad=integer(),
                   comprado=logical(),
                   contadorJuegosAdicional=integer(),
                   tiempoJuego=integer(),
                   tiempoJuegoReciente=integer()
                   )
  
  return(df)
}

retornaRegistroJuego <- function(reg, tiposPermitidos) {
  tipoActual <- as.vector(t(flatten(reg[["genres"]], recursive = TRUE)[["description"]]))
  mc <- ""
  mcurl <- ""
  if(!is.null(reg[["metacritic"]])) {
    mc <- reg[["metacritic"]][["score"]]
    mcurl <- reg[["metacritic"]][["url"]]
  }
  
  k <- c(
    reg[["steam_appid"]],
    reg[["name"]],
    as.double(reg[["price_overview"]][["final"]]),
    reg[["platforms"]][["windows"]],
    reg[["platforms"]][["linux"]],
    reg[["platforms"]][["mac"]],
    any(str_detect(tipoActual, tiposPermitidos[1])),
    any(str_detect(tipoActual, tiposPermitidos[2])),
    any(str_detect(tipoActual, tiposPermitidos[3])),
    mc,
    mcurl
  )

  print(reg[["name"]])

  return(k)
}

#
# a juego registro
# b revisiones data.frame
retornaRegistrosCompletos <- function(a, b) {
  retorno <- nuevoDataFrameCompleto()
  for (rowdf in 1:nrow(b)) {
    reg <- retornaRegistroCompleto(a, b[rowdf, ])
    retorno[nrow(retorno)+1,] <- reg
  }
  return(retorno)
}


# id:     id de la aplicacion
# nombre: nombre del juego
# precio:  precio final=double(),
# win:     plataforma windows
# lin:     plataforma linux
# mac:     plataforma MAC
# historia:   tipo de juego historia
# rapido:     tipo de juego rapido
# ajustable:  tipo de juego ajustable de caracter
# idPersona
# idioma
# fecha
# recomendado
# comunidad
# comprado
# contadorJuegosAdicional
# tiempoJuego
#
retornaRegistroCompleto <- function(reg1, reg2) {
  mc <- ""
  mcurl <- ""
  if(!is.null(reg2[["metacritic"]])) {
    mc <- reg2[["metacritic"]]
  }
  
  if(!is.null(reg2[["urlMetacritic"]])) {
    mcurl <- reg2[["urlMetacritic"]]
  }
  
  retornoRegistroUnitario <- c(
    reg1[["id"]],
    reg1[["nombre"]],
    reg1[["precio"]],
    reg1[["win"]],
    reg1[["lin"]],
    reg1[["mac"]],
    reg1[["historia"]],
    reg1[["rapido"]],
    reg1[["ajustable"]],
    mc,
    mcurl,
    reg2[["language"]],
    reg2[["timestamp_created"]],
    reg2[["voted_up"]],
    reg2[["votes_up"]],
    reg2[["steam_purchase"]],
    reg2[["author.num_games_owned"]],
    reg2[["author.playtime_forever"]],
    reg2[["author.playtime_last_two_weeks"]]
  )
  
  return(retornoRegistroUnitario)
}

procesaJsonPrecio <- function(idApp) {

  salida <- tryCatch(
    {
      precioAlto <- 0
      precioBajo <- 0
      contadorRebajas <- 0
      
      preciosJson <- read_json(paste("carpetaJsonPrecios/",idApp, ".json", sep = ""), simplifyVector = TRUE)
      preciosDFApp <- as.data.frame(preciosJson[["data"]][["history"]])
      salesDFApp <- as.data.frame(preciosJson[["data"]][["sales"]])
      salesDFApp <- t(salesDFApp)

      if(preciosJson[["success"]] == TRUE) {
        
        regMasAlto <- preciosDFApp[which.max(preciosDFApp$y),]
        regMasBajo <- preciosDFApp[which.min(preciosDFApp$y),]
        primerRegistro <- preciosDFApp[which.min(preciosDFApp$x),]
        
        precioAlto <- regMasAlto$y * 100
        precioAltoFecha <- regMasAlto$x
        precioBajo <- regMasBajo$y * 100
        precioBajoFecha <- regMasBajo$x
        primerPrecio <- primerRegistro$y * 100
        primerPrecioFecha <- primerRegistro$x
        contadorRebajas <- nrow(salesDFApp)
        
        return (list(precioAlto, precioAltoFecha, precioBajo, precioBajoFecha, primerPrecio, primerPrecioFecha, contadorRebajas))
      } else {
        return(NULL)
      }
    },
    error = function(cond) {
      print(cond)
      return(NULL)
    },
    warning = function(cond) {
      print(cond)
      return(NULL)
    }
  )

  return (salida)
}

esperaTiempoRequest <- function(cont){
  print(paste("->",cont,sep = " "))
  vs <- sample(2:3, 1)
  Sys.sleep(vs)
}

esperaTiempoRequestSteamDB <- function(cont){
  print(paste("->",cont,sep = " "))
  vs <- sample(6:10, 1)
  Sys.sleep(vs)
}