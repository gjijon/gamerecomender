################################################################################
# Consumo de api con R
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 26/09/2021
# Actualizacion: 26/09/2021
################################################################################

library("stringr")
require("httr")
require("jsonlite")

retornaCursorPorIdioma <- function(id, idioma, rowdf) {
  
  esperaTiempoRequest(rowdf)
  
  base <- "https://store.steampowered.com"
  reviewsEndPoint <- "/appreviews"
  appId <- paste("/", id, sep="")
  json <- "json=1"
  numPerPage <- "&num_per_page=1"
  cursorId <- "*"
  cursor <- paste("&cursor=",cursorId, sep="")
  
  langs <- idioma #"spanish" #english,french,german,spanish,latam
  filter <- paste("&filter=","all", sep="")
  language <- paste("&language=",langs, sep="")
  dayRange <- paste("&day_range=","600", sep="")
  reviewType <- paste("&review_type=","all", sep="")
  purchaseType <- paste("&purchase_type=","all", sep="")
  
  callRev <- paste(base,reviewsEndPoint,appId,"?",json, cursor, numPerPage, filter, language, dayRange, reviewType, purchaseType, sep="")
  
  #print(callRev)
  
  get_review <- GET(callRev)
  
  get_review_text <- content(get_review, "text")
  get_review_json <- fromJSON(get_review_text, flatten = TRUE)

  if(get_review_json[["query_summary"]][["num_reviews"]] > 0) {
    cursorId <- URLencode(get_review_json[["cursor"]], reserved = TRUE)
    print(paste("Cursor para", idioma, "es", cursorId, sep=" "))
    
    return(cursorId)
  } else {
    return("*")
  }
}

retornaDatosPorIdioma <- function(id, idioma, cursor, rowdf) {
  
  esperaTiempoRequest(rowdf)
  
  base <- "https://store.steampowered.com"
  reviewsEndPoint <- "/appreviews"
  appId <- paste("/", id, sep="")
  json <- "json=1"
  cursorId <- cursor
  cursor <- paste("&cursor=",cursorId, sep="")
  langs <- idioma
  filter <- paste("&filter=","all", sep="")
  language <- paste("&language=",langs, sep="")
  dayRange <- paste("&day_range=","600", sep="")
  reviewType <- paste("&review_type=","all", sep="")
  purchaseType <- paste("&purchase_type=","all", sep="")
  numPerPage <- "&num_per_page=20"
  
  callRev <- paste(base,reviewsEndPoint,appId,"?",json, cursor, numPerPage, filter, language, dayRange, reviewType, purchaseType, sep="")
  
  get_review <- GET(callRev)
  
  get_review_text1 <- content(get_review, "text")
  get_review_json1 <- fromJSON(get_review_text1, flatten = TRUE)
  
  if(get_review_json1[["query_summary"]][["num_reviews"]] > 0) {
  
    get_review_df1 <- as.data.frame(get_review_json1[["reviews"]])
    
    cursor <- paste("&cursor=",URLencode(get_review_json1[["cursor"]], reserved = TRUE), sep="")
    callRev <- paste(base,reviewsEndPoint,appId,"?",json, cursor, numPerPage, filter, language, dayRange, reviewType, purchaseType, sep="")
    
    esperaTiempoRequest(rowdf)
    
    get_review <- GET(callRev)
    
    print(callRev)
    
    get_review_text2 <- content(get_review, "text")
    get_review_json2 <- fromJSON(get_review_text2, flatten = TRUE)
    get_review_df2 <- as.data.frame(get_review_json2[["reviews"]])
    
    if(ncol(get_review_df1) == ncol(get_review_df2)) {
      return(rbind(get_review_df1, get_review_df2))
    } else {
      return(get_review_df1)
    }
  } else {
    return(nuevoDataFrameCompleto())
  }
}