################################################################################
# Funciones para modelos
#
# Funciones que retornan
#
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 26/09/2021
# Actualizacion: 26/09/2021
################################################################################

library(tidyr)

retornaIdNuevo <- function(id, ids) {
  
  if(id %in% ids && id != 0) {
    return(retornaIdNuevo(sample(1:300, 1), ids))
  } else {
    return(id)
  }
}

agregaIdsAleatoriosDeUsuario <- function(dfCalificaciones) {
  uid <- 0
  gid <- ""
  
  cont <- 0
  userIds <- integer()
  
  dfCalificaciones <- dfCalificaciones[order(dfCalificaciones$id), ]
  
  for(row in 1:nrow(dfCalificaciones)) {
    
    if(gid != dfCalificaciones[row, "id"]) {
      gid <- dfCalificaciones[row, "id"]
      userIds <- integer()
      cont <- 0
    }
    
    uid <- retornaIdNuevo(sample(1:300, 1), userIds)
    cont <- cont + 1
    userIds[cont] <- uid
    
    dfCalificaciones[row, "uid"] <- uid
  }
  
  dfCalificaciones <- dfCalificaciones[c(1,30,2:29)]
  
  dfCalificaciones$id = paste0("gid_", dfCalificaciones$id)
  dfCalificaciones$uid = paste0("uid_", dfCalificaciones$uid)
  
  return(dfCalificaciones)
}

calculaFactoresCorrelacion <- function(datos) {
  mat <- as.matrix(datos)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j])
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
  return(p.mat)
} 

recomendarJuego = function(selected_juegos, dissimilarity_matrix, juegos, n_recommendations = 3){
  
  selected_juego_indexes = which(colnames(dissimilarity_matrix) %in% selected_juegos$id)
  
  
  results = data.frame(dissimilarity_matrix[, selected_juego_indexes], 
                       juego_recomendado = row.names(dissimilarity_matrix),
                       stringsAsFactors = FALSE) 
  
  
  recomendaciones = results %>%
    pivot_longer(cols = c(-"juego_recomendado") , names_to = "juego_jugado", 
                 values_to = "dissimilarity")
  
  recomendaciones = recomendaciones %>%
    left_join(selected_juegos, by = c("juego_recomendado" = "id"))
  
  recomendaciones = recomendaciones %>%
    arrange(desc(dissimilarity)) %>%
    filter(juego_recomendado != juego_jugado)
  
  recomendaciones[["no_recomendado"]][is.na(recomendaciones[["no_recomendado"]])] <- 0
  
  recomendaciones = recomendaciones %>%
    filter(!is.na(si_recomendado) ) %>%
    mutate(
      similarity = 1 - dissimilarity,
      weighted_score = similarity * ((100 * si_recomendado)/(si_recomendado + no_recomendado))) %>%
    arrange(desc(weighted_score))
  
  recomendaciones = recomendaciones %>%
    filter(weighted_score > 0) %>%
    group_by(juego_recomendado) %>% slice(1)

  recomendaciones = recomendaciones %>%
    top_n(n_recommendations, weighted_score)
  
  recomendaciones = recomendaciones %>%
    left_join(juegos, by = c("juego_recomendado" = "id"))
  
  return(recomendaciones)
}

crearMatrizReal = function(juegos_usuarios){
  
  juegos_usuarios$rating <- 1
  
  juegos_usuarios$rating[juegos_usuarios$recomendado == 1] <- 5
  
  juegos_usuarios = juegos_usuarios %>% select(uid, id, rating)

  return(juegos_usuarios %>% as("realRatingMatrix"))
}

crearMatrizRealUBCF = function(juegos_usuarios){
  
  juegos_usuarios$rating <- 1
  
  juegos_usuarios$rating[juegos_usuarios$recomendado == 1] <- 5
  
  juegos_usuarios = juegos_usuarios %>% select(uid, id, rating)

  return(juegos_usuarios %>% as("realRatingMatrix"))
}



generaDataUsuariosSimilares = function(dfCalificaciones) {
  regiones <- c(1,2,3,4) #"USA", "LATAM", "SPAIN", "GERMANY")
  dfCalificaciones <- subset(dfCalificaciones, select = c("uid", "comunidad"))
  dfu <- dfCalificaciones %>% group_by(uid) %>% summarise(comunidad = sum(comunidad))
  
  dfu$region <- replicate(300, sample(regiones, 1))

  return(dfu)
}


evaluarModeloIBCFconRMSE <- function (
  dfJuegos,
  # data inputs
  ratings_matrix, # rating matrix
  # K-fold parameters
  n_fold = 15, # number of folds
  items_to_keep = 11, # number of items to keep in the test set
  # model parameters
  number_neighbors = 2, # number of nearest neighbors
  items_to_recommend = 6, # number of items to recommend,
  rating_threshold = 3
){
  ## ----eval=FALSE----------------------------------------------------------
  set.seed(1)
  eval_sets <- evaluationScheme(data = ratings_matrix,
                                method = "cross-validation",
                                k = n_fold,
                                given = items_to_keep)
  
  ## ----eval=FALSE----------------------------------------------------------
  recc_model <- Recommender(data = getData(eval_sets, "train"),
                            method = "IBCF",
                            parameter = list(method = "Jaccard", k = number_neighbors))
  
  eval_sets
  
  ## ----eval=FALSE----------------------------------------------------------
  dist_ratings <- as(recc_model@model$sim, "matrix")
  vector_items <- rownames(dist_ratings)
  
  ## ----eval=FALSE----------------------------------------------------------
  
  dfJuegosTabla <- data.frame(dfJuegos)
  dfJuegosTabla <- setDT(dfJuegosTabla)
  dist_precio_30 <- dfJuegosTabla[, 1 - as.matrix(dist(precio <= 2000))]
  dist_precio_60 <- dfJuegosTabla[, 1 - as.matrix(dist(precio > 2000))]
  dist_historia <- dfJuegosTabla[, 1 - as.matrix(dist(historia == 1))]
  dist_rapido <- dfJuegosTabla[, 1 - as.matrix(dist(rapido == 1))]
  dist_ajustable <- dfJuegosTabla[, 1 - as.matrix(dist(ajustable == 1))]
  dist_rebajas <- dfJuegosTabla[, 1 - as.matrix(dist(contadorRebajas < 10))]
  dist_fecha <- dfJuegosTabla[, 1 - as.matrix(dist(primerPrecioFecha > as.numeric(as.POSIXct(as.Date("2016-01-01", "%Y-%m-%d")))))]
  
  
  rownames(dist_precio_30) <- dfJuegosTabla[, id]
  colnames(dist_precio_30) <- dfJuegosTabla[, id]
  
  rownames(dist_precio_60) <- dfJuegosTabla[, id]
  colnames(dist_precio_60) <- dfJuegosTabla[, id]
  
  rownames(dist_historia) <- dfJuegosTabla[, id]
  colnames(dist_historia) <- dfJuegosTabla[, id]
  
  rownames(dist_rapido) <- dfJuegosTabla[, id]
  colnames(dist_rapido) <- dfJuegosTabla[, id]
  
  rownames(dist_ajustable) <- dfJuegosTabla[, id]
  colnames(dist_ajustable) <- dfJuegosTabla[, id]
  
  rownames(dist_rebajas) <- dfJuegosTabla[, id]
  colnames(dist_rebajas) <- dfJuegosTabla[, id]
  
  rownames(dist_fecha) <- dfJuegosTabla[, id]
  colnames(dist_fecha) <- dfJuegosTabla[, id]
  
  
  #comprobacion nombres
  vector_items <- rownames(dist_ratings)
  dist_precio_30 <- dist_precio_30[vector_items, vector_items]
  dist_precio_60 <- dist_precio_60[vector_items, vector_items]
  dist_historia <- dist_historia[vector_items, vector_items]
  dist_rapido <- dist_rapido[vector_items, vector_items]
  dist_ajustable <- dist_ajustable[vector_items, vector_items]
  dist_rebajas <- dist_rebajas[vector_items, vector_items]
  dist_fecha <- dist_fecha[vector_items, vector_items]
  
  
  ## ----eval=FALSE----------------------------------------------------------
  weight_precio_alto <- 0.04
  weight_precio_bajo <- 0.06
  weight_tipo_juego_historia  <- 0.05
  weight_tipo_juego_rapido  <- 0.10
  weight_tipo_juego_ajustable  <- 0.15
  weight_rebajas  <- 0.10
  weight_fecha  <- 0.05
  
  dist_tot <- 
    dist_precio_30 * weight_precio_bajo + 
    dist_precio_60 * weight_precio_alto + 
    dist_historia  * weight_tipo_juego_historia +
    dist_rapido    * weight_tipo_juego_rapido + 
    dist_ajustable * weight_tipo_juego_ajustable + 
    dist_rebajas   * weight_rebajas + 
    dist_fecha     * weight_fecha + 
    dist_ratings * (1 - 0.55)
  
  recc_model@model$sim <- as(dist_tot, "dgCMatrix")
  
  ## ----eval=FALSE----------------------------------------------------------
  eval_prediction <- predict(object = recc_model,
                             newdata = getData(eval_sets, "known"),
                             n = items_to_recommend,
                             type = "topNList")
  
  ## ----eval=FALSE----------------------------------------------------------
  eval_accuracy <- calcPredictionAccuracy(
    x = eval_prediction,
    data = getData(eval_sets, "unknown"),
    byUser = FALSE,
    given = items_to_recommend,
    goodRating = rating_threshold)
  
  ## ----eval=FALSE----------------------------------------------------------
  return(eval_accuracy)
}


evaluarModeloUBCF <- function (
  dfJuegos,
  # data inputs
  ratings_matrix, # rating matrix
  # K-fold parameters
  n_fold = 15, # number of folds
  items_to_keep = 11, # number of items to keep in the test set
  # model parameters
  number_neighbors = 2, # number of nearest neighbors
  items_to_recommend = 6, # number of items to recommend,
  rating_threshold = 3
){
  ## ----eval=FALSE----------------------------------------------------------
  set.seed(1)
  eval_sets <- evaluationScheme(data = ratings_matrix,
                                method = "cross-validation",
                                k = n_fold,
                                given = items_to_keep)
  
  ## ----eval=FALSE----------------------------------------------------------
  recc_model <- Recommender(data = getData(eval_sets, "train"),
                            method = "UBCF",
                            parameter = list(method = "Jaccard"))
  
  eval_sets

  ## ----eval=FALSE----------------------------------------------------------
  eval_prediction <- predict(object = recc_model,
                             newdata = getData(eval_sets, "known"),
                             n = items_to_recommend,
                             type = "topNList")
  
  ## ----eval=FALSE----------------------------------------------------------
  eval_accuracy <- calcPredictionAccuracy(
    x = eval_prediction,
    data = getData(eval_sets, "unknown"),
    byUser = FALSE,
    given = items_to_recommend,
    goodRating = rating_threshold)
  
  ## ----eval=FALSE----------------------------------------------------------
  return(eval_accuracy)
}


evaluarModeloIBCFconROC <- function (
  dfJuegos,
  # data inputs
  ratings_matrix, # rating matrix
  # K-fold parameters
  n_fold = 15, # number of folds
  items_to_keep = 11, # number of items to keep in the test set
  # model parameters
  number_neighbors = 30, # number of nearest neighbors
  items_to_recommend = 6, # number of items to recommend,
  rating_threshold = 3
){
  ## ----eval=FALSE----------------------------------------------------------
  set.seed(1)
  eval_sets <- evaluationScheme(data = ratings_matrix,
                                method = "cross-validation",
                                k = n_fold,
                                given = items_to_keep,
                                goodRating = rating_threshold)
  
  ## ----eval=FALSE----------------------------------------------------------
  recc_model <- Recommender(data = getData(eval_sets, "train"),
                            method = "IBCF",
                            parameter = list(method = "Jaccard", k = number_neighbors))
  
  ## ----eval=FALSE----------------------------------------------------------
  dist_ratings <- as(recc_model@model$sim, "matrix")
  vector_items <- rownames(dist_ratings)
  
  ## ----eval=FALSE----------------------------------------------------------
  
  dfJuegosTabla <- data.frame(dfJuegos)
  dfJuegosTabla <- setDT(dfJuegosTabla)
  dist_precio_30 <- dfJuegosTabla[, 1 - as.matrix(dist(precio <= 2000))]
  dist_precio_60 <- dfJuegosTabla[, 1 - as.matrix(dist(precio > 2000))]
  dist_historia <- dfJuegosTabla[, 1 - as.matrix(dist(historia == 1))]
  dist_rapido <- dfJuegosTabla[, 1 - as.matrix(dist(rapido == 1))]
  dist_ajustable <- dfJuegosTabla[, 1 - as.matrix(dist(ajustable == 1))]
  dist_rebajas <- dfJuegosTabla[, 1 - as.matrix(dist(contadorRebajas < 10))]
  dist_fecha <- dfJuegosTabla[, 1 - as.matrix(dist(primerPrecioFecha > as.numeric(as.POSIXct(as.Date("2016-01-01", "%Y-%m-%d")))))]
  
  
  rownames(dist_precio_30) <- dfJuegosTabla[, id]
  colnames(dist_precio_30) <- dfJuegosTabla[, id]
  
  rownames(dist_precio_60) <- dfJuegosTabla[, id]
  colnames(dist_precio_60) <- dfJuegosTabla[, id]
  
  rownames(dist_historia) <- dfJuegosTabla[, id]
  colnames(dist_historia) <- dfJuegosTabla[, id]
  
  rownames(dist_rapido) <- dfJuegosTabla[, id]
  colnames(dist_rapido) <- dfJuegosTabla[, id]
  
  rownames(dist_ajustable) <- dfJuegosTabla[, id]
  colnames(dist_ajustable) <- dfJuegosTabla[, id]
  
  rownames(dist_rebajas) <- dfJuegosTabla[, id]
  colnames(dist_rebajas) <- dfJuegosTabla[, id]
  
  rownames(dist_fecha) <- dfJuegosTabla[, id]
  colnames(dist_fecha) <- dfJuegosTabla[, id]
  
  
  #comprobacion nombres
  vector_items <- rownames(dist_ratings)
  dist_precio_30 <- dist_precio_30[vector_items, vector_items]
  dist_precio_60 <- dist_precio_60[vector_items, vector_items]
  dist_historia <- dist_historia[vector_items, vector_items]
  dist_rapido <- dist_rapido[vector_items, vector_items]
  dist_ajustable <- dist_ajustable[vector_items, vector_items]
  dist_rebajas <- dist_rebajas[vector_items, vector_items]
  dist_fecha <- dist_fecha[vector_items, vector_items]
  
  
  ## ----eval=FALSE----------------------------------------------------------
  weight_precio_alto <- 0.04
  weight_precio_bajo <- 0.06
  weight_tipo_juego_historia  <- 0.05
  weight_tipo_juego_rapido  <- 0.10
  weight_tipo_juego_ajustable  <- 0.15
  weight_rebajas  <- 0.10
  weight_fecha  <- 0.05
  
  dist_tot <- 
    dist_precio_30 * weight_precio_bajo + 
    dist_precio_60 * weight_precio_alto + 
    dist_historia  * weight_tipo_juego_historia +
    dist_rapido    * weight_tipo_juego_rapido + 
    dist_ajustable * weight_tipo_juego_ajustable + 
    dist_rebajas   * weight_rebajas + 
    dist_fecha     * weight_fecha + 
    dist_ratings * (1 - 0.55)
  
  recc_model@model$sim <- as(dist_tot, "dgCMatrix")
  
  ## ----eval=FALSE----------------------------------------------------------
  eval_prediction <- predict(object = recc_model,
                             newdata = getData(eval_sets, "known"),
                             n = items_to_recommend,
                             type = "ratings")
  
  ## ----eval=FALSE----------------------------------------------------------
  results <- evaluate(x = eval_sets, method = "IBCF", n = seq(10, 100, 10))

  
  ## ----eval=FALSE----------------------------------------------------------
  return(results)
}


getNombresJuegos <- function (dfJuegos, juegosAleatorios){

  retorno <- dfJuegos %>% filter(id %in% juegosAleatorios)
  retorno <- retorno %>% 
    mutate(promedioCalificacion = si_recomendado * 100 / (si_recomendado+no_recomendado)) %>%
    select(id, nombre, precio, historia, rapido, ajustable, promedioCalificacion) %>% 
    mutate(precio = precio / 100) %>%
    mutate_at(vars(promedioCalificacion), funs(round(., 0)))
  
  return(retorno)
}


presentaResultados <- function (dfJuegos, predicted, uid){
  recc_new_user <- predicted@items[[uid]]
  juegos_new_user <- predicted@itemLabels[recc_new_user]
  table_new_user <- data.frame(index = recc_new_user,
                                    juego = juegos_new_user)
  
  retorno <- dfJuegos %>% filter(id %in% juegos_new_user)
  retorno <- retorno %>% 
    mutate(promedioCalificacion = si_recomendado * 100 / (si_recomendado+no_recomendado)) %>%
    select(id, nombre, precio, historia, rapido, ajustable, promedioCalificacion) %>% 
    mutate(precio = precio / 100) %>%
    mutate_at(vars(promedioCalificacion), funs(round(., 0)))
  
  return(retorno)
}


retornaJuegoPorId <- function (dfJuegos, juegosId){
  
  retorno <- dfJuegos %>% filter(id %in% juegosId)
  retorno <- retorno[, -(22:23)]
  
  return(retorno)
}