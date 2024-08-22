################################################################################
# Cálculo de modelos 
#
# 
#
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 26/09/2021
# Actualizacion: 26/09/2021
################################################################################

library(ggplot2)
library(plyr)
library(dplyr)
library(cluster)
library(recommenderlab)
library(pander)
library(shiny)

set.seed(1)

setwd("../../data/processed_data")
dfCalificaciones <- read.csv("06_dataset_calificaciones.csv")
dfJuegos <- read.csv("07_dataset_juegos.csv")

dfCalificaciones$uid <- gsub('uid_', '', dfCalificaciones$uid)
dfCalificaciones$uid <- as.integer(dfCalificaciones$uid)

dfCalificaciones$id <- gsub('gid_', '', dfCalificaciones$id)
dfCalificaciones$id <- as.integer(dfCalificaciones$id)

dfJuegos$id <- gsub('gid_', '', dfJuegos$id)
dfJuegos$id <- as.integer(dfJuegos$id) 

setwd("../utils")
source("01funciones.R")

#
# GENERACION DE DF DE JUEGOS
#
dfJuegos$precioMasAltoFecha <- as.numeric(as.POSIXct(as.Date(dfJuegos$precioMasAltoFecha, "%Y-%m-%d")))
dfJuegos$precioMasBajoFecha <- as.numeric(as.POSIXct(as.Date(dfJuegos$precioMasBajoFecha, "%Y-%m-%d")))
dfJuegos$primerPrecioFecha <- as.numeric(as.POSIXct(as.Date(dfJuegos$primerPrecioFecha, "%Y-%m-%d")))

dfJuegos = dfJuegos %>% mutate_if(is.character, as.factor)

dfJuegos <- ddply(dfJuegos, c("id", "nombre", "precio", "win", "lin", "mac", "historia", "rapido", 
                              "ajustable", "metacritic", "precioMasAlto", "precioMasAltoFecha", "precioMasBajo",
                              "precioMasBajoFecha", "primerPrecio", "primerPrecioFecha", "contadorRebajas"), 
                  function(x) colSums(x[c("spanish","english","german","latam", "si_recomendado", "no_recomendado")]))

dfJuegos$metacritic[is.na(dfJuegos$metacritic)] <- 0
dfJuegos$metacritic[is.nan(dfJuegos$metacritic)] <- 0

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
# Análisis
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

dfCalificaciones$metacritic[is.na(dfCalificaciones$metacritic)] <- 0
dfCalificaciones$metacritic[is.nan(dfCalificaciones$metacritic)] <- 0

dfUsuarios <- generaDataUsuarios(dfCalificaciones)

calificacionesMatrix = crearMatrizRealUBCF(dfCalificaciones)
calificacionesMatrix %>% getRatings %>% hist(main = "Ratings")
calificacionesMatrix %>% normalize %>% getRatings %>% hist(main = "Rating normalizados por usuario")

dim(calificacionesMatrix)

#### Exploring which movies have been viewed
views_per_movie <- colCounts(calificacionesMatrix)

table_views <- data.frame(
  movie = names(views_per_movie),
  views = views_per_movie
)
table_views <- table_views[order(table_views$views, decreasing =
                                   TRUE), ]

ggplot(table_views[1:6, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity") + theme(axis.text.x =
                                      element_text(angle = 45, hjust = 1)) + ggtitle("Numero de veces que un juego fue revisado positivamente")

#### Exploring the average ratings
average_ratings <- colMeans(calificacionesMatrix)
qplot(average_ratings) + stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average movie rating")

average_ratings_relevant <- average_ratings[views_per_movie > 100]
qplot(average_ratings_relevant) + stat_bin(binwidth = 0.1) +
  ggtitle(paste("Distribution of the relevant average ratings"))


#### Visualizing the matrix
#### We can visualize the matrix by building a heat map whose colors represent the
#### ratings. Each row of the matrix corresponds to a user, each column to a movie, and
#### each cell to its rating. For this purpose, we can use the generic method: image. The
#### recommenderlab package redefined the method image for realRatingMatrix objects.
image(calificacionesMatrix, main = "Heatmap of the rating matrix")

image(calificacionesMatrix[1:30, 1:30], main = "Heatmap of the first rows and
columns")

min_n_juegos <- quantile(rowCounts(calificacionesMatrix), 0.90)
min_n_usuarios <- quantile(colCounts(calificacionesMatrix), 0.90)
min_n_juegos
min_n_usuarios


image(calificacionesMatrix[rowCounts(calificacionesMatrix) > min_n_juegos,
                           colCounts(calificacionesMatrix) > min_n_usuarios], main = "Heatmap of the top users and movies")

######################################################
## INICIO DE ANALISIS DE RECOMENDACION
######################################################

print(rowCounts(calificacionesMatrix))
print(colCounts(calificacionesMatrix))
ratings_juegos <- calificacionesMatrix[rowCounts(calificacionesMatrix) > 10,
                                       colCounts(calificacionesMatrix) > 20]
ratings_juegos

min_n_juegos <- quantile(rowCounts(ratings_juegos), 0.95)
min_n_usuarios <- quantile(colCounts(ratings_juegos), 0.95)
min_n_juegos
min_n_usuarios

image(ratings_juegos[rowCounts(ratings_juegos) > min_n_juegos, 
                     colCounts(ratings_juegos) > min_n_usuarios], main = "Heatmap of the top users and movies")

average_ratings_per_user <- rowMeans(ratings_juegos)

qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average rating per user")

ratings_juegos_norm <- normalize(ratings_juegos)

sum(rowMeans(ratings_juegos_norm) > 0.00001)

#visualize the normalized matrix
image(ratings_juegos_norm[rowCounts(ratings_juegos_norm) > min_n_juegos,
                          colCounts(ratings_juegos_norm) > min_n_usuarios], main = "Heatmap of the top users and movies")


ratings_juegos_jugados <- binarize(ratings_juegos, minRating = 0)

min_juegos_binary <- quantile(rowCounts(ratings_juegos), 0.95)
min_users_binary <- quantile(colCounts(ratings_juegos), 0.95)

image(ratings_juegos_jugados[rowCounts(ratings_juegos) > min_juegos_binary,
                             colCounts(ratings_juegos) > min_users_binary], main = "Heatmap of the top users and movies")

## ------------------------------------------------------------------------
ratings_movies_good <- binarize(ratings_juegos, minRating = 0)
image(ratings_movies_good[rowCounts(ratings_juegos) >
                            min_juegos_binary,
                          colCounts(ratings_juegos) >
                            min_users_binary],
      main = "Heatmap of the top users and movies binary")


#ENTRENAR

which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_juegos),
                      replace = TRUE, prob = c(0.8, 0.2))
head(which_train)

recc_data_train <- ratings_juegos[which_train, ]
recc_data_test <- ratings_juegos[!which_train, ]


which_set <- sample(x = 1:5, size = nrow(ratings_juegos), replace = TRUE)

####
####User-based collaborative filtering
####

# for(i_model in 1:5) {
#   which_train <- which_set == i_model
#   recc_data_train <- ratings_juegos[which_train, ]
#   recc_data_test <- ratings_juegos[!which_train, ]
#   # build the recommender
# }


recommender_modelsUBCF <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
df_parametersUBCF <- data.frame(
  parameter = names(recommender_modelsUBCF$UBCF_realRatingMatrix$parameters),
  default = unlist(recommender_modelsUBCF$UBCF_realRatingMatrix$parameters)
)
rownames(df_parametersUBCF) <- NULL
pander(head(df_parametersUBCF))

recc_modelUBCF <- Recommender(data = recc_data_train, method = "UBCF")
recc_modelUBCF

model_detailsUBCF <- getModel(recc_modelUBCF)
pander(data.frame(element = names(model_detailsUBCF)))
model_detailsUBCF$data

n_recommendedUBCF <- 6
recc_predictedUBCF <- predict(object = recc_modelUBCF, newdata = recc_data_test, n = n_recommendedUBCF)
recc_predictedUBCF

colnames(ratings_juegos)
recc_predictedUBCF@items

recc_listaResultadosUBCF <- sapply(recc_predictedUBCF@items, function(x){
  colnames(ratings_juegos)[x]
})


e <- matrix(unlist(recc_listaResultadosUBCF), nrow = n_recommendedUBCF, byrow = FALSE)
nombres <- names(recc_listaResultadosUBCF)

n <- rep()##rep(1:length(nombres))

for (i in 1:length(nombres)) {
  if(isTruthy(recc_listaResultadosUBCF[[nombres[i]]])) {
    n <- append(n, nombres[i])
  }
}
colnames(e) <- n
recc_matrixUBCF = e


dim(recc_matrixUBCF)

pander(recc_matrixUBCF[, 1:4])

number_of_itemsUBCF <- factor(table(recc_matrixUBCF))
chart_title <- "Numero de juegos para modelo UBCF"
qplot(number_of_itemsUBCF) + ggtitle(chart_title)


## Let's take a look at the top titles
number_of_items_sortedUBCF <- sort(number_of_itemsUBCF, decreasing = TRUE)
number_of_items_topUBCF <- head(number_of_items_sortedUBCF, n = 4)
table_topUBCF <- data.frame(
  names(number_of_items_topUBCF),
  number_of_items_topUBCF)

## ----eval=FALSE----------------------------------------------------------
## table_top

## ----echo=FALSE----------------------------------------------------------
pander(table_topUBCF)

