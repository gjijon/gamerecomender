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
library(data.table)

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


calificacionesMatrix = crearMatrizReal(dfCalificaciones)
calificacionesMatrix %>% getRatings %>% hist(main = "Ratings")
calificacionesMatrix %>% normalize %>% getRatings %>% hist(main = "Rating normalizados por usuario")

dim(calificacionesMatrix)

#### Exploring which movies have been viewed
calif_por_juego <- colCounts(calificacionesMatrix)

tabla_calif <- data.frame(
  Videojuego = names(calif_por_juego),
  Calificaciones = calif_por_juego
)
tabla_calif <- tabla_calif[order(tabla_calif$views, decreasing =
                                   TRUE), ]

ggplot(tabla_calif[1:6, ], aes(x = Videojuego, y = Calificaciones)) +
  geom_bar(stat="identity") + theme(axis.text.x =
  element_text(angle = 45, hjust = 1)) + ggtitle("Numero de veces que un juego fue revisado positivamente")

#### Exploring the average ratings
average_ratings <- colMeans(calificacionesMatrix)
qplot(average_ratings) + stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average movie rating")

average_ratings_relevant <- average_ratings[calif_por_juego > 100]
qplot(average_ratings_relevant) + stat_bin(binwidth = 0.1) +
  ggtitle(paste("Distribution of the relevant average ratings"))


#### Visualizing the matrix
#### We can visualize the matrix by building a heat map whose colors represent the
#### ratings. Each row of the matrix corresponds to a user, each column to a movie, and
#### each cell to its rating. For this purpose, we can use the generic method: image. The
#### recommenderlab package redefined the method image for realRatingMatrix objects.
image(calificacionesMatrix, main = "Mapa de calor")

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
                          colCounts(ratings_juegos_norm) > min_n_usuarios], main = "Mapa de calor normalizado por usuarios y videojuegos")


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
####ITEM-based collaborative filtering
####

# for(i_model in 1:5) {
#   which_train <- which_set == i_model
#   recc_data_train <- ratings_juegos[which_train, ]
#   recc_data_test <- ratings_juegos[!which_train, ]
#   # build the recommender
# }

# metodos
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models$IBCF_realRatingMatrix

recc_model <- Recommender(data = recc_data_train, method = "IBCF",
                          parameter = list(k = 30))

class(recc_model@model$sim)
dim(recc_model@model$sim)

## ------------------------------------------------------------------------
image(recc_model@model$sim)
n_items_top <- 20
model_details <- getModel(recc_model)
image(model_details$sim[1:n_items_top, 1:n_items_top],
      main = "Mapa de calor de los primeros 20 registros de usuarios y juegos")

## ------------------------------------------------------------------------
model_details$k
row_sums <- rowSums(model_details$sim > 0)
table(row_sums)

col_sums <- colSums(model_details$sim > 0)
qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of
the column count")
## ------------------------------------------------------------------------
range(recc_model@model$sim)

dist_ratings <- as(recc_model@model$sim, "matrix")

##calculo de peso
# primero juegos a datatable
dfJuegosTabla <- data.frame(dfJuegos)
dfJuegosTabla <- setDT(dfJuegosTabla)
dist_precio_30 <- dfJuegosTabla[, 1 - dist(precio <= 2000)]
dist_precio_30 <- as(dist_precio_30, "matrix")

dist_precio_60 <- dfJuegosTabla[, 1 - dist(precio > 2000)]
dist_precio_60 <- as(dist_precio_60, "matrix")

dist_historia <- dfJuegosTabla[, 1 - dist(historia == 1)]
dist_historia <- as(dist_historia, "matrix")

dist_rapido <- dfJuegosTabla[, 1 - dist(rapido == 1)]
dist_rapido <- as(dist_rapido, "matrix")

dist_ajustable <- dfJuegosTabla[, 1 - dist(ajustable == 1)]
dist_ajustable <- as(dist_ajustable, "matrix")

dist_rebajas <- dfJuegosTabla[, 1 - dist(contadorRebajas < 10)]
dist_rebajas <- as(dist_rebajas, "matrix")

dist_fecha <- dfJuegosTabla[, 1 - dist(primerPrecioFecha > as.numeric(as.POSIXct(as.Date("2016-01-01", "%Y-%m-%d"))))]
dist_fecha <- as(dist_fecha, "matrix")

## ------------------------------------------------------------------------
dim(dist_precio_30)
dim(dist_precio_60)
dim(dist_historia)
dim(dist_rapido)
dim(dist_ajustable)
dim(dist_rebajas)
dim(dist_fecha)

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

## ------------------------------------------------------------------------
identical(dim(dist_precio_30), dim(dist_ratings))
identical(rownames(dist_precio_30), rownames(dist_ratings))
identical(colnames(dist_precio_30), colnames(dist_ratings))

identical(dim(dist_precio_60), dim(dist_ratings))
identical(rownames(dist_precio_60), rownames(dist_ratings))
identical(colnames(dist_precio_60), colnames(dist_ratings))

identical(dim(dist_historia), dim(dist_ratings))
identical(rownames(dist_historia), rownames(dist_ratings))
identical(colnames(dist_historia), colnames(dist_ratings))

identical(dim(dist_rapido), dim(dist_ratings))
identical(rownames(dist_rapido), rownames(dist_ratings))
identical(colnames(dist_rapido), colnames(dist_ratings))

identical(dim(dist_ajustable), dim(dist_ratings))
identical(rownames(dist_ajustable), rownames(dist_ratings))
identical(colnames(dist_ajustable), colnames(dist_ratings))

identical(dim(dist_rebajas), dim(dist_ratings))
identical(rownames(dist_rebajas), rownames(dist_ratings))
identical(colnames(dist_rebajas), colnames(dist_ratings))

identical(dim(dist_fecha), dim(dist_ratings))
identical(rownames(dist_fecha), rownames(dist_ratings))
identical(colnames(dist_fecha), colnames(dist_ratings))

image(dist_precio_30)

#calculo de relacion de distancia
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

image(dist_tot)

recc_model@model$sim <- as(dist_tot, "dgCMatrix")


## Recommender of type 'IBCF' for 'realRatingMatrix'
## learned using 111 users.
class(recc_model)

model_details <- getModel(recc_model)
model_details$description
model_details$k

class(model_details$sim)
## [1] "dgCMatrix"
## attr(,"package")
## [1] "Matrix"
dim(model_details$sim)
## [1] 332 332

n_items_top <- 10

image(model_details$sim[1:n_items_top, 1:n_items_top],
      main = "Heatmap of the first rows and columns")

model_details$k
## [1] 30
row_sums <- rowSums(model_details$sim > 0)
table(row_sums)
## row_sums
## 30
## 332

col_sums <- colSums(model_details$sim > 0)

qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of the column count")


which_max <- order(col_sums, decreasing = TRUE)[1:6]

## ----echo=FALSE----------------------------------------------------------
df_colsums <- data.frame(
  movie = rownames(model_details$sim)[which_max],
  col_sum = col_sums[which_max])
rownames(df_colsums) <- NULL
pander(df_colsums)

##############
rownames(model_details$sim)[which_max]

n_recommended <- 6

recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)
recc_predicted
## Recommendations as 'topNList' with n = 6 for 449 users.

class(recc_predicted)
## [1] "topNList"
## attr(,"package")
## [1] "recommenderlab"
slotNames(recc_predicted)
## [1] "items" "itemLabels" "n"

recc_predicted@items[[1]]
## [1] 201 182 254 274 193 297

#We can extract the recommended movies from recc_predicted@item labels:
recc_user_1 <- recc_predicted@items[[1]]
juegos_user_1 <- recc_predicted@itemLabels[recc_user_1]
table_user_1 <- data.frame(index = recc_user_1,
                           juego = juegos_user_1)
pander(table_user_1)


#We can define a matrix with the recommendations for each user:
print(recc_predicted@items)
recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_juegos)[x]
})

e <- matrix(unlist(recc_matrix), nrow = n_recommended, byrow = FALSE)
nombres <- names(recc_matrix)

n <- rep()##rep(1:length(nombres))

for (i in 1:length(nombres)) {
  if(isTruthy(recc_matrix[[nombres[i]]])) {
    n <- append(n, nombres[i])
  }
}
colnames(e) <- n
recc_matrix = e




class(recc_matrix)
dim(recc_matrix)
## [1] 6 449

pander(recc_matrix[, 1:4])

number_of_items <- factor(table(unlist(recc_matrix)))
chart_title <- "Numero de juegos para modelo IBCF"
qplot(number_of_items) + ggtitle(chart_title)


#popular
number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(names(number_of_items_top),
                        number_of_items_top)
pander(table_top)

