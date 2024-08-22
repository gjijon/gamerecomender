################################################################################
# Evaluación de modelos 
#
# 
#
# Autor: Gabriel Jijón, Quito - Ecuador
# Fecha: 04/12/2021
# Actualizacion: 04/12/2021
################################################################################

library(pander)
library(recommenderlab)
library(ggplot2)
set.seed(1)

calificacionesMatrix = crearMatrizReal(dfCalificaciones)
calificacionesMatrix %>% getRatings %>% hist(main = "Ratings")
calificacionesMatrix %>% normalize %>% getRatings %>% hist(main = "Rating normalizados por usuario")

## EJEMPLOS LA 

ratings_juegos <- calificacionesMatrix[rowCounts(calificacionesMatrix) > 10,
                                       colCounts(calificacionesMatrix) > 20]

ratings_juegos

percentage_training <- 0.8
min(rowCounts(ratings_juegos))
items_to_keep <- 5
rating_threshold <- 5
n_eval <- 1

# Let's build eval_sets containing the sets:
eval_sets <- evaluationScheme(data = ratings_juegos,
                              method = "split",
                              train = percentage_training,
                              given = items_to_keep,
                              goodRating = rating_threshold,
                              k = n_eval)
eval_sets

getData(eval_sets, "train")
nrow(getData(eval_sets, "train")) / nrow(ratings_juegos)

#ver ambos
getData(eval_sets, "known")
getData(eval_sets, "unknown")


# There should be about 20 percent of data
nrow(getData(eval_sets, "known")) / nrow(ratings_juegos)
# Let's see how many items we have for each user in the known set. It should be equal to items_to_keep, that is 5
unique(rowCounts(getData(eval_sets, "known")))


# The same is not true for the users in the test set,
qplot(rowCounts(getData(eval_sets, "unknown"))) +
  geom_histogram(binwidth = 5) +
  ggtitle("unknown items by the users")


# Bootstrapping data
percentage_training <- 0.8
items_to_keep <- 5
rating_threshold <- 5
n_eval <- 1
eval_sets <- evaluationScheme(data = ratings_juegos,
                              method = "bootstrap", 
                              train = percentage_training, 
                              given = items_to_keep,
                              goodRating = rating_threshold, 
                              k = n_eval)

eval_sets

getData(eval_sets, "train")
nrow(getData(eval_sets, "train")) / nrow(ratings_juegos)

#ver ambos
getData(eval_sets, "known")
getData(eval_sets, "unknown")


# There should be about 20 percent of data
nrow(getData(eval_sets, "known")) / nrow(ratings_juegos)
# Let's see how many items we have for each user in the known set. It should be equal to items_to_keep, that is 5
unique(rowCounts(getData(eval_sets, "known")))


# The same is not true for the users in the test set,
qplot(rowCounts(getData(eval_sets, "unknown"))) +
  geom_histogram(binwidth = 5) +
  ggtitle("unknown items by the users")

# We can count how many times each user is repeated in the training set:
table_train <- table(eval_sets@runsTrain[[1]])
n_repetitions <- factor(as.vector(table_train))
qplot(n_repetitions) + ggtitle("Number of repetitions in the training set")

# Using k-fold to validate models
n_fold <- 4
eval_sets <- evaluationScheme(data = ratings_juegos,
                              method = "cross-validation",
                              k = n_fold,
                              given = items_to_keep,
                              goodRating = rating_threshold)

## ----warning=FALSE, message=FALSE----------------------------------------
size_sets <- sapply(eval_sets@runsTrain, length)
size_sets


################################################################################
# item based IBCF
################################################################################
n_fold <- 15
items_to_keep <- 11
rating_threshold <- 3
eval_sets <- evaluationScheme(data = ratings_juegos,
                              method = "cross-validation",
                              k = n_fold,
                              given = items_to_keep,
                              goodRating = rating_threshold)

eval_sets

size_sets <- sapply(eval_sets@runsTrain, length)
size_sets

model_to_evaluate <- "IBCF"
model_parameters <- list(method = "Jaccard", k = 30)

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate,
                                parameter = model_parameters)

items_to_recommend <- 3

## ------------------------------------------------------------------------
eval_prediction <- predict(object = eval_recommender,
                           newdata = getData(eval_sets, "known"),
                           n = items_to_recommend,
                           type = "ratings")
class(eval_prediction)

qplot(rowCounts(eval_prediction)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Distribución de juegos por usuario")

## ------------------------------------------------------------------------
eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction,
  data = getData(eval_sets, "unknown"),
  byUser = FALSE)

pander(head(eval_accuracy))

qplot(eval_accuracy[, "RMSE"]) +
  geom_histogram(binwidth = 0.1) +
  ggtitle("Distribución of the RMSE por usuario")


# evaluando las recomendaciones
results <- evaluate(x = eval_sets, method = model_to_evaluate, n = seq(10, 100, 10))
class(results)

pander(head(getConfusionMatrix(results)[[1]]))

columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]

pander(head(indices_summed))

plot(results, annotate = TRUE, main = "ROC curve")

plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")


# evaluando
models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  random = list(name = "RANDOM", param=NULL)
)

n_recommendations <- c(1, 5, seq(10, 70, 5))

list_results <- evaluate(x = eval_sets,
                         method = models_to_evaluate,
                         n = n_recommendations)
class(list_results)

## ------------------------------------------------------------------------
class(list_results[[1]])

plot(list_results, annotate = 1, legend = "topleft")
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")


#optimizando K
vector_k <- c(5, 10, 20, 30, 40)
models_to_evaluate <- lapply(vector_k, function(k){
  list(name = "IBCF", param = list(method = "cosine", k = k))
})
names(models_to_evaluate) <- paste0("IBCF_k_", vector_k)

n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft")
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")


################################################################################
##
##
##
##
##
##
##
## EVALUACION IBCF por medio de ejemplo del libro
##
##
##
##
##
##
##
################################################################################


nn_to_test <- seq(2, 40, by = 2)

list_performance <- lapply(
  X = nn_to_test,
  FUN = function(nn){
    evaluarModeloIBCFconRMSE(dfJuegos = dfJuegos, 
                             ratings_matrix = ratings_juegos,
                  number_neighbors = nn)
  })

list_performance

list_performance[[1]]


## llamada a evaluador


eval1 <- evaluarModeloIBCFconRMSE(dfJuegos, ratings_juegos)

pander(head(eval1))

eval2 <- evaluarModeloIBCFconROC(dfJuegos, ratings_juegos)

pander(head(getConfusionMatrix(eval2)[[1]]))

columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(eval2))[, columns_to_sum]
pander(head(indices_summed))

plot(eval2, annotate = TRUE, main = "ROC curve")

plot(eval2, "prec/rec", annotate = TRUE, main = "Precision-recall")



################################################################################
##
##
##
##
##
##
##
## EVALUACION UBCF por medio de ejemplo del libro
##
##
##
##
##
##
##
################################################################################

nn_to_test <- seq(2, 40, by = 2)

list_performance <- lapply(
  X = nn_to_test,
  FUN = function(nn){
    evaluarModeloUBCF(dfJuegos = dfJuegos, 
                             ratings_matrix = ratings_juegos,
                             number_neighbors = nn)
  })

list_performance[[1]]


## llamada a evaluador


eval1 <- evaluarModeloUBCF(dfJuegos, ratings_juegos)

pander(head(eval1))




################################################################################
##
##
##
##
##
##
##
## Comparación de modelos
##
##
##
##
##
##
##
################################################################################


n_fold <- 15
items_to_keep <- 11
rating_threshold <- 3
eval_sets <- evaluationScheme(data = ratings_juegos,
                              method = "cross-validation",
                              k = n_fold,
                              given = items_to_keep,
                              goodRating = rating_threshold)

eval_sets


# evaluando
models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine"))
)

n_recommendations <- c(1, 5, seq(10, 120, 10))

list_results <- evaluate(x = eval_sets,
                         method = models_to_evaluate,
                         n = n_recommendations)
class(list_results)

## ------------------------------------------------------------------------
class(list_results[[1]])

plot(list_results, annotate = 1, legend = "topleft")
abline(0, 1, col = "green")
title("Curva ROC: IBCF y UBCF")

plot(list_results, "prec/rec", annotate = 1, legend = "top")
title("Precision-recall: IBCF y UBCF")

plot(accuracy(list_results))
