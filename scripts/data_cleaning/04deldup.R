################################################################################
# borrado de duplicados
################################################################################

library(dplyr)

setwd("../../data/rawdata")

revs <- read.csv("03_apps_reviews.csv")

revs_del <- distinct(revs, id, fecha, .keep_all = TRUE)
revs_del <- subset(revs_del, select = -c(1,2))

setwd("../../data/processed_data")

write.csv(revs_del, "04_apps_reviews_final.csv", row.names = TRUE)
