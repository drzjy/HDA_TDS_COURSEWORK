
#loading packages
library(tidyverse)
library(missForest)

match_1_to_5 <- readRDS("outliers_matched_1to1_180322.RDS")

ids <- match_1_to_5$IDs
outcome <- match_1_to_5$final

match_1_to_5 <- subset(match_1_to_5, select=-c(IDs, final))

##calibrated tree = 47

imp_res <- missForest(match_1_to_5, replace = TRUE, maxiter = 5, ntree = 47,verbose = TRUE)

resuts = imp_res$ximp

write_rds(resuts, "rf_imputed_outliers_matched_1to1_180322.RDS.RDS")
