
#loading packages
library(naniar)
library(readr)
library(dplyr)
library(tibble)
library(stringr)
library(tidyr)

ukb_final_read_020322 <- readRDS("aggregated_ukb_final_020322.rds")

block <- ukb_final_read_020322[400001:450000,1:502]

block <- tibble::rownames_to_column(block, "IDs")

na_strings <- c("nan")

block <- block %>% mutate_if(is.factor, as.character)

lock <- block %>%
  replace_with_na_all(condition = ~.x %in% na_strings)


ukb_final_block <- block %>% 
     mutate_all(~replace(., is.nan(.), NA))


block <- readRDS("ukb_final_block7.rds")

ukb_final_block <- block %>% replace_with_na_all(condition = ~.x == "nan")


saveRDS(ukb_final_block, file = "ukb_final_b7.rds")
