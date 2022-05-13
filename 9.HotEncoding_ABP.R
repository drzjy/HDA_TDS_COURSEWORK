library(tidyverse)
library(data.table)
library(janitor)
library(openxlsx)
library(finalfit)
library(lubridate)

options(scipen = 999)


# 1. FastDummies Package & Data Load-------------------------------------------------------------

library(fastDummies)

ukb <- readRDS("completecases_without_outliers_180322_ana.RDS")

ukb$final <- as.factor(ukb$final)
ukb$final <- fct_relevel(ukb$final, "controls", "cases", "others")
table(ukb$final, useNA = "ifany")

# Recode physical activity
ukb$types_of_physical_activity_in_last_4_weeks <- ifelse(ukb$types_of_physical_activity_in_last_4_weeks == 0 | ukb$types_of_physical_activity_in_last_4_weeks == 1, 0, 1)

table(ukb$types_of_physical_activity_in_last_4_weeks, useNA = "ifany")

# Recode hair colour
table(ukb$hair_colour_natural_before_greying, useNA = "ifany")

ukb$hair_colour_natural_before_greying <- ifelse(ukb$hair_colour_natural_before_greying == "Black" | ukb$hair_colour_natural_before_greying == "Dark brown" | ukb$hair_colour_natural_before_greying == "Other", "Black",  ukb$hair_colour_natural_before_greyin)

ukb$hair_colour_natural_before_greying <- ifelse(ukb$hair_colour_natural_before_greying == "6", "Red",  ukb$hair_colour_natural_before_greyin)

ukb$hair_colour_natural_before_greying <- ifelse(ukb$hair_colour_natural_before_greying == "2" | ukb$hair_colour_natural_before_greying == "4" , "Fair",  ukb$hair_colour_natural_before_greyin)

# Recode skin colour
table(ukb$skin_colour, useNA = "ifany")

ukb$skin_colour <- ifelse(ukb$skin_colour == "Black" | ukb$skin_colour == "Brown" | ukb$skin_colour == "Dark olive", "Dark", "Light")
                          
# 2. Transform to factors ----
ukb$skin_colour <- as.factor(ukb$skin_colour)
ukb$hair_colour_natural_before_greying <- as.factor(ukb$hair_colour_natural_before_greying)
ukb$types_of_physical_activity_in_last_4_weeks <- factor( ukb$types_of_physical_activity_in_last_4_weeks)

# SaveRDS(ukb, "complete_cases_110322.rds")

# 3. Select only factors -----

ukbf <- ukb %>% select_if(is.factor)

# get the name of the variables with > 2 levels in the df with only FACTORS
indx <- sapply(ukbf, nlevels) > 2   
ukbf <- ukbf[,indx, drop = TRUE]

ukb_dum <- dummy_cols(ukbf, remove_selected_columns = TRUE, ignore_na = TRUE)
rownames(ukb_dum) <- rownames(ukb)

#get the names the variables with > 2 levels in all UKB
indx2 <- sapply(ukb, nlevels) > 2  
ukb2 <- ukb[,!indx2, drop = FALSE]

ukb_fin <- merge(ukb2, ukb_dum, by = "row.names",all.x = TRUE)

rownames(ukb_fin) <- ukb_fin$Row.names

saveRDS(ukb_fin, "ukb_complete_hotencond_18032022.rds")

