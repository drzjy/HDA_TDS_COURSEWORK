rm(list=ls())

#Script to REVERSE one hot encoding

library(tidyverse)

#Load dataframe
#Dataframe details:
# 1:5 matched
#PRS score added
#RF imputed
#one hot encoded

df <- readRDS("RF_imputed_outliers_matched_1to5_Hotencoded_PRS_2903.rds")
#names(df)



#Subset NON-OHE (i.e. continuous + binary) variables
df_continuous <- df[ , -c(74:104)]



#Subset one hot encoded (OHE) variables into separate dataframes
df_alcohol_drinker_status <- df[ , c(74:76)]
df_alcohol_intake_frequency <- df[ , c(77:79)]
df_average_total_household_income_before <- df[ , c(80:82)]
df_childhood_sunburn_occasions <- df[ , c(83:85)]
df_facial_ageing <- df[ , c(86:88)]
df_frequency_of_solarium_sunlamp_use <- df[ , c(89:91)]
df_hair_colour_natural_before_greying <- df[ , c(92:94)]
df_qualifications <- df[ , c(95:97)]
df_smoking_status <- df[ , c(98:100)]
df_use_of_sun_uv_protection <- df[ , c(101:104)]

#Create new column with NAs for variables
df_alcohol_drinker_status$alcohol_drinker_status <- NA
df_alcohol_intake_frequency$alcohol_intake_frequency <- NA
df_average_total_household_income_before$average_total_household_income_before_tax <- NA
df_childhood_sunburn_occasions$childhood_sunburn_occasions <- NA
df_facial_ageing$facial_ageing <- NA
df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use <- NA
df_hair_colour_natural_before_greying$hair_colour_natural_before_greying <- NA
df_qualifications$qualifications <- NA
df_smoking_status$smoking_status <- NA
df_use_of_sun_uv_protection$use_of_sun_uv_protection <- NA

#Re-create factor variable from OHE variables
df_alcohol_drinker_status$alcohol_drinker_status <- ifelse(df_alcohol_drinker_status$alcohol_drinker_status_0 == 1, 0, df_alcohol_drinker_status$alcohol_drinker_status)
df_alcohol_drinker_status$alcohol_drinker_status <- ifelse(df_alcohol_drinker_status$alcohol_drinker_status_1 == 1, 1, df_alcohol_drinker_status$alcohol_drinker_status)
df_alcohol_drinker_status$alcohol_drinker_status <- ifelse(df_alcohol_drinker_status$alcohol_drinker_status_2 == 1, 2, df_alcohol_drinker_status$alcohol_drinker_status)


df_alcohol_intake_frequency$alcohol_intake_frequency <- ifelse(df_alcohol_intake_frequency$alcohol_intake_frequency_0 == 1, 0, df_alcohol_intake_frequency$alcohol_intake_frequency)
df_alcohol_intake_frequency$alcohol_intake_frequency <- ifelse(df_alcohol_intake_frequency$alcohol_intake_frequency_1 == 1, 1, df_alcohol_intake_frequency$alcohol_intake_frequency)
df_alcohol_intake_frequency$alcohol_intake_frequency <- ifelse(df_alcohol_intake_frequency$alcohol_intake_frequency_2 == 1, 2, df_alcohol_intake_frequency$alcohol_intake_frequency)


df_average_total_household_income_before$average_total_household_income_before_tax <- ifelse(df_average_total_household_income_before$average_total_household_income_before_tax_0 == 1, 0, df_average_total_household_income_before$average_total_household_income_before_tax)
df_average_total_household_income_before$average_total_household_income_before_tax <- ifelse(df_average_total_household_income_before$average_total_household_income_before_tax_1 == 1, 1, df_average_total_household_income_before$average_total_household_income_before_tax)
df_average_total_household_income_before$average_total_household_income_before_tax <- ifelse(df_average_total_household_income_before$average_total_household_income_before_tax_2 == 1, 2, df_average_total_household_income_before$average_total_household_income_before_tax)


df_childhood_sunburn_occasions$childhood_sunburn_occasions <- ifelse(df_childhood_sunburn_occasions$childhood_sunburn_occasions_0 == 1, 0, df_childhood_sunburn_occasions$childhood_sunburn_occasions)
df_childhood_sunburn_occasions$childhood_sunburn_occasions <- ifelse(df_childhood_sunburn_occasions$childhood_sunburn_occasions_1 == 1, 1, df_childhood_sunburn_occasions$childhood_sunburn_occasions)
df_childhood_sunburn_occasions$childhood_sunburn_occasions <- ifelse(df_childhood_sunburn_occasions$childhood_sunburn_occasions_2 == 1, 2, df_childhood_sunburn_occasions$childhood_sunburn_occasions)


df_facial_ageing$facial_ageing <- ifelse(df_facial_ageing$facial_ageing_0 == 1, 0, df_facial_ageing$facial_ageing)
df_facial_ageing$facial_ageing <- ifelse(df_facial_ageing$facial_ageing_1 == 1, 1, df_facial_ageing$facial_ageing)
df_facial_ageing$facial_ageing <- ifelse(df_facial_ageing$facial_ageing_2 == 1, 2, df_facial_ageing$facial_ageing)


df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use <- ifelse(df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use_0 == 1, 0, df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use)
df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use <- ifelse(df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use_1 == 1, 1, df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use)
df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use <- ifelse(df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use_2 == 1, 2, df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use)


df_hair_colour_natural_before_greying$hair_colour_natural_before_greying <- ifelse(df_hair_colour_natural_before_greying$hair_colour_natural_before_greying_Black == 1, 'Black', df_hair_colour_natural_before_greying$hair_colour_natural_before_greying)
df_hair_colour_natural_before_greying$hair_colour_natural_before_greying <- ifelse(df_hair_colour_natural_before_greying$hair_colour_natural_before_greying_Fair == 1, 'Fair', df_hair_colour_natural_before_greying$hair_colour_natural_before_greying)
df_hair_colour_natural_before_greying$hair_colour_natural_before_greying <- ifelse(df_hair_colour_natural_before_greying$hair_colour_natural_before_greying_Red == 1, 'Red', df_hair_colour_natural_before_greying$hair_colour_natural_before_greying)


df_qualifications$qualifications <- ifelse(df_qualifications$qualifications_0 == 1, 0, df_qualifications$qualifications)
df_qualifications$qualifications <- ifelse(df_qualifications$qualifications_1 == 1, 1, df_qualifications$qualifications)
df_qualifications$qualifications <- ifelse(df_qualifications$qualifications_2 == 1, 2, df_qualifications$qualifications)


df_smoking_status$smoking_status <- ifelse(df_smoking_status$smoking_status_0 == 1, 0, df_smoking_status$smoking_status)
df_smoking_status$smoking_status <- ifelse(df_smoking_status$smoking_status_1 == 1, 1, df_smoking_status$smoking_status)
df_smoking_status$smoking_status <- ifelse(df_smoking_status$smoking_status_2 == 1, 2, df_smoking_status$smoking_status)


df_use_of_sun_uv_protection$use_of_sun_uv_protection <- ifelse(df_use_of_sun_uv_protection$use_of_sun_uv_protection_0 == 1, 0, df_use_of_sun_uv_protection$use_of_sun_uv_protection)
df_use_of_sun_uv_protection$use_of_sun_uv_protection <- ifelse(df_use_of_sun_uv_protection$use_of_sun_uv_protection_1 == 1, 1, df_use_of_sun_uv_protection$use_of_sun_uv_protection)
df_use_of_sun_uv_protection$use_of_sun_uv_protection <- ifelse(df_use_of_sun_uv_protection$use_of_sun_uv_protection_2 == 1, 2, df_use_of_sun_uv_protection$use_of_sun_uv_protection)
df_use_of_sun_uv_protection$use_of_sun_uv_protection <- ifelse(df_use_of_sun_uv_protection$use_of_sun_uv_protection_3 == 1, 3, df_use_of_sun_uv_protection$use_of_sun_uv_protection)

df_use_of_sun_uv_protection$use_of_sun_uv_protection <- ifelse(df_use_of_sun_uv_protection$use_of_sun_uv_protection_0 == 1, 0, df_use_of_sun_uv_protection$use_of_sun_uv_protection)
df_use_of_sun_uv_protection$use_of_sun_uv_protection <- ifelse(df_use_of_sun_uv_protection$use_of_sun_uv_protection_1 == 1, 1, df_use_of_sun_uv_protection$use_of_sun_uv_protection)
df_use_of_sun_uv_protection$use_of_sun_uv_protection <- ifelse(df_use_of_sun_uv_protection$use_of_sun_uv_protection_2 == 1, 2, df_use_of_sun_uv_protection$use_of_sun_uv_protection)
df_use_of_sun_uv_protection$use_of_sun_uv_protection <- ifelse(df_use_of_sun_uv_protection$use_of_sun_uv_protection_3 == 1, 3, df_use_of_sun_uv_protection$use_of_sun_uv_protection)


#as.factor
df_alcohol_drinker_status$alcohol_drinker_status <- as.factor(df_alcohol_drinker_status$alcohol_drinker_status)
df_alcohol_intake_frequency$alcohol_intake_frequency <- as.factor(df_alcohol_intake_frequency$alcohol_intake_frequency)

df_average_total_household_income_before$average_total_household_income_before_tax <- as.factor(df_average_total_household_income_before$average_total_household_income_before_tax)

df_childhood_sunburn_occasions$childhood_sunburn_occasions <- as.factor(df_childhood_sunburn_occasions$childhood_sunburn_occasions)
df_facial_ageing$facial_ageing <- as.factor(df_facial_ageing$facial_ageing)
df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use <- as.factor(df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use)
df_hair_colour_natural_before_greying$hair_colour_natural_before_greying <- as.factor(df_hair_colour_natural_before_greying$hair_colour_natural_before_greying)
df_qualifications$qualifications <- as.factor(df_qualifications$qualifications)
df_smoking_status$smoking_status <- as.factor(df_smoking_status$smoking_status)
df_use_of_sun_uv_protection$use_of_sun_uv_protection <- as.factor(df_use_of_sun_uv_protection$use_of_sun_uv_protection)

#Add recoded OHE variables back
df_final <- cbind(df_continuous, 
                  df_alcohol_drinker_status$alcohol_drinker_status,
                  df_alcohol_intake_frequency$alcohol_intake_frequency,
                  df_average_total_household_income_before$average_total_household_income_before_tax,
                  df_childhood_sunburn_occasions$childhood_sunburn_occasions,
                  df_facial_ageing$facial_ageing,
                  df_frequency_of_solarium_sunlamp_use$frequency_of_solarium_sunlamp_use,
                  df_hair_colour_natural_before_greying$hair_colour_natural_before_greying,
                  df_qualifications$qualifications,
                  df_smoking_status$smoking_status,
                  df_use_of_sun_uv_protection$use_of_sun_uv_protection)



#Rename columns
names(df_final)[77] <- 'alcohol_drinker_status'
names(df_final)[78] <- 'alcohol_intake_frequency'
names(df_final)[79] <- 'average_total_household_income_before_tax'
names(df_final)[80] <- 'childhood_sunburn_occasions'
names(df_final)[81] <- 'facial_ageing'
names(df_final)[82] <- 'frequency_of_solarium_sunlamp_use'
names(df_final)[83] <- 'hair_colour_natural_before_greying'
names(df_final)[84] <- 'qualifications'
names(df_final)[85] <- 'smoking_status'
names(df_final)[86] <- 'use_of_sun_uv_protection'
  
  

saveRDS(df_final, 'RF_imputed_outliers_matched_1to5_ReverseHotEncoded_PRS_010422.rds')
