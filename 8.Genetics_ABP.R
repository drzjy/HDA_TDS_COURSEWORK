library(tidyverse)
library(data.table)
library(janitor)
library(openxlsx)
library(finalfit)
library(lubridate)

options(scipen = 999)

# Load data ----

genetic <- genetic <- readRDS("genetic_data.rds")

# Genetic 1 ----- 
# order genetic data by rownames and by columnames(SNPs) in alphabetic order
genetic <- genetic[sort(rownames(genetic)),]

genetic <- as.data.frame(genetic) %>% 
  select(order(colnames(.)))

# Create a row / ID variable
genetic$row <- row.names(genetic)
genetic$row <- as.numeric(genetic$row)
str(genetic$row)

# Mutate all values in the df to numbers
genetic <- genetic %>% 
  mutate_all(as.numeric)

# remove all the odds rows 
genetic <- genetic %>% 
  filter(row > 0) %>% 
  relocate(row)

row.names(genetic) <- genetic$row

saveRDS(genetic, "genetic_1.rds")

sum(is.na(genetic$row) == TRUE) 

# Recode all SNPs (0 to NA, 1 to 0, 2 to 1 etc)
geneticNA <- genetic %>% 
  mutate(across(where(is.numeric), ~na_if(., "0"))) 

geneticNA[geneticNA == "1"] <- 0
geneticNA[geneticNA == "2"] <- 1
geneticNA[geneticNA == "3"] <- 2

saveRDS(geneticNA, "genetic_NA.rds")


## Genetic NA ----
geneticNA <- readRDS("genetic_NA.rds")

# How many NAs per row & column exist

table(rowSums(is.na(geneticNA)))
table(colSums(is.na(geneticNA)))

# Create variable with number NAs per row
geneticNA$sumNA <- apply(geneticNA, MARGIN = 1, function(x) sum(is.na(x)))

# Remove rows with > 2 NAs for SNPs
geneticNA <- geneticNA %>% 
  filter(sumNA <= 3)

table(geneticNA$sumNA)

geneticNA <- geneticNA %>% 
  mutate(n_SNP = 54 - sumNA)

summary(geneticNA$n_SNP)

saveRDS(geneticNA, "genetic_NA_prepared.rds")

# Create genetic_complete (dropNAs)----
genetic_complete <- geneticNA %>% drop_na() 
row.names(genetic_complete) <- genetic_complete$rownames
genetic_complete <- genetic_complete %>% select(-rownames)

# Arrange Nature.Excelfile----

library(readxl)
nature <- read_excel("GeneticNature_recoded2.xlsx", 
                     sheet = "Nature&UKB")

nature <- nature %>% 
  arrange(SNP) %>% 
  clean_names()

nature <- nature %>% 
  select(chromosome_posi, snp, effect_allele_single_snp, or_1_allele, beta_change)

nature$log <- log(nature$or_1_allele)

nature$log_final <- ifelse(nature$beta_change == 0, nature$log, nature$log*-1 )

# create the vector of weights for SNPs
logs <- nature$log_final

# Create score points or each patient----

genetic_info <- geneticNA %>% 
  select(row, sumNA, n_SNP)

geneticNA_matrix <- geneticNA %>% 
  select(-c(row, sumNA, n_SNP))

geneticNA_matrix <- as.matrix(geneticNA_matrix)
head(geneticNA_matrix)
dim(geneticNA_matrix)

score <- sweep(geneticNA_matrix, 2, logs, "*")


dim(score)
head(score)
score <- as.data.frame(score)

# Sum of all rows
score$sumSNPs <- rowSums(score, na.rm = TRUE)

# Add all variables and calculate score/numberSNPS
score <- cbind(score, genetic_info)

score <- score %>% 
  mutate(PRS = sumSNPs/n_SNP)

saveRDS(score, "score_complete.rds")

score <- readRDS("score_complete.rds")

# Match score with ukb
ukb <- readRDS("ukb_0903_no_outliers.rds")

ukb <- ukb %>% 
  select(final, sex)

table(ukb$final)

# PRS Standarterize

score <- score %>%
  select(PRS)

score$final = ukb[match(row.names(score), row.names(ukb)),"final"]
score$sex = ukb[match(row.names(score), row.names(ukb)),"sex"]

score %>% 
  group_by(final) %>% 
  summarise(mean = mean(PRS, na.rm = TRUE),
            sd = sd(PRS, na.rm = TRUE))

# cases; mean: -0.07745597, SD: 0.01138872
# controls: mean; -0.08403310, SD: 0.01325923

score <- score %>% 
  mutate(PRSs = (PRS - (-0.08403310))/0.01325923)  #use mean and SD of controls


saveRDS(score, "score_final.rds")


# Apply the score to final dataframe
ukb2 <- readRDS("rf_imputed_ukb_1_to_5_hotencond_18032022.rds")

score <- score[sort(rownames(score)),]
ukb2 <- ukb2[sort(rownames(ukb2)),]

ukb2$PRS = score[match(row.names(ukb2), row.names(score)),"PRS"]
ukb2$PRSs = score[match(row.names(ukb2), row.names(score)),"PRSs"]

summary(ukb2$PRSs)

saveRDS(ukb2, "outliers_matched_1to5_Hotencoded_PRS") 

# Plot score----

score <- score %>% 
  mutate(final = factor(final))

score$final <- fct_relevel(score$final, "controls", "cases")

ukb2 %>% 
  drop_na() %>% 
  ggplot(aes(x = PRS, colour = final)) + 
  geom_density(alpha = 0.2)

# Basic glm
glmPRS <- glm(final ~ PRS + sex, family = "binomial", data = ukb2)

summary(glmPRS)


# Check missings in cases and controls

outliers_matched_1to5_Hotencoded_PRS <- readRDS("outliers_matched_1to5_Hotencoded_PRS.RDS")
rf_imputed_PRS <- readRDS("rf_imputed_PRS.rds")

summary(rf_imputed_PRS$PRSs)
summary(outliers_matched_1to5_Hotencoded_PRS$PRSs)

outliers_matched_1to5_Hotencoded_PRS$missing <- ifelse(is.na(outliers_matched_1to5_Hotencoded_PRS$PRSs), "Mis", "PRS")
outliers_matched_1to5_Hotencoded_PRS$final <- ifelse((outliers_matched_1to5_Hotencoded_PRS$final == 1), "Cases", "Controls")

table(outliers_matched_1to5_Hotencoded_PRS$missing, outliers_matched_1to5_Hotencoded_PRS$final, useNA = "ifany")
