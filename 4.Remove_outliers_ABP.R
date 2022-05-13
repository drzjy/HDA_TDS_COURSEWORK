library(tidyverse)
library(data.table)
library(janitor)
library(openxlsx)
library(finalfit)
library(lubridate)

options(scipen = 999)

ukb <- readRDS("rf_imputed_PRS.rds")

# Check variabes
table(ukb$final)
table(ukb$types_of_physical_activity_in_last_4_weeks, useNA = "ifany")
table(ukb$hair_colour_natural_before_greying, useNA = "ifany")
table(ukb$skin_colour, useNA = "ifany")

# Recode variables if not recoded----
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

# transform to factors
ukb$skin_colour <- as.factor(ukb$skin_colour)
ukb$hair_colour_natural_before_greying <- as.factor(ukb$hair_colour_natural_before_greying)
ukb$types_of_physical_activity_in_last_4_weeks <- factor( ukb$types_of_physical_activity_in_last_4_weeks)

str(ukb$final)
table(ukb$final, useNA = "ifany")

# filter numeric variables only----
ukb_num <- ukb %>% select_if(is.numeric)

ukb_num %>%
  keep(is.numeric) %>%                          # Keep only numeric columns
  gather() %>%                                  # Convert to key-value pairs
  ggplot(aes(value)) +                          # Plot the values
  facet_wrap(~ key, scales = "free") +        # In separate panels
  geom_histogram(aes(y = ..density..),          # Density on Y instead of the default"Count"
                 colour = 1, position = "identity", fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  theme_minimal()   

# Get name of skewed varables which need outliers removal

table(ukb$final)

ukb <- ukb %>% 
  filter(final != "other") %>% 
  select(order(colnames(.)))

colnames(ukb)

colnames_choices <- read_csv("colnames_choices.csv")

colnames_choices$remove[colnames_choices$colnames == "home_location_east_co_ordinate_rounded"] <- "n"
colnames_choices$remove[colnames_choices$colnames == "home_location_north_co_ordinate_rounded"] <- "n"

colnames_choices <- colnames_choices %>% filter(remove == "n")

names_ukb <- colnames(ukb)
names_c <- colnames_choices$colnames

cols_intersection <- intersect(names_ukb, names_c)

ukb <- ukb[, cols_intersection]

ukb <- ukb %>% 
  select(order(colnames(.)))

colnames(ukb)

# Choose skweed variables
names_ukb <- colnames(ukb)
names_out <- skewed_variab$colnames
cols_outliers <- intersect(names_ukb, names_out)
ukb_out <- ukb[, cols_outliers]
ukb_out <- ukb_out %>% select(order(colnames(.)))
colnames(ukb_out)

# Use outliers function
is_outlier <- function(x, na.rm = TRUE) {
  qs = quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  
  lowerq <- qs[1]
  upperq <- qs[2]
  iqr = upperq - lowerq 
  
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  
  # Return logical vector
  aux = x > extreme.threshold.upper | x < extreme.threshold.lower 
  aux[is.na(aux)] = FALSE
  aux
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    cat("Removing outliers in column: ", col, " \n")
    cat("now df has the following rows: ", nrow(df), " \n")
    index = !is_outlier(df[[col]])
    df <- df[index,]
  }
  df
}


df_filtered <- remove_outliers(ukb, names_out)

saveRDS(df_filtered, "ukb_0903_no_outliers.rds")

