rm(list=ls())

library(tidyverse)
library(data.table)
library(finalfit)
library(janitor)
library(openxlsx)
library(lubridate)
library(dplyr)

options(scipen = 999)

ukb <- readRDS("RF_imputed_outliers_matched_1to5_ReverseHotEncoded_PRS_010422.rds")



#GLM model
Lasso <- read.csv("lasso_prop.csv")
   
View(Lasso)

Lasso <- Lasso %>% 
  clean_names() %>% 
  filter(x_2 >= 0.8)

lasso_vector <- Lasso$x
view(lasso_vector)

names.use <- names(ukb)[(names(ukb) %in% lasso_vector)]
names(ukb_fin)

ukb.subset <- ukb[, names.use]

setdiff(lasso_vector, names.use)

# Add variables not included in the lasso vector because they were outcome, matching or hot-enconded with dfferent names

ukb_extra <- ukb %>%
  select(final, age_when_attended_assessment_centre, sex, 
         home_location_east_co_ordinate_rounded,
         home_location_north_co_ordinate_rounded,
         average_total_household_income_before_tax,
         hair_colour_natural_before_greying,
         use_of_sun_uv_protection, 
         childhood_sunburn_occasions,
         alcohol_drinker_status,
         smoking_status, 
         frequency_of_solarium_sunlamp_use)

ukb_fin <- merge.data.frame(ukb_extra, ukb.subset, by = "row.names",all.x = TRUE)

row.names(ukb_fin) <- ukb_fin$Row.names

ukb_fin <- ukb_fin %>% select(-c(Row.names, current_tobacco_smoking, 
                                 ethnic_background, sex,
                                 age_when_attended_assessment_centre, 
                                 home_location_east_co_ordinate_rounded,
                                 home_location_north_co_ordinate_rounded))
names(ukb_fin)

table(ukb_fin$final)

ukb_fin <- ukb_fin %>% 
  mutate(final = factor(final))

#Final multivariate logistic regression model
glmmodel <- glm(final ~ ., data = ukb_fin, family = "binomial")
summary(glmmodel)

#Check univariate association
glmmodel1 <- glm(final ~ reticulocyte_count, data = ukb_fin, family = "binomial")
summary(glmmodel1)




library(broomExtra)
summary(glmmodel)
tidy(glmmodel)
#calculate odds ratio
exp(coef(glmmodel))

OR <- exp(cbind(coef(glmmodel), confint(glmmodel)))
glance(glmmodel)



# Forest Plot

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())

plot_model(glmmodel, 
           sort.est = TRUE,
           show.values = TRUE, 
           value.offset = 0.4, 
           value.size = 4,
           #vline.color = "blue",
           colors = "social") +
  #ylim(-1, 5) +    #labels start with more significant
  ggtitle("Multivariate Logistic Regression Model with Selected Variables")









#axis.labels = c("Ethnic Background", "Reticulocyte Count", "PRSs")


#AUC glmmodel <- glm(final ~., data = ukb_fin, family = "binomial")

#split to test and train data
library(caTools)
set.seed(31)
split <- sample.split(ukb_fin$final, SplitRatio = 0.7)
train <- subset(ukb_fin, split==TRUE)
test <- subset(ukb_fin, split==FALSE)

#fit logistic regression of development of melanoma with all variables
names(ukb_fin)
melanoma_pred <- glm(final ~ ., data = train, family=binomial)
summary(melanoma_pred)

#predict on test set
predictTest <- predict(melanoma_pred, type="response", newdata=test)

#confusion matrix
table(test$final, predictTest > 0.5)

#plot AUC
library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve: All variables", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")


#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################


###########################
##### ONE GROUP AUC #####
###########################


#1. fit logistic regression of development of melanoma with PRS
melanoma_pred_PRS <- glm(final ~ PRSs, data = train, family=binomial)
summary(melanoma_pred_PRS)

predictTest <- predict(melanoma_pred_PRS, type="response", newdata=test)

table(test$final, predictTest > 0.5)

library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve: PRSs", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")

###########################
##### TWO GROUP AUC #####
###########################

#2. fit logistic regression of development of melanoma with BIOLOGICAL variables
melanoma_pred_bio <- glm(final ~ PRSs + reticulocyte_count + phosphate + neutrophill_count + total_protein +
                           igf + glucose2 + eosinophill_count + monocyte_count + neutrophill_count + vitamin_d + testosterone,
                         data = train, family=binomial)
summary(melanoma_pred_bio)

predictTest <- predict(melanoma_pred_bio, type="response", newdata=test)

table(test$final, predictTest > 0.5)

library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")


plot(perf1, add = TRUE, col = 'black')

#3. fit logistic regression of development of melanoma with BASELINE variables
melanoma_pred_base <- glm(final ~ PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                       average_total_household_income_before_tax + weight + year_of_birth +
                       standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                       hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in, data = train, family=binomial)
summary(melanoma_pred_base)

predictTest <- predict(melanoma_pred_base, type="response", newdata=test)

table(test$final, predictTest > 0.5)

library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")


#4. fit logistic regression of development of melanoma with EXPOSURE variables
melanoma_pred_exp <- glm(final ~ PRSs + childhood_sunburn_occasions + close_to_major_road +
                       greenspace_percentage_buffer_1000m + traffic_intensity_on_the_nearest_major_road +
                       types_of_transport_used_excluding_work, data = train, family=binomial)
summary(melanoma_pred_exp)

predictTest <- predict(melanoma_pred_exp, type="response", newdata=test)

table(test$final, predictTest > 0.5)

library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")


#5. fit logistic regression of development of melanoma with LIFESTYLE/BEHAVIOR variables
melanoma_pred_bev <- glm(final ~ PRSs + use_of_sun_uv_protection + alcohol_drinker_status + smoking_status +
                           frequency_of_solarium_sunlamp_use + getting_up_in_morning +
                           lamb_mutton_intake + morning_evening_person_chronotype + non_oily_fish_intake, 
                         data = train, family=binomial)
summary(melanoma_pred_bev)

predictTest <- predict(melanoma_pred_bev, type="response", newdata=test)

table(test$final, predictTest > 0.5)

library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")

###########################
##### THREE GROUP AUC #####
###########################

#fit logistic regression of development of melanoma with PRS, BASELINE, BIOLOGICAL variables
melanoma_pred_bio <- glm(final ~ PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                           average_total_household_income_before_tax + weight + year_of_birth +
                           standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                           hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in + 
                           reticulocyte_count + phosphate + neutrophill_count + total_protein +
                           igf + glucose2 + eosinophill_count + monocyte_count + neutrophill_count + vitamin_d +
                           testosterone,
                         data = train, family=binomial)
summary(melanoma_pred_bio)

predictTest <- predict(melanoma_pred_bio, type="response", newdata=test)

table(test$final, predictTest > 0.5)

library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")


#fit logistic regression of development of melanoma with PRS, BASELINE, EXPOSURE variables
melanoma_pred_exp <- glm(final ~ PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                           average_total_household_income_before_tax + weight + year_of_birth +
                           standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                           hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in + 
                           childhood_sunburn_occasions + close_to_major_road +
                           greenspace_percentage_buffer_1000m + traffic_intensity_on_the_nearest_major_road +
                           types_of_transport_used_excluding_work, data = train, family=binomial)
summary(melanoma_pred_exp)

predictTest <- predict(melanoma_pred_exp, type="response", newdata=test)

table(test$final, predictTest > 0.5)

library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")


#fit logistic regression of development of melanoma with PRS, BASELINE, LIFESTYLE/BEHAVIOR variables
melanoma_pred_bev <- glm(final ~ PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                           average_total_household_income_before_tax + weight + year_of_birth +
                           standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                           hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in + 
                           use_of_sun_uv_protection + alcohol_drinker_status + smoking_status +
                           frequency_of_solarium_sunlamp_use + getting_up_in_morning +
                           lamb_mutton_intake + morning_evening_person_chronotype + non_oily_fish_intake, 
                         data = train, family=binomial)
summary(melanoma_pred_bev)

predictTest <- predict(melanoma_pred_bev, type="response", newdata=test)

table(test$final, predictTest > 0.5)

library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")

###########################
##### FOUR GROUP AUC #####
###########################

#fit logistic regression of development of melanoma with PRS, BASELINE, BIOLOGICAL, EXPOSURE variables
melanoma_pred_exp <- glm(final ~ PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                           average_total_household_income_before_tax + weight + year_of_birth +
                           standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                           hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in + 
                           reticulocyte_count + phosphate + neutrophill_count + total_protein +
                           igf + glucose2 + eosinophill_count + monocyte_count + neutrophill_count + vitamin_d +
                           testosterone +
                           childhood_sunburn_occasions + close_to_major_road +
                           greenspace_percentage_buffer_1000m + traffic_intensity_on_the_nearest_major_road +
                           types_of_transport_used_excluding_work, data = train, family=binomial)
summary(melanoma_pred_exp)

predictTest <- predict(melanoma_pred_exp, type="response", newdata=test)

table(test$final, predictTest > 0.5)

library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")


#fit logistic regression of development of melanoma with PRS, BASELINE, BIOLOGICAL, LIFESTYLE/BEHAVIOR variables
melanoma_pred_bev <- glm(final ~ PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                           average_total_household_income_before_tax + weight + year_of_birth +
                           standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                           hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in + 
                           reticulocyte_count + phosphate + neutrophill_count + total_protein +
                           igf + glucose2 + eosinophill_count + monocyte_count + neutrophill_count + vitamin_d +
                           testosterone +
                           use_of_sun_uv_protection + alcohol_drinker_status + smoking_status +
                           frequency_of_solarium_sunlamp_use + getting_up_in_morning +
                           lamb_mutton_intake + morning_evening_person_chronotype + non_oily_fish_intake, 
                         data = train, family=binomial)
summary(melanoma_pred_bev)

predictTest <- predict(melanoma_pred_bev, type="response", newdata=test)

table(test$final, predictTest > 0.5)

library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")

########### all variables ###############
#fit logistic regression of development of melanoma with ALL variables
melanoma_pred_exp <- glm(final ~ PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                           average_total_household_income_before_tax + weight + year_of_birth +
                           standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                           hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in + 
                           reticulocyte_count + phosphate + neutrophill_count + total_protein +
                           igf + glucose2 + eosinophill_count + monocyte_count + neutrophill_count + vitamin_d +
                           testosterone +
                           childhood_sunburn_occasions + close_to_major_road +
                           greenspace_percentage_buffer_1000m + traffic_intensity_on_the_nearest_major_road +
                           types_of_transport_used_excluding_work +
                           use_of_sun_uv_protection + alcohol_drinker_status + smoking_status +
                           frequency_of_solarium_sunlamp_use + getting_up_in_morning +
                           lamb_mutton_intake + morning_evening_person_chronotype + non_oily_fish_intake, 
                          data = train, family=binomial)
summary(melanoma_pred_exp)

predictTest <- predict(melanoma_pred_exp, type="response", newdata=test)

table(test$final, predictTest > 0.5)

library(ROCR)
ROCRpred <- prediction(predictTest, test$final)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf, main = "ROC curve", col= "red")
abline(a = 0, b=1)

auc <- performance(ROCRpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.6, .2, auc, title = "AUC")


###########################################
###### STEPWISE (REAL) AUC ######
###########################################

#stepwise AUC - all variables

#null model with no predictors
null_model <- glm(final ~ 1, data = ukb_fin, family = "binomial")

#full model using all potential predictors
full_model <- glm(final ~PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                    average_total_household_income_before_tax + weight + year_of_birth +
                    standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                    hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in +
                    reticulocyte_count + phosphate + neutrophill_count + total_protein +
                    igf + glucose2 + eosinophill_count + monocyte_count + neutrophill_count +
                    vitamin_d + testosterone + childhood_sunburn_occasions + close_to_major_road +
                    greenspace_percentage_buffer_1000m + traffic_intensity_on_the_nearest_major_road +
                    types_of_transport_used_excluding_work + use_of_sun_uv_protection + alcohol_drinker_status +
                    smoking_status + frequency_of_solarium_sunlamp_use + getting_up_in_morning +
                    lamb_mutton_intake + morning_evening_person_chronotype + non_oily_fish_intake,
                  data = ukb_fin, family = "binomial")

#forward stepwise
step_model <- step(null_model, scope=list(lower=null_model, upper=full_model))

#stepwise donation probability
step_prob <- predict(step_model, type="response")

#plot ROC
library(pROC)
ROC <- roc (ukb_fin$final, step_prob)
plot(ROC, col = "red", main="Stepwise ROC curve", print.auc=TRUE)
auc(ROC)



#1. stepwise AUC - PRS and baseline

#null model with no predictors
null_model <- glm(final ~ 1, data = ukb_fin, family = "binomial")

#full model using all potential predictors
full_model <- glm(final ~ PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                    average_total_household_income_before_tax + weight + year_of_birth +
                    standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                    hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in,
                  data = ukb_fin, family = "binomial")

#forward stepwise
step_model <- step(null_model, scope=list(lower=null_model, upper=full_model))

#stepwise donation probability
step_prob <- predict(step_model, type="response")

#plot ROC
library(pROC)
ROC <- roc (ukb_fin$final, step_prob)
plot(ROC, col = "red", main="Stepwise ROC curve: PRSs + Baseline Characteristics", print.auc=TRUE)
auc(ROC)


#2. stepwise AUC - PRS and biological

#null model with no predictors
null_model <- glm(final ~ 1, data = ukb_fin, family = "binomial")

#full model using all potential predictors
full_model <- glm(final ~ PRSs + reticulocyte_count + phosphate + neutrophill_count + total_protein +
                    igf + glucose2 + eosinophill_count + monocyte_count + neutrophill_count + vitamin_d + testosterone,
                  data = ukb_fin, family = "binomial")

#forward stepwise
step_model <- step(null_model, scope=list(lower=null_model, upper=full_model))

#stepwise donation probability
step_prob <- predict(step_model, type="response")

#plot ROC
library(pROC)
ROC <- roc (ukb_fin$final, step_prob)
plot(ROC, col = "red", main="Stepwise ROC curve", print.auc=TRUE)
auc(ROC)



#3. stepwise AUC - PRS and exposure

#null model with no predictors
null_model <- glm(final ~ 1, data = ukb_fin, family = "binomial")

#full model using all potential predictors
full_model <- glm(final ~ PRSs + childhood_sunburn_occasions + close_to_major_road +
                    greenspace_percentage_buffer_1000m + traffic_intensity_on_the_nearest_major_road +
                    types_of_transport_used_excluding_work, data = ukb_fin, family = "binomial")

#forward stepwise
step_model <- step(null_model, scope=list(lower=null_model, upper=full_model))

#stepwise donation probability
step_prob <- predict(step_model, type="response")

#plot ROC
library(pROC)
ROC <- roc (ukb_fin$final, step_prob)
plot(ROC, col = "red", main="Stepwise ROC curve", print.auc=TRUE)
auc(ROC)


#4. stepwise AUC - PRS and lifestyle/behavior

#null model with no predictors
null_model <- glm(final ~ 1, data = ukb_fin, family = "binomial")

#full model using all potential predictors
full_model <- glm(final ~ PRSs + use_of_sun_uv_protection + alcohol_drinker_status + smoking_status +
                    frequency_of_solarium_sunlamp_use + getting_up_in_morning +
                    lamb_mutton_intake + morning_evening_person_chronotype + non_oily_fish_intake,
                  data = ukb_fin, family = "binomial")

#forward stepwise
step_model <- step(null_model, scope=list(lower=null_model, upper=full_model))

#stepwise donation probability
step_prob <- predict(step_model, type="response")

#plot ROC
library(pROC)
ROC <- roc (ukb_fin$final, step_prob)
plot(ROC, col = "red", main="Stepwise AUC by Variable Groups", print.auc=TRUE)
auc(ROC)




############################################################################
############################################################################
############################################################################
############################################################################


#Sequential AUC plot

#1. fit logistic regression of development of melanoma with PRS
melanoma_pred_PRS <- glm(final ~ PRSs, data = train, family=binomial)
summary(melanoma_pred_PRS)

predictTest1 <- predict(melanoma_pred_PRS, type="response", newdata=test)

table(test$final, predictTest1 > 0.5)

library(ROCR)
ROCRpred1 <- prediction(predictTest1, test$final)
auc1 <- as.numeric(performance(ROCRpred1, "auc")@y.values)
perf1 <- performance(ROCRpred1,"tpr","fpr")

plot(perf1, 
     main = "Stepwise AUC by Variable Groups", 
     col= "orange",
     lwd = 1)
abline(a = 0, b=1)

#legend(.6, .2, auc1, title = "AUC")


#2. fit logistic regression of development of melanoma with BASELINE variables
melanoma_pred_base <- glm(final ~ PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                            average_total_household_income_before_tax + weight + year_of_birth +
                            standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                            hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in, data = train, family=binomial)
summary(melanoma_pred_base)

predictTest2 <- predict(melanoma_pred_base, type="response", newdata=test)

table(test$final, predictTest2 > 0.5)

library(ROCR)
ROCRpred2 <- prediction(predictTest2, test$final)
auc2 <- as.numeric(performance(ROCRpred2, "auc")@y.values)
perf2 <- performance(ROCRpred2,"tpr","fpr")

plot(perf2, 
     main = "Stepwise AUC by Variable Groups", 
     add = TRUE,
     col= "red",
     lwd = 1)
abline(a = 0, b=1)



#3. fit logistic regression of development of melanoma with PRS, BASELINE, BIOCHEM variables
melanoma_pred_bio <- glm(final ~ PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                           average_total_household_income_before_tax + weight + year_of_birth +
                           standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                           hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in + 
                           reticulocyte_count + phosphate + neutrophill_count + total_protein +
                           igf + glucose2 + eosinophill_count + monocyte_count + neutrophill_count + vitamin_d +
                           testosterone,
                         data = train, family=binomial)
summary(melanoma_pred_bio)

predictTest3 <- predict(melanoma_pred_bio, type="response", newdata=test)

table(test$final, predictTest3 > 0.5)

library(ROCR)
ROCRpred3 <- prediction(predictTest3, test$final)
auc3 <- as.numeric(performance(ROCRpred, "auc")@y.values)
perf3 <- performance(ROCRpred3,"tpr","fpr")

plot(perf3, 
     main = "Stepwise AUC by Variable Groups", 
     add = TRUE,
     col= "blue",
     lwd = 1)
abline(a = 0, b=1)


#4. fit logistic regression of development of melanoma with PRS, BASELINE, BIOLOGICAL, EXPOSURE variables
melanoma_pred_exp <- glm(final ~ PRSs + country_of_birth_uk_elsewhere + current_employment_status + 
                           average_total_household_income_before_tax + weight + year_of_birth +
                           standing_height + skin_colour + systolic_blood_pressure_automated_reading +
                           hair_colour_natural_before_greying + own_or_rent_accommodation_lived_in + 
                           reticulocyte_count + phosphate + neutrophill_count + total_protein +
                           igf + glucose2 + eosinophill_count + monocyte_count + neutrophill_count + vitamin_d +
                           testosterone +
                           childhood_sunburn_occasions + close_to_major_road +
                           greenspace_percentage_buffer_1000m + traffic_intensity_on_the_nearest_major_road +
                           types_of_transport_used_excluding_work, data = train, family=binomial)
summary(melanoma_pred_exp)

predictTest4 <- predict(melanoma_pred_exp, type="response", newdata=test)

table(test$final, predictTest4 > 0.5)

library(ROCR)
ROCRpred4 <- prediction(predictTest4, test$final)
auc4 <- as.numeric(performance(ROCRpred4, "auc")@y.values)
perf4 <- performance(ROCRpred4,"tpr","fpr")

plot(perf4, 
     main = "Stepwise AUC by Variable Groups", 
     add = TRUE,
     col= "darkgreen",
     lwd = 1)
abline(a = 0, b=1)


#5. fit full model
melanoma_pred_exp <- glm(final ~ ., data = train, family=binomial)
summary(melanoma_pred_exp)

predictTest5 <- predict(melanoma_pred_exp, type="response", newdata=test)

table(test$final, predictTest5 > 0.5)

library(ROCR)
ROCRpred5 <- prediction(predictTest5, test$final)
auc5 <- as.numeric(performance(ROCRpred5, "auc")@y.values)
perf5 <- performance(ROCRpred5,"tpr","fpr")

plot(perf5, main = "Stepwise AUC by Variable Groups", 
     #add=TRUE, 
     col= "purple", 
     lwd = 4,
     add=TRUE)
abline(a = 0, b=1)

#auc5 <- round(auc5, 3)
#legend(0.6, 0.2 , title = "AUC", legend = auc5, col = "black", lwd = 3)

