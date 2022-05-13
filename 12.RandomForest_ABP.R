library(tidyverse)
library(data.table)
library(janitor)
library(openxlsx)
library(finalfit)
library(lubridate)

library(randomForest)
library(ranger)
library(parsnip)
library(caret)
library(Boruta)   #pick best features based on RF

library(ggplot2)
library(cowplot)

library(corrplot)
library(smotefamily)

options(scipen = 999)

# Load data and redefined cases and controls

ukb <- readRDS("RF_imputed_outliers_matched_1to5_ReverseHotEncoded_PRS_010422.rds")

ukb <- ukb %>% 
  mutate(final = factor(final))

ukb$final <- fct_recode(ukb$final,"Controls" = "0", "Cases" = "1")

table(ukb$final, useNA = "ifany")

prop.table(table(ukb$final))

table(ukb$final)
table(ukb$hair_colour_natural_before_greying)

# Create small df

ukbs <- ukb %>% 
  select(-c("IDs","year_of_birth"))

table(ukb$smoking_status)
table(ukb$ever_smoked)

# Train and Test slip----

library(caret)
set.seed(3456)
trainIndex <- createDataPartition(ukbs$final, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

ukbTrain <- ukbs[ trainIndex,]
ukbTest  <- ukbs[-trainIndex,]

# Balance Data
library(ROSE)

prop.table(table(ukbs$final))

train_smote <- ovun.sample(final~., data = ukbTrain, method = "both", seed = 123)
train_smote <- train_smote$data

prop.table(table(train_smote$final))

# Divide predictors
outcome <- train_smote$final
predictors <- train_smote %>% select(-final)

colnames(predictors)

# Basic Model RF, 500 trees and mtry sqrt predictors ----

mtry = floor(sqrt(ncol(predictors)))

set.seed(123)
model_base <- randomForest(outcome~., predictors, mtry = floor(sqrt(ncol(predictors))), 
                           ntree = 500,
                           importance = TRUE)

table(outcome)
model_base

saveRDS(model_base, "RF_base.rds")

# Get variable importance ----
varImpPlot(model_base)

randomForest::importance(model_base)
imp = as.data.frame(randomForest::importance(model_base))
imp = cbind(vars = rownames(imp), imp)

# Clean names ----
names(imp$vars)

imp$vars <- gsub("\\_"," ", imp$vars)

imp$vars <- str_to_title(imp$vars)

imp$vars

imp$vars[8] <- "Calcium"
imp$vars[13] <- "Creatinin"
imp$vars[23] <- "Glucose"
imp$vars[73] <- "PRSs"

names(imp)
names(imp)[4] <- "Mean Decrease Accuracy"
names(imp)[5] <- "Mean Decrease Gini"

# Importance Plots
imp = imp[order(imp$'Mean Decrease Accuracy'), ]
imp$vars = factor(imp$vars, levels = unique(imp$vars))

barplot(imp$'Mean Decrease Accuracy', names.arg = imp$vars)

imp %>%
  pivot_longer(cols = matches("Mean")) %>%
  ggplot(aes(value, vars, fill = vars)) +
  geom_col() +
  geom_text(aes(label = round(value), x = 0.5 * value),
            size = 1.5,
            colour = "white") +
  facet_grid(. ~ name, scales = "free_x") +
  scale_x_continuous(expand = expansion(c(0, 0.04))) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 4)) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)

# Model using CV----
# Define the controltrain

library(caret)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid",
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

set.seed(1234)

# Run the model (the outcome and the predictors need to be in the SAME df)

rf_default <- train(final~.,
                    data = train_smote,
                    method = "rf",
                    metric = "ROC",
                    trControl = trControl)

# Print the results
print(rf_default)

saveRDS(rf_default, "cv_rf.rds")

# Predictions
trellis.par.set(caretTheme())
plot(rf_default) 

predictions <- predict(rf_default, ukbTest, type = 'prob')
predictions$result <- ifelse(predictions$cases >= 0.5, 1, 0)
predictions$result <- factor(predictions$result, levels = c(0,1), labels = c("Controls", "Cases"))

confusionMatrix(predictions$result, reference = ukbTest$final, mode = "prec_recall", positive = "Cases")

plot(varImp(rf_default, scale = FALSE))

# Plot2 ----
library(viridis)

rfcv <- rf_default$finalModel

varImp(rfcv)

randomForest::importance(rfcv)

impcv = as.data.frame(randomForest::importance(rfcv))
impcv = cbind(vars = rownames(impcv), impcv)
impcv = impcv[order(impcv$MeanDecreaseGini), ]
impcv$vars = factor(impcv$vars, levels = unique(impcv$vars))

impcv$vars <- gsub("\\_"," ", impcv$vars)

impcv$vars <- str_to_title(impcv$vars)

impcv$vars

impcv$vars[63] <- "Calcium"
impcv$vars[80] <- "Creatinin"
impcv$vars[71] <- "Glucose"
impcv$vars[94] <- "PRSs"
row.names(impcv) <- impcv$vars

impcv$vars = factor(impcv$vars, levels = unique(impcv$vars))

impcv = rename(impcv, "Mean Decrease Gini" = "MeanDecreaseGini")

impcv_pivot <- impcv  %>% 
  pivot_longer(cols = matches("Mean")) %>%
  arrange(desc(value)) 

impcv_pivot %>% 
  arrange(desc(value)) %>% 
  ggplot(aes(value, vars, fill = vars)) +
  geom_col() +
  geom_text(aes(label = round(value), x = 0.5 * value),
            size = 1.5,
            colour = "white") +
  facet_grid(. ~ name) +
  scale_x_continuous(expand = expansion(c(0, 0.04))) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 4)) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)
