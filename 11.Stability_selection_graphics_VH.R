rm(list=ls())

library(tidyverse)
library(ggplot2)

#Playaround with plotting stability selection
#Aim - sort by groups



#Load data
df_prop <- read.csv('lasso_prop.csv')
variable_match <- read.csv('variable_group.csv')

colnames(df_prop) <- c('variables', 'prop')

df <- left_join(df_prop, variable_match, by = "variables")

write.csv(df, 'selected_variables_and_groups.csv')

#df <- read.csv('selected_variables_and_groups.csv')

#subset >0.2
df<- subset(df, df$prop > 0.5)

#recorder variables column by value by proportion
df <- mutate(df, variables = fct_reorder(variables, Risk.factor.grouping))

#Plot with ggplot
myplot <- 
  ggplot(df, 
         aes(x = prop,
             y = variables, 
             fill = Risk.factor.grouping)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  geom_vline(xintercept = 0.80, linetype="dashed", size = 1) +
  theme_minimal()

myplot + labs(x = 'Selection Proportion',
              y = 'Variables') +
  guides(fill = guide_legend(title="Variable Group"))


ggsave('RF_stability_selection', width = 12, height = 6)