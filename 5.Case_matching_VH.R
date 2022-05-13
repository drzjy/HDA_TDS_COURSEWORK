rm(list=ls())

library(tidyverse)
library(MatchIt)
library(arsenal)

#Load DF to Match Case:Controls
df <- readRDS("ukb_complete_hotencond_18032022.rds")

#df$final
df$final <- ifelse(df$final == 'cases', 1, 0)

#Round coordinates to 1 significant figure (i.e. round the nearest 100km, NB: original coordinates are in metres)
#Create new columns where values are banded to nearest 100km
df$home_location_east_coord_100km <- signif((df$home_location_east_co_ordinate_rounded/1000), 1)
df$home_location_north_coord_100km <- signif((df$home_location_north_co_ordinate_rounded/1000), 1)

#Select for only complete cases with respect to matching variables (sex, age_at_recruitment, home_location_eat_coord_100km, home_location_north_coord_100km)
#NB: This removes two CASES which have NA values in ethnic_background
#df(df$IDs == 2087198 & df$IDs == 4704235)

df <- df[complete.cases(df[ , c('sex','age_when_attended_assessment_centre', 
                                'ethnic_background', 
                                'home_location_east_coord_100km',
                                'home_location_north_coord_100km')]), ] 

t0 <- Sys.time()
#match case and controls
df_match <- matchit(final ~ sex + age_when_attended_assessment_centre +
                      ethnic_background + 
                      home_location_east_coord_100km + 
                      home_location_north_coord_100km, 
                    exact = ~ sex + ethnic_background,
                    data = df,
                    ratio=1)

t1 <- Sys.time()
print(t1-t0)

#match.matrix
head(df_match$match.matrix)

#extract matched data into a dataframe
df_match_extract <- match.data(df_match)

#arsenal - table 1
tab1 <- tableby(final ~ sex + age_when_attended_assessment_centre +
                  ethnic_background + 
                  home_location_east_coord_100km + 
                  home_location_north_coord_100km,
                data = df_match_extract)
summary(tab1, text=TRUE)

#Remove redundant columns added by matchit()
df_match_extract$weights <- NULL
df_match_extract$distance <- NULL
df_match_extract$subclass <- NULL

df_match_extract$home_location_east_coord_100km <- NULL
df_match_extract$home_location_north_coord_100km <- NULL


#Save final matched dataset
saveRDS(df_match_extract, "/CC_matched_hotencoded_1to1_190322.rds")
