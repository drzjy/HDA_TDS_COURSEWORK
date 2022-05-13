rm(list=ls())
path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
getwd()


#loading packages
library("readr")
library(dplyr)
library(tibble)
library(naniar)

##read dcancer data

cancer <- readRDS("cancer1.rds")


cancer$cancer <- ifelse(is.na(cancer$cancer), "no_cancer", "cancer")

colnames(cancer)

cancer$cancertype <- ifelse(cancer$cancer != "no_cancer" & cancer$cancer1 <= cancer$date_of_attending_assessment_centre_0_0, "prevalent", "no_cancer")
cancer$cancertype <- ifelse(cancer$cancer != "no_cancer" & cancer$cancer1 > cancer$date_of_attending_assessment_centre_0_0, "incident", cancer$cancertype)

cancer1 <- as.data.frame(t(cancer))
cancer2 <- as.data.frame(t(cancer1[, cancer1["cancertype", ] != "prevalent"]))



##re-arrange columns that follow this pattern: "date_of_cancer_diagnosis_0_0", "type_of_cancer_icd10_0_0", "type_of_cancer_icd9_0_0" 

cancer2 <- cancer2 %>% select(-c("date_of_attending_assessment_centre_0_0", "date_of_attending_assessment_centre_1_0", "date_of_attending_assessment_centre_2_0","date_of_attending_assessment_centre_3_0", "cancer1" ))


instance_array <- c('_0_0', '_1_0','_2_0', '_3_0','_4_0', '_5_0','_6_0', '_7_0','_8_0', '_9_0','_10_0', '_11_0','_12_0', '_13_0','_14_0', '_15_0', '_16_0', "cancertype")


cancer_ordered <- cancer2 %>%
  select(contains(instance_array))


##remove columns with icd9

cancer_ordered_icd10 <- cancer_ordered %>%
  select(-contains(c("icd9")))


##drop columns having NA in each row
cancer_ordered_cleaned <- cancer_ordered_icd10 %>% 
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )

##transpose cancer_ordered_cleaned and drop cols having NA in each row

cancer_ordered_cleaned1 <- as.data.frame(t(cancer_ordered_cleaned)) %>% 
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )


date_df <- select(as.data.frame(t(cancer_ordered_cleaned1)),contains(c("date")))

date_df <- as.data.frame(t(date_df)) %>% 
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )

date_df <- as.data.frame(t(date_df))

type_df <- select(as.data.frame(t(cancer_ordered_cleaned1)),contains(c("type","cancertype")))

new_type_df <- type_df[rownames(date_df), ] ##subsetting date_df using rownames of date_df

write.csv(date_df, "date.csv")
write.csv(new_type_df, "type.csv")

##matching

cases <- read_csv("out1.csv")

controls <- type_df %>% filter(cancertype == "no_cancer")

melanoma.allcases <- cases %>% filter(grepl('Melanoma',ignore.case = TRUE, Cancer_description))

melanoma.allcases <- as.data.frame(melanoma.allcases)

rownames(melanoma.allcases) <- (melanoma.allcases)[,1]

melanoma.allcases <- melanoma.allcases[, c(2, 3, 4)]

tmp3 <- cancer[rownames(melanoma.allcases),]

melanoma.allcases$first_assessment_date <- tmp3$date_of_attending_assessment_centre_0_0
melanoma.allcases$newDate6months <- melanoma.allcases$first_assessment_date + 180
melanoma.allcases$newDate10yrss <- melanoma.allcases$first_assessment_date + 3650

##select cases who developed cancer between 0.5 and 10 years
tmp4 <- melanoma.allcases[melanoma.allcases$First_Diagnose_Date > melanoma.allcases$newDate6months, ]
cases_6m_and_10yrs <- melanoma.allcases[melanoma.allcases$First_Diagnose_Date < melanoma.allcases$newDate10yrss, ]

####ukb dataset

ukb <- readRDS("ukb_finalV2.rds")

ukb_keep_rows_cases <- as.vector(rownames(cases_6m_and_10yrs))

ukb_keep_rows_controls <- as.vector(rownames(controls))

###prepare ukb_subset data for 2802 cases and 391364 controls
#ukb_subset_cases <- ukb[rownames(ukb) %in% ukb_keep_rows, ]
#ukb_subset_controls <- ukb[rownames(ukb) %in% ukb_keep_rows_controls, ]

ukb <- ukb %>%
  add_column(case_control = 0, .before="sex_0_0")

##subset ukb_case df and assign 1 in case_control column
ukb_case <- ukb[rownames(ukb) %in% ukb_keep_rows_cases, ]
ukb_case["case_control"][ukb_case["case_control"] == 0] <- 1
ukb_case1 <- tibble::rownames_to_column(ukb_case, "IDs")

##subset ukb_control df and assign 2 in case_control column
ukb_control <- ukb[rownames(ukb) %in% ukb_keep_rows_controls, ]
ukb_control["case_control"][ukb_control["case_control"] == 0] <- 2
ukb_control1 <- tibble::rownames_to_column(ukb_control, "IDs")

##subset ukb_others df and assign 0 in case_control column

ukb_rownames <- as.vector(rownames(ukb))
ukb_cases_controls <- c(ukb_keep_rows_cases, ukb_keep_rows_controls)


ukb_keep_rows_others <- as.vector(setdiff(ukb_rownames,ukb_cases_controls))
ukb_others <- ukb[rownames(ukb) %in% ukb_keep_rows_others, ]
ukb_others1 <- tibble::rownames_to_column(ukb_others, "IDs")


##stack ukb_case1, ukb_control1 and ukb_others1 to make the final df

ukb_final <- rbind(ukb_case1, ukb_control1, ukb_others1)

saveRDS(ukb_final, "ukb_final_220322.rds")

library('stringr')


ukb_final_read <- readRDS("ukb_final_220322.rds")


tset <- ukb_final_read[1:10, 1:50]

##collapsing columns of the same type of instance by joining values with % in between
tset <-tset %>%
  gather(key = "col_ID", "Value",-IDs, na.rm = FALSE) %>%
  separate(col_ID,c("col_ID"), sep = "_[0-9]_[0-9]") %>%
  group_by(IDs, col_ID) %>%
  summarize(new_value=paste0(Value,collapse = "%")) %>%
  spread(col_ID,new_value)

##examples of introducing % 
##NA%RR%NA%NA-->NA
##NA%RR%NA%NA%NA -->RR
####applied each column in data frame, remove "NA%" and "%NA" if each value in the column containing either or both, this ensures that
###all non-NA values in each joint values are kept even if these values are not found in the first position of the joint values;
####in the case of all NA are joint, one NA is kept

tset_new <-as.data.frame(apply(tset,2, str_remove_all, "NA%")) 
tset_new <-as.data.frame(apply(tset_new,2, str_remove_all, "%NA")) 

write_csv(tset_new, "data_merge.csv")

###combines 5 parts of merged and cleaned dfs into one for python processing

d0<- readRDS("p0.rds")
d1<- readRDS("p1.rds")

print(all(colnames(p0) == colnames(p1)))
##clean.rds is converted from clean.csv, which is cleaned from all.csv file by python script (get_day_cancerType_final_version.py)
##clean means removing duplicated values in each instance (column)
clean<- readRDS("collapsed.rds")

##relocate column case_control and IDs
ukb_clean1 <- clean %>% relocate(case_control)
ukb_clean1 <- ukb_clean1 %>% relocate(IDs)

##remove column Unnamed..0
ukb_clean1 <- select (ukb_clean1,-c(Unnamed..0))
##set column IDs as row index
ukb_clean1 <- ukb_clean1 %>%
  remove_rownames() %>%
  column_to_rownames(var = 'IDs')

##remove column named "X"
ukb_clean1 <- select (ukb_clean1,-c(X))

ukb_clean1 <- ukb_clean1 %>%
  select(-contains(c("icd9")))

###after checking each of 38 columns, which contain almost full 100% missing values (NaN and nan)

#These columns will be removed from ukb_clean1. This brings the number of variables down to 502
uncollapsed <- ukb_clean1 %>% select(ends_with("_0"))

na_strings <- c("nan")
uncollapsed <- uncollapsed %>%
  replace_with_na_all(condition = ~.x %in% na_strings)

uncollapsed <- uncollapsed %>% 
  mutate_all(~replace(., is.nan(.), NA))

###remove columns in uncollapsed

removed_clos <- c("cancer_record_format_11_0", "cancer_record_format_12_0", "cancer_record_format_13_0", "cancer_record_format_14_0",
                  "cancer_record_format_15_0", "cancer_record_format_16_0",
                  "histology_of_cancer_tumour_10_0", "histology_of_cancer_tumour_11_0", "histology_of_cancer_tumour_12_0", "histology_of_cancer_tumour_13_0",
                  "histology_of_cancer_tumour_14_0", "histology_of_cancer_tumour_15_0", "histology_of_cancer_tumour_16_0",
                  "date_of_cancer_diagnosis_10_0", "date_of_cancer_diagnosis_11_0", "date_of_cancer_diagnosis_12_0", "date_of_cancer_diagnosis_13_0",
                  "date_of_cancer_diagnosis_14_0","date_of_cancer_diagnosis_15_0", "date_of_cancer_diagnosis_16_0",
                  "behaviour_of_cancer_tumour_10_0", "behaviour_of_cancer_tumour_11_0", "behaviour_of_cancer_tumour_12_0", "behaviour_of_cancer_tumour_13_0",
                  "behaviour_of_cancer_tumour_14_0", "behaviour_of_cancer_tumour_15_0", "behaviour_of_cancer_tumour_16_0",
                  "age_at_cancer_diagnosis_10_0", "age_at_cancer_diagnosis_11_0", "age_at_cancer_diagnosis_12_0", "age_at_cancer_diagnosis_13_0",
                  "age_at_cancer_diagnosis_14_0", "age_at_cancer_diagnosis_15_0", "age_at_cancer_diagnosis_16_0",
                  "type_of_cancer_icd10_10_0", "type_of_cancer_icd10_11_0", "type_of_cancer_icd10_13_0", "type_of_cancer_icd10_15_0", "type_of_cancer_icd10_16_0",
                  "cancer_record_format_11_0")

ukb_clean1 <- ukb_clean1 %>%
  select(-contains(removed_clos))

##save the clean file, which has the instance of each variable (each variable has multiple instances) if any value of the instance for a given variables 
##is not NA; So, most of variables have their first instance being Non-NA value, and the first instance is taken for most of variables; in the case of 
##the first instance for a variable is NA, the value of second instance will be used if it not being NA, and so on.


##After cleaning, the dimension of aggregated_ukb_final_020322.rds is 502461 x 502
saveRDS(ukb_clean1, "aggregated_ukb_final_020322.rds")

ukb_final_0303 <- readRDS("aggregated_ukb_final_020322.rds")

na_strings <- c("nan")

test <- ukb_final_0303[1:10,1:20]

test <- tibble::rownames_to_column(test, "IDs")

test <- test %>%
  replace_with_na_all(condition = ~.x %in% na_strings)

test <- test %>% 
  mutate_all(~replace(., is.nan(.), NA))


##combining blocks of 16

uk_final <- rbind("ukb_final_block1.rds", "ukb_final_block2.rds", "ukb_final_block3.rds", "ukb_final_block4.rds", "ukb_final_block5.rds", "ukb_final_block6.rds",  
                  "ukb_final_block7.rds", "ukb_final_block8.rds", "ukb_final_block9.rds", "ukb_final_block10.rds", 
                  "ukb_final_block11.rds", "ukb_final_block12.rds", "ukb_final_block13.rds", "ukb_final_block14.rds", "ukb_final_block15.rds", "ukb_final_block16.rds")

fixed <- readRDS("ukb_final_060322.rds")




