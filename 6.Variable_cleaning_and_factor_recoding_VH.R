rm(list=ls())

library(tidyverse)
library(janitor)

#Load Michelle's value types dataframe
df <- readxl::read_xlsx('Matches_2.xlsx')

#Clean names
df <- clean_names(df)

#Table of Value Types
table(df$value_type_matched, useNA = "ifany")

#Replace NA values in value_type_matched, called Admin
df$value_type_matched[is.na(df$value_type_matched)] <- "Admin"



#Clean variable numbers in df$cancer_list to match with ukb dataset
df$cancer_list <- gsub(" ", "_", df$cancer_list)

#Load UKB_final
ukb <- readRDS('ukb_final_060322.rds')
#Work with a small sample for speed
#ukb <- sample_n(ukb, 1000)


#Check the variable names match
sum(df$cancer_list %in% names(ukb))

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################


#Filter categorical (single, multiple, compound) from df
#NA not included

df_categorical <- filter(df, value_type_matched == 'Categorical single' | 
                           value_type_matched == 'Categorical multiple' | 
                           value_type_matched == 'Compound')
#Filter continous (continous, integer, date) + Everything else (outcome variables)
#NA not included
df_continuous <- filter(df, value_type_matched == 'Integer' | 
                          value_type_matched == 'Continuous' | 
                          value_type_matched == 'Date' |
                          value_type_matched == 'Admin'
                          )

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

#Subset ukb_categorical
ukb_categorical <- ukb[df_categorical$cancer_list]

#Subset ukb_continuous
ukb_continuous <- ukb[df_continuous$cancer_list]

#Check for Inf / -Inf
#ukb_categorical has no Inf / - Inf values
#max(ukb_categorical, na.rm = TRUE)
#min(ukb_categorical, na.rm = TRUE)

#ukb_continuous has Inf / - Inf values
#max(ukb_continuous, na.rm = TRUE)
#min(ukb_continuous, na.rm = TRUE)


#Identify problematic variables below:
#skim(ukb_continuous)

####systolic_blood_pressure_automated_reading
####cancer_record_format_10_0
####period_spent_working_mix_of_day_and_night_shifts
####interpolated_year_when_cancer_first_diagnosed
####diastolic_blood_pressure_automated_reading
####interpolated_age_of_participant_when_cancer_first_diagnosed
####year_ended_full_time_education
####period_spent_working_day_shifts



#Variable containing Inf / -Inf - Replace with NA, for the following variables:

#systolic_blood_pressure_automated_reading
ukb_continuous$systolic_blood_pressure_automated_reading <- ifelse(
  ukb_continuous$systolic_blood_pressure_automated_reading == -Inf | 
    ukb_continuous$systolic_blood_pressure_automated_reading == Inf, 
  NA, 
  ukb_continuous$systolic_blood_pressure_automated_reading)

#period_spent_working_mix_of_day_and_night_shifts
ukb_continuous$period_spent_working_mix_of_day_and_night_shifts <- ifelse(
  ukb_continuous$period_spent_working_mix_of_day_and_night_shifts == -Inf | 
    ukb_continuous$period_spent_working_mix_of_day_and_night_shifts == Inf, 
  NA, 
  ukb_continuous$period_spent_working_mix_of_day_and_night_shifts)

#interpolated_year_when_cancer_first_diagnosed
ukb_continuous$interpolated_year_when_cancer_first_diagnosed <- ifelse(
  ukb_continuous$interpolated_year_when_cancer_first_diagnosed == -Inf | 
    ukb_continuous$interpolated_year_when_cancer_first_diagnosed == Inf, 
  NA, 
  ukb_continuous$interpolated_year_when_cancer_first_diagnosed)

#diastolic_blood_pressure_automated_reading
ukb_continuous$diastolic_blood_pressure_automated_reading <- ifelse(
  ukb_continuous$diastolic_blood_pressure_automated_reading == -Inf | 
    ukb_continuous$diastolic_blood_pressure_automated_reading == Inf, 
  NA, 
  ukb_continuous$diastolic_blood_pressure_automated_reading)

#interpolated_age_of_participant_when_cancer_first_diagnosed
ukb_continuous$interpolated_age_of_participant_when_cancer_first_diagnosed <- ifelse(
  ukb_continuous$interpolated_age_of_participant_when_cancer_first_diagnosed == -Inf | 
    ukb_continuous$interpolated_age_of_participant_when_cancer_first_diagnosed == Inf, 
  NA, 
  ukb_continuous$interpolated_age_of_participant_when_cancer_first_diagnosed)



#period_spent_working_day_shifts
ukb_continuous$period_spent_working_day_shifts <- ifelse(
  ukb_continuous$period_spent_working_day_shifts == -Inf | 
    ukb_continuous$period_spent_working_day_shifts == Inf, 
  NA, 
  ukb_continuous$period_spent_working_day_shifts)


###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################


#Exploring ukb_categorical

#convert all columns to characters
ukb_categorical <- ukb_categorical %>% mutate_all(as.character)

#make copy of ukb_categorical dataframe
df2 <- ukb_categorical

#Banding categorical variables according to 'Matches 2.xlsx'

#1. beef_intake
df2$beef_intake[df2$beef_intake == "Do not know"] <- NA
df2$beef_intake[df2$beef_intake == "Prefer not to answer"] <- NA

df2$beef_intake[df2$beef_intake == "Never"] <- 0

df2$beef_intake[df2$beef_intake == "Less than once a week"] <- 1
df2$beef_intake[df2$beef_intake == "Once a week"] <- 1
df2$beef_intake[df2$beef_intake == "2-4 times a week"] <- 1
df2$beef_intake[df2$beef_intake == "5-6 times a week"] <- 1
df2$beef_intake[df2$beef_intake == "Once or more daily"] <- 1

table(df2$beef_intake, useNA = 'ifany')


#2. hcv_seropositivity_for_hepatitis_c_virus
df2 <- subset(df2, select = -c(hcv_seropositivity_for_hepatitis_c_virus))


#3. bkv_seropositivity_for_human_polyomavirus_bkv
df2 <- subset(df2, select = -c(bkv_seropositivity_for_human_polyomavirus_bkv))


#4. current_employment_status
df2$current_employment_status[df2$current_employment_status == "None of the above"] <- NA
df2$current_employment_status[df2$current_employment_status == "Prefer not to answer"] <- NA

df2$current_employment_status[df2$current_employment_status == "In paid employment or self-employed"] <- 0

df2$current_employment_status[df2$current_employment_status == "Doing unpaid or voluntary work"] <- 1
df2$current_employment_status[df2$current_employment_status == "Full or part-time student"] <- 1
df2$current_employment_status[df2$current_employment_status == "Looking after home and/or family"] <- 1
df2$current_employment_status[df2$current_employment_status == "Retired"] <-1
df2$current_employment_status[df2$current_employment_status == "Unemployed"] <-1
df2$current_employment_status[df2$current_employment_status == "Unable to work because of sickness or disability"] <-1

table(df2$current_employment_status, useNA = 'ifany')

#5. never_eat_eggs_dairy_wheat_sugar
df2$never_eat_eggs_dairy_wheat_sugar[df2$never_eat_eggs_dairy_wheat_sugar == "Prefer not to answer"] <- NA

df2$never_eat_eggs_dairy_wheat_sugar[df2$never_eat_eggs_dairy_wheat_sugar == "I eat all of the above"] <- 0

df2$never_eat_eggs_dairy_wheat_sugar[df2$never_eat_eggs_dairy_wheat_sugar == "Dairy products"] <- 1
df2$never_eat_eggs_dairy_wheat_sugar[df2$never_eat_eggs_dairy_wheat_sugar == "Eggs or foods containing eggs"] <- 1
df2$never_eat_eggs_dairy_wheat_sugar[df2$never_eat_eggs_dairy_wheat_sugar == "Wheat products"] <- 1
df2$never_eat_eggs_dairy_wheat_sugar[df2$never_eat_eggs_dairy_wheat_sugar == "Sugar or foods/drinks containing sugar"] <- 1

table(df2$never_eat_eggs_dairy_wheat_sugar, useNA = 'ifany')


#6. c_trachomatis_definition_i_seropositivity_for_chlamydia_trachomatis
df2 <- subset(df2, select = -c(c_trachomatis_definition_i_seropositivity_for_chlamydia_trachomatis))


#7. workplace_had_a_lot_of_cigarette_smoke_from_other_people_smoking
df2$workplace_had_a_lot_of_cigarette_smoke_from_other_people_smoking[df2$workplace_had_a_lot_of_cigarette_smoke_from_other_people_smoking == "Do not know"] <- NA

df2$workplace_had_a_lot_of_cigarette_smoke_from_other_people_smoking[df2$workplace_had_a_lot_of_cigarette_smoke_from_other_people_smoking == "Rarely/never"] <- 0

df2$workplace_had_a_lot_of_cigarette_smoke_from_other_people_smoking[df2$workplace_had_a_lot_of_cigarette_smoke_from_other_people_smoking == "Often"] <- 1
df2$workplace_had_a_lot_of_cigarette_smoke_from_other_people_smoking[df2$workplace_had_a_lot_of_cigarette_smoke_from_other_people_smoking == "Sometimes"] <- 1

table(df2$workplace_had_a_lot_of_cigarette_smoke_from_other_people_smoking, useNA = 'ifany')

#8. hpv 16 definition i seropositivity for human papillomavirus type 16
df2 <- subset(df2, select = -c(hpv_16_definition_i_seropositivity_for_human_papillomavirus_type_16))


#9. cmv seropositivity for human cytomegalovirus
df2 <- subset(df2, select = -c(cmv_seropositivity_for_human_cytomegalovirus))

#10. morning evening person chronotype
df2$morning_evening_person_chronotype[df2$morning_evening_person_chronotype == "Do not know"] <- NA
df2$morning_evening_person_chronotype[df2$morning_evening_person_chronotype == "Prefer not to answer"] <- NA

df2$morning_evening_person_chronotype[df2$morning_evening_person_chronotype == "Definitely a 'morning' person"] <- 0
df2$morning_evening_person_chronotype[df2$morning_evening_person_chronotype == "More a 'morning' than 'evening' person"] <- 0

df2$morning_evening_person_chronotype[df2$morning_evening_person_chronotype == "Definitely an 'evening' person"] <- 1
df2$morning_evening_person_chronotype[df2$morning_evening_person_chronotype == "More an 'evening' than a 'morning' person"] <- 1

table(df2$morning_evening_person_chronotype, useNA = 'ifany')

#11. close to major road
df2$close_to_major_road[df2$close_to_major_road == "No"] <- 0
df2$close_to_major_road[df2$close_to_major_road == "Yes"] <- 1

table(df2$close_to_major_road, useNA = 'ifany')


#12. death record format
df2 <- subset(df2, select = -c(death_record_format))


#13. coffee type
#drop column
df2 <- subset(df2, select = -c(coffee_type))

#14. mixture of day and night shifts worked
df2$mixture_of_day_and_night_shifts_worked[df2$mixture_of_day_and_night_shifts_worked == "Shift pattern was worked for whole of job"] <- 0

df2$mixture_of_day_and_night_shifts_worked[df2$mixture_of_day_and_night_shifts_worked == "Shift pattern was worked for some (but not all) of job"] <- 1
df2$mixture_of_day_and_night_shifts_worked[df2$mixture_of_day_and_night_shifts_worked == "This type of shift pattern was not worked during job"] <- 1

table(df2$mixture_of_day_and_night_shifts_worked, useNA = 'ifany')

#15. jcv seropositivity for human polyomavirus jcv
df2 <- subset(df2, select = -c(jcv_seropositivity_for_human_polyomavirus_jcv))

#16. getting up in morning
df2$getting_up_in_morning[df2$getting_up_in_morning == "Do not know"] <- NA
df2$getting_up_in_morning[df2$getting_up_in_morning == "Prefer not to answer"] <- NA

df2$getting_up_in_morning[df2$getting_up_in_morning == "Not at all easy"] <- 0
df2$getting_up_in_morning[df2$getting_up_in_morning == "Not very easy"] <- 0

df2$getting_up_in_morning[df2$getting_up_in_morning == "Fairly easy"] <- 1
df2$getting_up_in_morning[df2$getting_up_in_morning == "Very easy"] <- 1

table(df2$getting_up_in_morning, useNA = 'ifany')

#17. job involves heavy manual or physical work

df2$job_involves_heavy_manual_or_physical_work[df2$job_involves_heavy_manual_or_physical_work == "Do not know"] <- NA
df2$job_involves_heavy_manual_or_physical_work[df2$job_involves_heavy_manual_or_physical_work == "Prefer not to answer"] <- NA

df2$job_involves_heavy_manual_or_physical_work[df2$job_involves_heavy_manual_or_physical_work == "Never/rarely"] <- 0

df2$job_involves_heavy_manual_or_physical_work[df2$job_involves_heavy_manual_or_physical_work == "Sometimes"] <- 1
df2$job_involves_heavy_manual_or_physical_work[df2$job_involves_heavy_manual_or_physical_work == "Usually"] <- 1
df2$job_involves_heavy_manual_or_physical_work[df2$job_involves_heavy_manual_or_physical_work == "Always"] <- 1

table(df2$job_involves_heavy_manual_or_physical_work, useNA = 'ifany')

#18. ease of skin tanning
df2$ease_of_skin_tanning[df2$ease_of_skin_tanning == "Do not know"] <- NA
df2$ease_of_skin_tanning[df2$ease_of_skin_tanning == "Prefer not to answer"] <- NA

df2$ease_of_skin_tanning[df2$ease_of_skin_tanning == "Never tan, only burn"] <- 0

df2$ease_of_skin_tanning[df2$ease_of_skin_tanning == "Get mildly or occasionally tanned"] <- 1
df2$ease_of_skin_tanning[df2$ease_of_skin_tanning == "Get moderately tanned"] <- 1
df2$ease_of_skin_tanning[df2$ease_of_skin_tanning == "Get very tanned"] <- 1

table(df2$ease_of_skin_tanning, useNA = 'ifany')


#19. hpv 18 seropositivity for human papillomavirus type 18
df2 <- subset(df2, select = -c(hpv_18_seropositivity_for_human_papillomavirus_type_18))

#20. non cancer illness code self reported
#Code skin diseases == 1, else == 0

df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "eczema/dermatitis"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "psoriasis"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "stevens johnson syndrome"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "pemphigoid/pemphigus"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "chronic skin ulcers"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "acne/acne vulgaris"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "lichen planus"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "lichen sclerosis"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "cellulitis"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "rosacea"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "vitiligo"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "alopecia / hair loss"] <- 1
df2$non_cancer_illness_code_self_reported[df2$non_cancer_illness_code_self_reported == "bowen's disease"] <- 1

df2$non_cancer_illness_code_self_reported <- ifelse(df2$non_cancer_illness_code_self_reported == 1 |
                                                      is.na(df2$non_cancer_illness_code_self_reported) == TRUE,
                                                    df2$non_cancer_illness_code_self_reported, 
                                                    0)

table(df2$non_cancer_illness_code_self_reported, useNA = 'ifany')

#21. contributory secondary causes of death icd10
#Outcome

#22. country of birth uk elsewhere
df2$country_of_birth_uk_elsewhere[df2$country_of_birth_uk_elsewhere == "Do not know"] <- NA
df2$country_of_birth_uk_elsewhere[df2$country_of_birth_uk_elsewhere == "Prefer not to answer"] <- NA


df2$country_of_birth_uk_elsewhere[df2$country_of_birth_uk_elsewhere == "England"] <- 0
df2$country_of_birth_uk_elsewhere[df2$country_of_birth_uk_elsewhere == "Northern Ireland"] <- 0
df2$country_of_birth_uk_elsewhere[df2$country_of_birth_uk_elsewhere == "Republic of Ireland"] <- 0
df2$country_of_birth_uk_elsewhere[df2$country_of_birth_uk_elsewhere == "Scotland"] <- 0
df2$country_of_birth_uk_elsewhere[df2$country_of_birth_uk_elsewhere == "Wales"] <- 0

df2$country_of_birth_uk_elsewhere[df2$country_of_birth_uk_elsewhere == "Elsewhere"] <- 1

table(df2$country_of_birth_uk_elsewhere, useNA = 'ifany')

#23. maternal smoking around birth
df2$maternal_smoking_around_birth[df2$maternal_smoking_around_birth == "Do not know"] <- NA
df2$maternal_smoking_around_birth[df2$maternal_smoking_around_birth == "Prefer not to answer"] <- NA

df2$maternal_smoking_around_birth[df2$maternal_smoking_around_birth == "No"] <- 0

df2$maternal_smoking_around_birth[df2$maternal_smoking_around_birth == "Yes"] <- 1

table(df2$maternal_smoking_around_birth, useNA = 'ifany')


#24. hhv 7 seropositivity for human herpesvirus
df2 <- subset(df2, select = -c(hhv_7_seropositivity_for_human_herpesvirus))


#25. medication for cholesterol blood pressure diabetes or take exogenous hormones
#drop variable
df2 <- subset(df2, select = -c(medication_for_cholesterol_blood_pressure_diabetes_or_take_exogenous_hormones))

#26. cheese intake
df2$cheese_intake[df2$cheese_intake == "Do not know"] <- NA
df2$cheese_intake[df2$cheese_intake == "Prefer not to answer"] <- NA

df2$cheese_intake[df2$cheese_intake == "Never"] <- 0

df2$cheese_intake[df2$cheese_intake == "Less than once a week"] <- 1
df2$cheese_intake[df2$cheese_intake == "Once a week"] <- 1
df2$cheese_intake[df2$cheese_intake == "2-4 times a week"] <- 1
df2$cheese_intake[df2$cheese_intake == "5-6 times a week"] <- 1
df2$cheese_intake[df2$cheese_intake == "Once or more daily"] <- 1

table(df2$cheese_intake, useNA = 'ifany')


#26. hsv 2 seropositivity for herpes simplex virus
df2 <- subset(df2, select = -c(hsv_2_seropositivity_for_herpes_simplex_virus))


#27. histology of cancer tumour
#outcome

#28. non oily fish intake
df2$non_oily_fish_intake[df2$non_oily_fish_intake == "Do not know"] <- NA
df2$non_oily_fish_intake[df2$non_oily_fish_intake == "Prefer not to answer"] <- NA

df2$non_oily_fish_intake[df2$non_oily_fish_intake == "Never"] <- 0

df2$non_oily_fish_intake[df2$non_oily_fish_intake == "Less than once a week"] <- 1
df2$non_oily_fish_intake[df2$non_oily_fish_intake == "Once a week"] <- 1
df2$non_oily_fish_intake[df2$non_oily_fish_intake == "2-4 times a week"] <- 1
df2$non_oily_fish_intake[df2$non_oily_fish_intake == "5-6 times a week"] <- 1
df2$non_oily_fish_intake[df2$non_oily_fish_intake == "Once or more daily"] <- 1

table(df2$non_oily_fish_intake, useNA = 'ifany')

#29. description of cause of death
#outcome


#30. types of physical activity in last 4 weeks
df2$types_of_physical_activity_in_last_4_weeks[df2$types_of_physical_activity_in_last_4_weeks == "Prefer not to answer"] <- NA

df2$types_of_physical_activity_in_last_4_weeks[df2$types_of_physical_activity_in_last_4_weeks == "None of the above"] <- 0

df2$types_of_physical_activity_in_last_4_weeks[df2$types_of_physical_activity_in_last_4_weeks == "Light DIY (eg: pruning, watering the lawn)"] <- 1
df2$types_of_physical_activity_in_last_4_weeks[df2$types_of_physical_activity_in_last_4_weeks == "Walking for pleasure (not as a means of transport)"] <- 1

df2$types_of_physical_activity_in_last_4_weeks[df2$types_of_physical_activity_in_last_4_weeks == "Heavy DIY (eg: weeding, lawn mowing, carpentry, digging)"] <- 2
df2$types_of_physical_activity_in_last_4_weeks[df2$types_of_physical_activity_in_last_4_weeks == "Other exercises (eg: swimming, cycling, keep fit, bowling)"] <- 2

df2$types_of_physical_activity_in_last_4_weeks[df2$types_of_physical_activity_in_last_4_weeks == "Strenuous sports"] <- 3



table(df2$types_of_physical_activity_in_last_4_weeks, useNA = 'ifany')


#31. former alcohol drinker
df2$former_alcohol_drinker[df2$former_alcohol_drinker == "Prefer not to answer"] <- NA

df2$former_alcohol_drinker[df2$former_alcohol_drinker == "No"] <- 0
df2$former_alcohol_drinker[df2$former_alcohol_drinker == "Yes"] <- 1

table(df2$former_alcohol_drinker, useNA = 'ifany')


#32. work hours lumped category
df2$work_hours_lumped_category[df2$work_hours_lumped_category == "Prefer not to answer"] <- NA

df2$work_hours_lumped_category[df2$work_hours_lumped_category == "15 to less-than-20 hours"] <- 0
df2$work_hours_lumped_category[df2$work_hours_lumped_category == "20 to less-than-30 hours"] <- 0

df2$work_hours_lumped_category[df2$work_hours_lumped_category == "30 to 40 hours"] <- 1

df2$work_hours_lumped_category[df2$work_hours_lumped_category == "Over 40 hours"] <- 2

table(df2$work_hours_lumped_category, useNA = 'ifany')


#33. ever used hormone replacement therapy hrt
#Sex specific
df2$ever_used_hormone_replacement_therapy_hrt[df2$ever_used_hormone_replacement_therapy_hrt == "Prefer not to answer"] <- NA
df2$ever_used_hormone_replacement_therapy_hrt[df2$ever_used_hormone_replacement_therapy_hrt == "Do not know"] <- NA


df2$ever_used_hormone_replacement_therapy_hrt[df2$ever_used_hormone_replacement_therapy_hrt == "No"] <- 0

df2$ever_used_hormone_replacement_therapy_hrt[df2$ever_used_hormone_replacement_therapy_hrt == "Yes"] <- 1

table(df2$ever_used_hormone_replacement_therapy_hrt, useNA = 'ifany')


#34. alcohol intake frequency
df2$alcohol_intake_frequency[df2$alcohol_intake_frequency == "Prefer not to answer"] <- NA

df2$alcohol_intake_frequency[df2$alcohol_intake_frequency == "Never"] <- 0

df2$alcohol_intake_frequency[df2$alcohol_intake_frequency == "Special occasions only"] <- 1
df2$alcohol_intake_frequency[df2$alcohol_intake_frequency == "One to three times a month"] <- 1

df2$alcohol_intake_frequency[df2$alcohol_intake_frequency == "Daily or almost daily"] <- 2
df2$alcohol_intake_frequency[df2$alcohol_intake_frequency == "Three or four times a week"] <- 2
df2$alcohol_intake_frequency[df2$alcohol_intake_frequency == "Once or twice a week"] <- 2


table(df2$alcohol_intake_frequency, useNA = 'ifany')


#35. mcv seropositivity for merkel cell polyomavirus
df2 <- subset(df2, select = -c(mcv_seropositivity_for_merkel_cell_polyomavirus))


#36. job involves mainly walking or standing
df2$job_involves_mainly_walking_or_standing[df2$job_involves_mainly_walking_or_standing == "Do not know"] <- NA
df2$job_involves_mainly_walking_or_standing[df2$job_involves_mainly_walking_or_standing == "Prefer not to answer"] <- NA

df2$job_involves_mainly_walking_or_standing[df2$job_involves_mainly_walking_or_standing == "Never/rarely"] <- 0

df2$job_involves_mainly_walking_or_standing[df2$job_involves_mainly_walking_or_standing == "Sometimes"] <- 1
df2$job_involves_mainly_walking_or_standing[df2$job_involves_mainly_walking_or_standing == "Usually"] <- 1
df2$job_involves_mainly_walking_or_standing[df2$job_involves_mainly_walking_or_standing == "Always"] <- 1

table(df2$job_involves_mainly_walking_or_standing, useNA = 'ifany')


#37. workplace full of chemical or other fumes
df2$workplace_full_of_chemical_or_other_fumes[df2$workplace_full_of_chemical_or_other_fumes == "Do not know"] <- NA

df2$workplace_full_of_chemical_or_other_fumes[df2$workplace_full_of_chemical_or_other_fumes == "Rarely/never"] <- 0

df2$workplace_full_of_chemical_or_other_fumes[df2$workplace_full_of_chemical_or_other_fumes == "Often"] <- 1
df2$workplace_full_of_chemical_or_other_fumes[df2$workplace_full_of_chemical_or_other_fumes == "Sometimes"] <- 1

table(df2$workplace_full_of_chemical_or_other_fumes, useNA = 'ifany')

#38. lamb mutton intake
df2$lamb_mutton_intake[df2$lamb_mutton_intake == "Do not know"] <- NA
df2$lamb_mutton_intake[df2$lamb_mutton_intake == "Prefer not to answer"] <- NA

df2$lamb_mutton_intake[df2$lamb_mutton_intake == "Never"] <- 0

df2$lamb_mutton_intake[df2$lamb_mutton_intake == "Less than once a week"] <- 1
df2$lamb_mutton_intake[df2$lamb_mutton_intake == "Once a week"] <- 1
df2$lamb_mutton_intake[df2$lamb_mutton_intake == "2-4 times a week"] <- 1
df2$lamb_mutton_intake[df2$lamb_mutton_intake == "5-6 times a week"] <- 1
df2$lamb_mutton_intake[df2$lamb_mutton_intake == "Once or more daily"] <- 1

table(df2$lamb_mutton_intake, useNA = 'ifany')


#39. mineral and other dietary supplements
df2$mineral_and_other_dietary_supplements[df2$mineral_and_other_dietary_supplements == "Prefer not to answer"] <- NA

df2$mineral_and_other_dietary_supplements[df2$mineral_and_other_dietary_supplements == "Fish oil (including cod liver oil)"] <- 1

df2$mineral_and_other_dietary_supplements <- ifelse(df2$mineral_and_other_dietary_supplements == 1 |
                                                      is.na(df2$mineral_and_other_dietary_supplements) == TRUE,
                                                    df2$mineral_and_other_dietary_supplements, 
                                                    0)

table(df2$mineral_and_other_dietary_supplements, useNA = 'ifany')


#40. c trachomatis definition ii seropositivity for chlamydia trachomatis
df2 <- subset(df2, select = -c(c_trachomatis_definition_ii_seropositivity_for_chlamydia_trachomatis))


#41. h pylori definition ii seropositivity for helicobacter pylori
df2 <- subset(df2, select = -c(h_pylori_definition_ii_seropositivity_for_helicobacter_pylori))


#42. hiv 1 seropositivity for human immunodeficiency virus
df2 <- subset(df2, select = -c(hiv_1_seropositivity_for_human_immunodeficiency_virus))


#43. ever smoked
df2$ever_smoked[df2$ever_smoked == "No"] <- 0

df2$ever_smoked[df2$ever_smoked == "Yes"] <- 1

table(df2$ever_smoked, useNA = 'ifany')


#44. milk type used
df2 <- subset(df2, select = -c(milk_type_used))

#45. night shifts worked
df2$night_shifts_worked[df2$night_shifts_worked == "Shift pattern was worked for whole of job"] <- 0

df2$night_shifts_worked[df2$night_shifts_worked == "Shift pattern was worked for some (but not all) of job"] <- 1
df2$night_shifts_worked[df2$night_shifts_worked == "This type of shift pattern was not worked during job"] <- 1

table(df2$night_shifts_worked, useNA = 'ifany')


#46. worked with pesticides
df2$worked_with_pesticides[df2$worked_with_pesticides == "Do not know"] <- NA

df2$worked_with_pesticides[df2$worked_with_pesticides == "Rarely/never"] <- 0

df2$worked_with_pesticides[df2$worked_with_pesticides == "Often"] <- 1
df2$worked_with_pesticides[df2$worked_with_pesticides == "Sometimes"] <- 1

table(df2$worked_with_pesticides, useNA = 'ifany')


#47. uk biobank assessment centre
df2 <- subset(df2, select = -c(uk_biobank_assessment_centre))

#48. behaviour of cancer tumour
#outcome


#49. leisure social activities
df2 <- subset(df2, select = -c(leisure_social_activities))

#50. current tobacco smoking
df2$current_tobacco_smoking[df2$current_tobacco_smoking == "Prefer not to answer"] <- NA

df2$current_tobacco_smoking[df2$current_tobacco_smoking == "No"] <- 0

df2$current_tobacco_smoking[df2$current_tobacco_smoking == "Only occasionally"] <- 1
df2$current_tobacco_smoking[df2$current_tobacco_smoking == "Yes, on most or all days"] <- 1

table(df2$current_tobacco_smoking, useNA = 'ifany')

#51. use of sun uv protection
df2$use_of_sun_uv_protection[df2$use_of_sun_uv_protection == "Do not know"] <- NA
df2$use_of_sun_uv_protection[df2$use_of_sun_uv_protection == "Prefer not to answer"] <- NA

df2$use_of_sun_uv_protection[df2$use_of_sun_uv_protection == "Do not go out in sunshine"] <- 0

df2$use_of_sun_uv_protection[df2$use_of_sun_uv_protection == "Always"] <- 1

df2$use_of_sun_uv_protection[df2$use_of_sun_uv_protection == "Most of the time"] <- 2
df2$use_of_sun_uv_protection[df2$use_of_sun_uv_protection == "Sometimes"] <- 2

df2$use_of_sun_uv_protection[df2$use_of_sun_uv_protection == "Never/rarely"] <- 3

table(df2$use_of_sun_uv_protection, useNA = 'ifany')

#52. cancer code self reported
#outcome


#53. own or rent accommodation lived in
df2$own_or_rent_accommodation_lived_in[df2$own_or_rent_accommodation_lived_in == "Prefer not to answer"] <- NA

df2$own_or_rent_accommodation_lived_in[df2$own_or_rent_accommodation_lived_in == "Own outright (by you or someone in your household)"] <- 1
df2$own_or_rent_accommodation_lived_in[df2$own_or_rent_accommodation_lived_in == "Own with a mortgage"] <- 1

df2$own_or_rent_accommodation_lived_in <- ifelse(df2$own_or_rent_accommodation_lived_in == 1 |
                                                      is.na(df2$own_or_rent_accommodation_lived_in) == TRUE,
                                                    df2$own_or_rent_accommodation_lived_in, 
                                                    0)

table(df2$own_or_rent_accommodation_lived_in, useNA = 'ifany')


#54. ever taken oral contraceptive pill
df2$ever_taken_oral_contraceptive_pill[df2$ever_taken_oral_contraceptive_pill == "Prefer not to answer"] <- NA
df2$ever_taken_oral_contraceptive_pill[df2$ever_taken_oral_contraceptive_pill == "Do not know"] <- NA

df2$ever_taken_oral_contraceptive_pill[df2$ever_taken_oral_contraceptive_pill == "No"] <- 0

df2$ever_taken_oral_contraceptive_pill[df2$ever_taken_oral_contraceptive_pill == "Yes"] <- 1

table(df2$ever_taken_oral_contraceptive_pill, useNA = 'ifany')


#55. sleeplessness insomnia
df2$sleeplessness_insomnia[df2$sleeplessness_insomnia == "Prefer not to answer"] <- NA

df2$sleeplessness_insomnia[df2$sleeplessness_insomnia == "Never/rarely"] <- 0

df2$sleeplessness_insomnia[df2$sleeplessness_insomnia == "Sometimes"] <- 1
df2$sleeplessness_insomnia[df2$sleeplessness_insomnia == "Usually"] <- 1


table(df2$sleeplessness_insomnia, useNA = 'ifany')


#56. hhv 6 overall seropositivity for human herpesvirus
df2 <- subset(df2, select = -c(hhv_6_overall_seropositivity_for_human_herpesvirus))


#57. htlv 1 seropositivity for human t lymphotropic virus
df2 <- subset(df2, select = -c(htlv_1_seropositivity_for_human_t_lymphotropic_virus))


#58. past tobacco smoking
df2 <- subset(df2, select = -c(past_tobacco_smoking))


#59. cancer diagnosed by doctor
#outcome

#60. country of birth non uk origin
df2 <- subset(df2, select = -c(country_of_birth_non_uk_origin))

#61. workplace had a lot of diesel exhaust
df2$workplace_had_a_lot_of_diesel_exhaust[df2$workplace_had_a_lot_of_diesel_exhaust == "Do not know"] <- NA

df2$workplace_had_a_lot_of_diesel_exhaust[df2$workplace_had_a_lot_of_diesel_exhaust == "Rarely/never"] <- 0

df2$workplace_had_a_lot_of_diesel_exhaust[df2$workplace_had_a_lot_of_diesel_exhaust == "Often"] <- 1
df2$workplace_had_a_lot_of_diesel_exhaust[df2$workplace_had_a_lot_of_diesel_exhaust == "Sometimes"] <- 1

table(df2$workplace_had_a_lot_of_diesel_exhaust, useNA = 'ifany')

#62. current employment status corrected
df2 <- subset(df2, select = -c(current_employment_status_corrected))

#63. sex
#No changes
table(df2$sex, useNA = 'ifany')


#64. hbv seropositivity for hepatitis b virus
df2 <- subset(df2, select = -c(hbv_seropositivity_for_hepatitis_b_virus))


#65. underlying primary cause of death icd10
#outcome

#66. medication for cholesterol blood pressure or diabetes
df2 <- subset(df2, select = -c(medication_for_cholesterol_blood_pressure_or_diabetes))


#67. transport type for commuting to job workplace
df2$transport_type_for_commuting_to_job_workplace[df2$transport_type_for_commuting_to_job_workplace == "Do not know"] <- NA

df2$transport_type_for_commuting_to_job_workplace[df2$transport_type_for_commuting_to_job_workplace == "Walk"] <- 1
df2$transport_type_for_commuting_to_job_workplace[df2$transport_type_for_commuting_to_job_workplace == "Cycle"] <- 1

df2$transport_type_for_commuting_to_job_workplace <- ifelse(df2$transport_type_for_commuting_to_job_workplace == 1 |
                                                   is.na(df2$transport_type_for_commuting_to_job_workplace) == TRUE,
                                                 df2$transport_type_for_commuting_to_job_workplace, 
                                                 0)

table(df2$transport_type_for_commuting_to_job_workplace, useNA = 'ifany')


#68. t gondii seropositivity for toxoplasma gondii
df2 <- subset(df2, select = -c(t_gondii_seropositivity_for_toxoplasma_gondii))


#69. number of vehicles in household
df2$number_of_vehicles_in_household[df2$number_of_vehicles_in_household == "Do not know"] <- NA
df2$number_of_vehicles_in_household[df2$number_of_vehicles_in_household == "Prefer not to answer"] <- NA

df2$number_of_vehicles_in_household[df2$number_of_vehicles_in_household == "One"] <- 0

df2$number_of_vehicles_in_household <- ifelse(df2$number_of_vehicles_in_household == 0 |
                                                              is.na(df2$number_of_vehicles_in_household) == TRUE,
                                                            df2$number_of_vehicles_in_household, 
                                                            1)

table(df2$number_of_vehicles_in_household, useNA = 'ifany')

#70. job coding
#remove - variable too complex
df2 <- subset(df2, select = -c(job_coding))


#71. vzv seropositivity for varicella zoster virus
df2 <- subset(df2, select = -c(vzv_seropositivity_for_varicella_zoster_virus))


#72. h pylori definition i seropositivity for helicobacter pylori
df2 <- subset(df2, select = -c(h_pylori_definition_i_seropositivity_for_helicobacter_pylori))


#73. types of transport used excluding work
df2$types_of_transport_used_excluding_work[df2$types_of_transport_used_excluding_work == "Prefer not to answer"] <- NA

df2$types_of_transport_used_excluding_work[df2$types_of_transport_used_excluding_work == "Walk"] <- 1
df2$types_of_transport_used_excluding_work[df2$types_of_transport_used_excluding_work == "Cycle"] <- 1

df2$types_of_transport_used_excluding_work <- ifelse(df2$types_of_transport_used_excluding_work == 1 |
                                                              is.na(df2$types_of_transport_used_excluding_work) == TRUE,
                                                            df2$types_of_transport_used_excluding_work, 
                                                            0)

table(df2$types_of_transport_used_excluding_work, useNA = 'ifany')


#74. ebv seropositivity for epstein barr virus
df2 <- subset(df2, select = -c(ebv_seropositivity_for_epstein_barr_virus))

#75. type of cancer icd10
#outcome

#76. medication for pain relief constipation heartburn
df2 <- subset(df2, select = -c(medication_for_pain_relief_constipation_heartburn))

#77. kshv seropositivity for kaposis sarcoma associated herpesvirus
df2 <- subset(df2, select = -c(kshv_seropositivity_for_kaposis_sarcoma_associated_herpesvirus))


#78. facial ageing
df2$facial_ageing[df2$facial_ageing == "Do not know"] <- NA
df2$facial_ageing[df2$facial_ageing == "Prefer not to answer"] <- NA

df2$facial_ageing[df2$facial_ageing == "Younger than you are"] <- 0

df2$facial_ageing[df2$facial_ageing == "About your age"] <- 1

df2$facial_ageing[df2$facial_ageing == "Older than you are"] <- 2

table(df2$facial_ageing, useNA = 'ifany')


#79. hsv 1 seropositivity for herpes simplex virus
df2 <- subset(df2, select = -c(hsv_1_seropositivity_for_herpes_simplex_virus))


#80. qualifications
df2$qualifications[df2$qualifications == "Prefer not to answer"] <- NA

df2$qualifications[df2$qualifications == "None of the above"] <- 0

df2$qualifications[df2$qualifications == "NVQ or HND or HNC or equivalent"] <- 1
df2$qualifications[df2$qualifications == "CSEs or equivalent"] <- 1
df2$qualifications[df2$qualifications == "O levels/GCSEs or equivalent"] <- 1
df2$qualifications[df2$qualifications == "A levels/AS levels or equivalent"] <- 1

df2$qualifications[df2$qualifications == "College or University degree"] <- 2
df2$qualifications[df2$qualifications == "Other professional qualifications eg: nursing, teaching"] <- 2

table(df2$qualifications, useNA = 'ifany')


#81. type of accommodation lived in
df2$type_of_accommodation_lived_in[df2$type_of_accommodation_lived_in == "Prefer not to answer"] <- NA

df2$type_of_accommodation_lived_in[df2$type_of_accommodation_lived_in == "A house or bungalow"] <- 0

df2$type_of_accommodation_lived_in <- ifelse(df2$type_of_accommodation_lived_in == 0 |
                                                   is.na(df2$type_of_accommodation_lived_in) == TRUE,
                                                 df2$type_of_accommodation_lived_in, 
                                                 1)

table(df2$type_of_accommodation_lived_in, useNA = 'ifany')


#82. job involves shift work
df2$job_involves_shift_work[df2$job_involves_shift_work == "Do not know"] <- NA
df2$job_involves_shift_work[df2$job_involves_shift_work == "Prefer not to answer"] <- NA

df2$job_involves_shift_work[df2$job_involves_shift_work == "Never/rarely"] <- 0

df2$job_involves_shift_work[df2$job_involves_shift_work == "Sometimes"] <- 1
df2$job_involves_shift_work[df2$job_involves_shift_work == "Usually"] <- 1
df2$job_involves_shift_work[df2$job_involves_shift_work == "Always"] <- 1

table(df2$job_involves_shift_work, useNA = 'ifany')


#83. ethnic background
df2$ethnic_background[df2$ethnic_background == 'Do not know'] <- NA
df2$ethnic_background[df2$ethnic_background == 'Prefer not to answer'] <- NA


#Band "White"
df2$ethnic_background[df2$ethnic_background == 'British'] <- 'White'
df2$ethnic_background[df2$ethnic_background == 'Irish'] <- 'White'
df2$ethnic_background[df2$ethnic_background == 'Any other white background'] <- 'White'

#Band 'Mixed'
df2$ethnic_background[df2$ethnic_background == 'White and Black Caribbean'] <- 'Mixed'
df2$ethnic_background[df2$ethnic_background == 'White and Black African'] <- 'Mixed'
df2$ethnic_background[df2$ethnic_background == 'White and Asian'] <- 'Mixed'
df2$ethnic_background[df2$ethnic_background == 'Any other mixed background'] <- 'Mixed'

#Band 'Asian or Asian British'
df2$ethnic_background[df2$ethnic_background == 'Indian'] <- 'Asian or Asian British'
df2$ethnic_background[df2$ethnic_background == 'Pakistani'] <- 'Asian or Asian British'
df2$ethnic_background[df2$ethnic_background == 'Bangladeshi'] <- 'Asian or Asian British'
df2$ethnic_background[df2$ethnic_background == 'Any other Asian background'] <- 'Asian or Asian British'

#Band 'Black or Black British'
df2$ethnic_background[df2$ethnic_background == 'Caribbean'] <- 'Black or Black British'
df2$ethnic_background[df2$ethnic_background == 'African'] <- 'Black or Black British'
df2$ethnic_background[df2$ethnic_background == 'Any other Black background'] <- 'Black or Black British'


#White == 0, non-white == 1
df2$ethnic_background[df2$ethnic_background == 'White'] <- 0


df2$ethnic_background <- ifelse(df2$ethnic_background == 0 | is.na(df2$ethnic_background) == TRUE, df2$ethnic_background, 1)

table(df2$ethnic_background, useNA = 'ifany')


#84. home area population density urban or rural
df2$home_area_population_density_urban_or_rural[df2$home_area_population_density_urban_or_rural == "Scotland - Other Urban Area"] <- 0
df2$home_area_population_density_urban_or_rural[df2$home_area_population_density_urban_or_rural == "England/Wales - Urban - less sparse"] <- 0
df2$home_area_population_density_urban_or_rural[df2$home_area_population_density_urban_or_rural == "Scotland - Large Urban Area"] <- 0
df2$home_area_population_density_urban_or_rural[df2$home_area_population_density_urban_or_rural == "Scotland - Large Urban Area"] <- 0

df2$home_area_population_density_urban_or_rural <- ifelse(df2$home_area_population_density_urban_or_rural == 0, df2$home_area_population_density_urban_or_rural, 1)

table(df2$home_area_population_density_urban_or_rural, useNA = 'ifany')


#85. hhv 6b seropositivity for human herpesvirus
df2 <- subset(df2, select = -c(hhv_6b_seropositivity_for_human_herpesvirus))


#86. alcohol drinker status
df2$alcohol_drinker_status[df2$alcohol_drinker_status == 'Prefer not to answer'] <- NA

df2$alcohol_drinker_status[df2$alcohol_drinker_status == 'Never'] <- 0

df2$alcohol_drinker_status[df2$alcohol_drinker_status == 'Previous'] <- 1

df2$alcohol_drinker_status[df2$alcohol_drinker_status == 'Current'] <- 2

table(df2$alcohol_drinker_status, useNA = 'ifany')


#87. job involved shift work
df2$job_involved_shift_work[df2$job_involved_shift_work == 'No'] <- 0
df2$job_involved_shift_work[df2$job_involved_shift_work == 'Yes'] <- 1

table(df2$job_involved_shift_work, useNA = 'ifany')


#88. had menopause
df2$had_menopause[df2$had_menopause == 'Prefer not to answer'] <- NA

df2$had_menopause[df2$had_menopause == 'No'] <- 0

df2$had_menopause <- ifelse(df2$had_menopause == 0 |
                                               is.na(df2$had_menopause) == TRUE,
                                             df2$had_menopause, 
                                             1)


table(df2$had_menopause, useNA = 'ifany')


#89. poultry intake
df2$poultry_intake[df2$poultry_intake == "Do not know"] <- NA
df2$poultry_intake[df2$poultry_intake == "Prefer not to answer"] <- NA

df2$poultry_intake[df2$poultry_intake == "Never"] <- 0

df2$poultry_intake[df2$poultry_intake == "Less than once a week"] <- 1
df2$poultry_intake[df2$poultry_intake == "Once a week"] <- 1
df2$poultry_intake[df2$poultry_intake == "2-4 times a week"] <- 1
df2$poultry_intake[df2$poultry_intake == "5-6 times a week"] <- 1
df2$poultry_intake[df2$poultry_intake == "Once or more daily"] <- 1

table(df2$poultry_intake, useNA = 'ifany')


#90. vitamin and mineral supplements
df2$vitamin_and_mineral_supplements[df2$vitamin_and_mineral_supplements == "Prefer not to answer"] <- NA

df2$vitamin_and_mineral_supplements[df2$vitamin_and_mineral_supplements == "None of the above"] <- 0

df2$vitamin_and_mineral_supplements <- ifelse(df2$vitamin_and_mineral_supplements == 0 |
                                               is.na(df2$vitamin_and_mineral_supplements) == TRUE,
                                             df2$vitamin_and_mineral_supplements, 
                                             1)


table(df2$vitamin_and_mineral_supplements, useNA = 'ifany')


#91. hpv 16 definition ii seropositivity for human papillomavirus type 16
df2 <- subset(df2, select = -c(hpv_16_definition_ii_seropositivity_for_human_papillomavirus_type_16))


#92. hhv 6a seropositivity for human herpesvirus
df2 <- subset(df2, select = -c(hhv_6a_seropositivity_for_human_herpesvirus))


#93. processed meat intake
df2$processed_meat_intake[df2$processed_meat_intake == "Do not know"] <- NA
df2$processed_meat_intake[df2$processed_meat_intake == "Prefer not to answer"] <- NA

df2$processed_meat_intake[df2$processed_meat_intake == "Never"] <- 0

df2$processed_meat_intake[df2$processed_meat_intake == "Less than once a week"] <- 1
df2$processed_meat_intake[df2$processed_meat_intake == "Once a week"] <- 1
df2$processed_meat_intake[df2$processed_meat_intake == "2-4 times a week"] <- 1
df2$processed_meat_intake[df2$processed_meat_intake == "5-6 times a week"] <- 1
df2$processed_meat_intake[df2$processed_meat_intake == "Once or more daily"] <- 1

table(df2$processed_meat_intake, useNA = 'ifany')

#94. worked with materials containing asbestos
df2$worked_with_materials_containing_asbestos[df2$worked_with_materials_containing_asbestos == "Do not know"] <- NA

df2$worked_with_materials_containing_asbestos[df2$worked_with_materials_containing_asbestos == "Rarely/never"] <- 0

df2$worked_with_materials_containing_asbestos[df2$worked_with_materials_containing_asbestos == "Often"] <- 1
df2$worked_with_materials_containing_asbestos[df2$worked_with_materials_containing_asbestos == "Sometimes"] <- 1

table(df2$worked_with_materials_containing_asbestos, useNA = 'ifany')


#95. job involves night shift work
df2$job_involves_night_shift_work[df2$job_involves_night_shift_work == "Do not know"] <- NA
df2$job_involves_night_shift_work[df2$job_involves_night_shift_work == "Prefer not to answer"] <- NA

df2$job_involves_night_shift_work[df2$job_involves_night_shift_work == "Never/rarely"] <- 0

df2$job_involves_night_shift_work[df2$job_involves_night_shift_work == "Sometimes"] <- 1
df2$job_involves_night_shift_work[df2$job_involves_night_shift_work == "Usually"] <- 1
df2$job_involves_night_shift_work[df2$job_involves_night_shift_work == "Always"] <- 1

table(df2$job_involves_night_shift_work, useNA = 'ifany')


#96. hair colour natural before greying
df2$hair_colour_natural_before_greying[df2$hair_colour_natural_before_greying == "Do not know"] <- NA
df2$hair_colour_natural_before_greying[df2$hair_colour_natural_before_greying == "Prefer not to answer"] <- NA

table(df2$hair_colour_natural_before_greying, useNA = 'ifany')

#97. skin colour
df2$skin_colour[df2$skin_colour == "Do not know"] <- NA
df2$skin_colour[df2$skin_colour == "Prefer not to answer"] <- NA

table(df2$skin_colour, useNA = 'ifany')


#98. worked with paints thinners or glues
df2$worked_with_paints_thinners_or_glues[df2$worked_with_paints_thinners_or_glues == "Do not know"] <- NA

df2$worked_with_paints_thinners_or_glues[df2$worked_with_paints_thinners_or_glues == "Rarely/never"] <- 0

df2$worked_with_paints_thinners_or_glues[df2$worked_with_paints_thinners_or_glues == "Often"] <- 1
df2$worked_with_paints_thinners_or_glues[df2$worked_with_paints_thinners_or_glues == "Sometimes"] <- 1

table(df2$worked_with_paints_thinners_or_glues, useNA = 'ifany')


#99. day shifts worked
df2$day_shifts_worked[df2$day_shifts_worked == "Shift pattern was worked for whole of job"] <- 0

df2$day_shifts_worked[df2$day_shifts_worked == "Shift pattern was worked for some (but not all) of job"] <- 1
df2$day_shifts_worked[df2$day_shifts_worked == "This type of shift pattern was not worked during job"] <- 1

table(df2$day_shifts_worked, useNA = 'ifany')


#100. average total household income before tax
df2$average_total_household_income_before_tax[df2$average_total_household_income_before_tax == "Do not know"] <- NA
df2$average_total_household_income_before_tax[df2$average_total_household_income_before_tax == "Prefer not to answer"] <- NA

df2$average_total_household_income_before_tax[df2$average_total_household_income_before_tax == "Less than 18,000"] <- 0

df2$average_total_household_income_before_tax[df2$average_total_household_income_before_tax == "18,000 to 30,999"] <- 1
df2$average_total_household_income_before_tax[df2$average_total_household_income_before_tax == "31,000 to 51,999"] <- 1

df2$average_total_household_income_before_tax[df2$average_total_household_income_before_tax == "52,000 to 100,000"] <- 2
df2$average_total_household_income_before_tax[df2$average_total_household_income_before_tax == "Greater than 100,000"] <- 2

table(df2$average_total_household_income_before_tax, useNA = 'ifany')


#101. smoking status
df2$smoking_status[df2$smoking_status == "Do not know"] <- NA
df2$smoking_status[df2$smoking_status == "Prefer not to answer"] <- NA


df2$smoking_status[df2$smoking_status == "Never"] <- 0
df2$smoking_status[df2$smoking_status == "Previous"] <- 1
df2$smoking_status[df2$smoking_status == "Current"] <- 2

table(df2$smoking_status, useNA = 'ifany')


#102. duration walking for pleasure
df2$duration_walking_for_pleasure[df2$duration_walking_for_pleasure == "Do not know"] <- NA
df2$duration_walking_for_pleasure[df2$duration_walking_for_pleasure == "Prefer not to answer"] <- NA

df2$duration_walking_for_pleasure[df2$duration_walking_for_pleasure == "Less than 15 minutes"] <- 0
df2$duration_walking_for_pleasure[df2$duration_walking_for_pleasure == "Between 15 and 30 minutes"] <- 0
df2$duration_walking_for_pleasure[df2$duration_walking_for_pleasure == "Between 30 minutes and 1 hour"] <- 0

df2$duration_walking_for_pleasure[df2$duration_walking_for_pleasure == "Between 1 and 1.5 hours"] <- 1
df2$duration_walking_for_pleasure[df2$duration_walking_for_pleasure == "Between 1.5 and 2 hours"] <- 1
df2$duration_walking_for_pleasure[df2$duration_walking_for_pleasure == "Between 2 and 3 hours"] <- 1

df2$duration_walking_for_pleasure[df2$duration_walking_for_pleasure == "Over 3 hours"] <- 2

table(df2$duration_walking_for_pleasure, useNA = 'ifany')


#103. cereal type
df2 <- subset(df2, select = -c(cereal_type))

#104. oily fish intake
df2$oily_fish_intake[df2$oily_fish_intake == "Do not know"] <- NA
df2$oily_fish_intake[df2$oily_fish_intake == "Prefer not to answer"] <- NA

df2$oily_fish_intake[df2$oily_fish_intake == "Never"] <- 0

df2$oily_fish_intake[df2$oily_fish_intake == "Less than once a week"] <- 1
df2$oily_fish_intake[df2$oily_fish_intake == "Once a week"] <- 1
df2$oily_fish_intake[df2$oily_fish_intake == "2-4 times a week"] <- 1
df2$oily_fish_intake[df2$oily_fish_intake == "5-6 times a week"] <- 1
df2$oily_fish_intake[df2$oily_fish_intake == "Once or more daily"] <- 1

table(df2$oily_fish_intake, useNA = 'ifany')

#105. cancer record format
df2 <- subset(df2, select = -c(cancer_record_format))

#106. pork intake
df2$pork_intake[df2$pork_intake == "Do not know"] <- NA
df2$pork_intake[df2$pork_intake == "Prefer not to answer"] <- NA

df2$pork_intake[df2$pork_intake == "Never"] <- 0

df2$pork_intake[df2$pork_intake == "Less than once a week"] <- 1
df2$pork_intake[df2$pork_intake == "Once a week"] <- 1
df2$pork_intake[df2$pork_intake == "2-4 times a week"] <- 1
df2$pork_intake[df2$pork_intake == "5-6 times a week"] <- 1
df2$pork_intake[df2$pork_intake == "Once or more daily"] <- 1

table(df2$pork_intake, useNA = 'ifany')

#df2 (copy of ukb_categorical) now cleaned


###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################



#REMOVE specific ukb_continuous variables
#Remove some "integer" variables are CORRUPT (e.g. "_intake" variables) and results in HIGH NAs
#or remove pointless ukb admin variable (e.g.. volume of blood sample held by ukb)


#First, create a copy of ukb_continuous
df3 <- ukb_continuous


#Remove integer variables contains "frequency_"
#################
#PLEASE NOTE SOLARIUM LAMP VARIABLE NEEDS TO BE REPLACED BACK AS THIS IS AN IMPORTANT VARIABLE
#################
df3 %>% select(contains("frequency_")) %>% names

df3 <- subset(df3, select = -c(frequency_of_travelling_from_home_to_job_workplace,
                               frequency_of_solarium_sunlamp_use))

#remove childhood sunburn
#################
#PLEASE NOTE CHILDHOOD SUNBURNS VARIABLE NEEDS TO BE REPLACED BACK AS THIS IS AN IMPORTANT VARIABLE
#################
df3 <- subset(df3, select = -c(childhood_sunburn_occasions))

#Remove variable containing "volume of sample held by ukb"
df3 %>% select(starts_with("volume_of")) %>% names

df3 <- subset(df3, select = -c(volume_of_edta2_plasma_held_by_ukb,
                               volume_of_edta1_plasma_held_by_ukb,
                               volume_of_edta2_buffy_held_by_ukb,
                               volume_of_edta2_buffy_held_by_ukb,
                               volume_of_serum_held_by_ukb,
                               volume_of_acd_held_by_ukb,
                               volume_of_li_hep_plasma_held_by_ukb,
                               volume_of_edta1_buffy_held_by_ukb,
                               volume_of_edta1_red_cells_held_by_ukb,
                               volume_of_edta2_red_cells_held_by_ukb,
                               volume_of_rna_held_by_ukb))



#Remove integer variable containing "_intake"
df3 %>% select(contains("_intake")) %>% names

df3 <- subset(df3, select = -c(tea_intake,
                               average_weekly_intake_of_other_alcoholic_drinks,
                               water_intake,
                               fresh_fruit_intake,
                               average_weekly_spirits_intake,
                               cereal_intake,
                               average_weekly_champagne_plus_white_wine_intake,
                               average_weekly_fortified_wine_intake,
                               salad_raw_vegetable_intake,
                               average_weekly_red_wine_intake,
                               average_weekly_beer_plus_cider_intake))

#Remove integer variables contain "time"
df3 %>% select(contains("time")) %>% names

df3 <- subset(df3, select = -c(time_spent_outdoors_in_winter,
                               time_employed_in_main_current_job,
                               time_spend_outdoors_in_summer,
                               year_ended_full_time_education,
                               age_completed_full_time_education))

#Remove integer variables contain "first"
df3 %>% select(contains("first")) %>% names

df3 <- subset(df3, select = -c(cancer_year_age_first_occurred,
                               age_at_first_live_birth))


#Remove integer variables contain "duration"
df3 %>% select(contains("duration")) %>% names

df3 <- subset(df3, select = -c(sleep_duration,
                               duration_of_walks,
                               duration_of_moderate_activity,
                               duration_of_vigorous_activity))


#Remove integer variables contain "number_of"
df3 %>% select(contains("number_of")) %>% names

df3 <- subset(df3, select = -c(number_of_days_week_walked_10_minutes,
                               number_of_cigarettes_previously_smoked_daily,
                               number_of_cigarettes_currently_smoked_daily_current_cigarette_smokers,
                               number_of_self_reported_non_cancer_illnesses,
                               number_of_operations_self_reported,
                               number_of_night_shifts_worked_monthly_during_night_shift_periods,
                               number_of_live_births,
                               number_of_treatments_medications_taken,
                               number_of_self_reported_cancers,
                               number_of_days_week_of_moderate_physical_activity_10_minutes,
                               number_of_days_week_of_vigorous_physical_activity_10_minutes,
                               number_of_night_shifts_worked_monthly_during_mixed_shift_periods))

#Remove integer variables starts with "rest_"
df3 %>% select(starts_with("rest_")) %>% names

df3 <- subset(df3, select = -c(rest_days_during_mixed_shift_periods,
                               rest_days_during_night_shift_periods))

#Remove integer variables starts with "age_"
df3 %>% select(starts_with("age_")) %>% names
#Except do NOT remove "age_at_recruitment", 'age_at_death', 'age_at_cancer_diagnosis', 'age_when_attended_assessment_centre'

df3 <- subset(df3, select = -c(age_stopped_smoking,
                               age_of_primiparous_women_at_birth_of_child,
                               age_started_smoking_in_former_smokers,
                               age_at_death,
                               age_started_smoking_in_current_smokers,
                               age_started_hormone_replacement_therapy_hrt,
                               age_when_periods_started_menarche,
                               age_when_last_used_oral_contraceptive_pill,
                               age_started_oral_contraceptive_pill,
                               age_last_used_hormone_replacement_therapy_hrt
                               ))

#remove number_in_household
df3 <- subset(df3, select = -c(number_in_household))

#remove integer variables containing "exposure_to"
df3 %>% select(contains("exposure_to")) %>% names

df3 <- subset(df3, select = -c(exposure_to_tobacco_smoke_at_home,
                               exposure_to_tobacco_smoke_outside_home))

#remove integer variables containing "consecutive_"
df3 %>% select(contains("consecutive_")) %>% names

df3 <- subset(df3, select = -c(consecutive_night_shifts_during_mixed_shift_periods,
                               consecutive_night_shifts_during_night_shift_periods))





###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################


#Recombine df2 and df3 for new ukb df
ukb_pruned <- cbind(df3, df2)



