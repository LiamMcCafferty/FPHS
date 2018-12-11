#Code designed to process raw data from the q1_survey into useable form.
#Optionally you can load the annotated dataset made in file
#"understanding_data.R" and use function comment() to understand original
#variables.

#Clearing global cache
rm(list = ls())

#Load in tidyverse, necessary for most processing.
library(tidyverse)

#Set working directory to current file location
setwd("~/Desktop/FPHS Analysis/Final Processing Set")

#Loading annotated dataset, uncomment to access
# load("annotated_dataset")
# annotated_dataset <- dataset

#Loading normal dataset and character only dataset in case
dataset <- read.csv("q1_test_data.csv")
dataset_character <- read.csv("q1_test_data.csv", colClasses = "character")

#Eliminating non "_etl" variables in datasets
dataset_character <- dataset_character %>% select(persnet_id, dob_etl, startplay_age_etl, numb_season_etl, position___1_etl:yr_dx_cancer)
dataset <- dataset %>% select(persnet_id, dob_etl, startplay_age_etl, numb_season_etl, position___1_etl:yr_dx_cancer)




############################## Player position ################################

#Selecting position data
p_position <- dataset %>% select(persnet_id, position___1_etl:position___10_etl)

#Saving ordered list of player positions
pos_names <- c("Offensive Line", "Defensive Line", "Linebacker",
               "Defensive Back", "Running Back", "Wide Receiver",
               "Tight End", "Quarterback", "Kicker/Punter", "Special Teams")

#Naming each column to its correct identify
colnames(p_position) <- c("persnet_id", pos_names)

#First gathers the count of each position by persnet_id, filters out counts of
#  0, arranges by persnet id, and then only selects the names of each count to
#  have a record of the names. Grouped by persnet ID to give a per-persnet setup.
#  Slice determines which section we select (1 = first from left)
position1 <- p_position %>% gather(player_position, count, -persnet_id) %>%
  filter(count == 1) %>% arrange(persnet_id) %>% select(-count) %>%
  group_by(persnet_id) %>% slice(1) %>% data.frame()

#Same but slice 2
position2 <- p_position %>% gather(player_position, count, -persnet_id) %>%
  filter(count == 1) %>% arrange(persnet_id) %>% select(-count) %>%
  group_by(persnet_id) %>% slice(2) %>% data.frame()

#Assiging factors for each player position.
position1$player_position <- factor(position1$player_position,
                                    levels = c("Offensive Line", "Defensive Line",
                                               "Linebacker", "Defensive Back",
                                               "Running Back", "Wide Receiver",
                                               "Tight End", "Quarterback",
                                               "Kicker/Punter", "Special Teams"))

position2$player_position <- factor(position2$player_position,
                                    levels = c("Offensive Line", "Defensive Line",
                                               "Linebacker", "Defensive Back",
                                               "Running Back", "Wide Receiver",
                                               "Tight End", "Quarterback",
                                               "Kicker/Punter", "Special Teams"))

#Combining positions then assigning correct names for import into larger dataset
positioning <- left_join(position1, position2, by = 'persnet_id')
colnames(positioning) <- c("persnet_id", "player_position1", "player_position2")

#Assignging to larger dataset
dataset <- left_join(dataset, positioning, by = "persnet_id")


######################### Global Promis Scores ################################
for(i in grep("global", colnames(dataset))){
  dataset[,i] <- factor(dataset[,i], levels = c(1:5),
                        labels = c("Poor","Fair", "Good", "Very Good", "Excellent"))
}

########################  Nuero QOL scores  ###################################
for(i in grep("nqcog", colnames(dataset))){
  dataset[,i] <- factor(dataset[,i], levels = c(1:5),
                        labels = c("Very Often (Several times a day)",
                                   "Often (about once a day)",
                                   "Sometimes (2-3 times)", "Rarely (once)",
                                   "Never"))
}

########################     Generics      ####################################
dataset$pcp <- factor(dataset$pcp, levels = c(0, 1), labels = c("No", "Yes"))
dataset$other_health_professional <- factor(dataset$other_health_professional,
                                            levels = c(0, 1), labels = c("No", "Yes"))

############################## Question 65 ####################################

#Pre assignging column names for surgery binaries
question_65_bin <- c("knee_joint_replacement", "hip_joint_replacemen",
                     "cardiac_surgery", "cataract_surgery",
                     "neck_spine_surgery", "back_surgery1",
                     "othersurgery")

question_65_years <- c("approxyrssurg_knee", "approxyrssurg_hip",
                       "approxyrssurg_cardiac", "approxyrssurg_cataract",
                       "approxyrssurg_neckspine", "approxyrssurg_back",
                       "years_other_surgery")

#Similar process to position setup
surgery <- dataset %>% select("persnet_id", question_65_bin)

#Changing NA's into 0's.
surgery[is.na(surgery)] <- 0

surgery1 <- surgery %>% gather(surgery, count, -persnet_id) %>% filter(count == 1) %>% 
  arrange(persnet_id) %>% select(-count) %>% group_by(persnet_id) %>% slice(1) %>% 
  data.frame()

surgery2 <- surgery %>% gather(surgery, count, -persnet_id) %>% filter(count == 1) %>% 
  arrange(persnet_id) %>% select(-count) %>% group_by(persnet_id) %>% slice(2) %>% 
  data.frame()

surgery3 <- surgery %>% gather(surgery, count, -persnet_id) %>% filter(count == 1) %>% 
  arrange(persnet_id) %>% select(-count) %>% group_by(persnet_id) %>% slice(3) %>% 
  data.frame()

surgery_labels <- c("Knee Joint Replacement", "Hip Joint Replacement", "Cardiac",
                    "Cataract", "Neck/Spine", "Back", "Other")


surgery1$surgery <- factor(surgery1$surgery,
                           levels = question_65_bin, 
                           labels = surgery_labels)

surgery2$surgery <- factor(surgery2$surgery,
                           levels = question_65_bin, 
                           labels = surgery_labels)

surgery3$surgery <- factor(surgery3$surgery,
                           levels = question_65_bin, 
                           labels = surgery_labels)

full_surgery <- left_join(surgery1, surgery2, by = 'persnet_id')
full_surgery <- left_join(full_surgery, surgery3, by = 'persnet_id')
colnames(full_surgery) <- c("persnet_id", "surgery1", "surgery2", "surgery3")

dataset <- left_join(dataset, full_surgery, by = "persnet_id")

#Post creating factor versions, assigning factors to each binary in case
#  individual variables would be used.
for(i in question_65_bin){
  dataset[,i] <- factor(dataset[,i], levels = c(0, 1),
                        labels = c("No", "Yes"))
}


############################## Question 67 ####################################

#Same setup as question 65 and 13.
question_67_bin <- c("heart_attack", "stroke", "sleep_apnea", "dementia",
                     "cte", "parkinsons", "arthritis", "als",
                     "renal_kidney_disease", "cancer")


question_67_years <- c("yr_dx_heart_attack", "yr_dx_stroke", "yr_dx_sleepapnea",
                       "yr_dx_dementia", "yr_dx_cte", "yr_dx_parkinsons",
                       "yr_dx_arthritis", "yr_dx_als",
                       "yr_dx_kidney_dx", "yr_dx_cancer")


disease <- dataset %>% select("persnet_id", question_67_bin)

disease[is.na(disease)] <- 0

disease1 <- disease %>% gather(disease, count, -persnet_id) %>% filter(count == 1) %>% 
  arrange(persnet_id) %>% select(-count) %>% group_by(persnet_id) %>% slice(1) %>% 
  data.frame()

disease2 <- disease %>% gather(disease, count, -persnet_id) %>% filter(count == 1) %>% 
  arrange(persnet_id) %>% select(-count) %>% group_by(persnet_id) %>% slice(2) %>% 
  data.frame()

disease3 <- disease %>% gather(disease, count, -persnet_id) %>% filter(count == 1) %>% 
  arrange(persnet_id) %>% select(-count) %>% group_by(persnet_id) %>% slice(3) %>% 
  data.frame()

disease_labels <- c("Heart attack", "Stroke", "Sleep Apnea", "Dementia", "CTE",
                    "Parkinsons", "Arthritis", "ALS", "Kidney Disease", "Cancer")

disease1$disease <- factor(disease1$disease,
                           levels = question_67_bin, 
                           labels = disease_labels)

disease2$disease <- factor(disease2$disease,
                           levels = question_67_bin, 
                           labels = disease_labels)

disease3$disease <- factor(disease3$disease,
                           levels = question_67_bin, 
                           labels = disease_labels)

full_disease <- left_join(disease1, disease2, by = 'persnet_id')
full_disease <- left_join(full_disease, disease3, by = 'persnet_id')
colnames(full_disease) <- c("persnet_id", "disease1", "disease2", "disease3")

dataset <- left_join(dataset, full_disease, by = "persnet_id")

#Assining binaries to factors in case.
for(i in question_67_bin){
  dataset[,i] <- factor(dataset[,i], levels = c(0, 1),
                        labels = c("No", "Yes"))
}

######### Renaming dataset variables to something reasonable ##################

#Saving original variable names
original_dataset <- dataset

#Assigning reasonable variable names. Use newer data dictionary for this.
colnames(dataset) <- c("persnet_id", "dob", "startplay_age", "num_seasons", "position___1",
                       "position___2","position___3","position___4","position___5",
                       "position___6","position___7","position___8","position___9",
                       "position___10","promis1","promis2","promis3","promis4",
                       "promis5","promis6","promis7","promis8","promis9",
                       "nqcog1","nqcog2","nqcog3","nqcog4","nqcog5","nqcog6",
                       "nqcog7","nqcog8","nqcog9","nqcog10","nqcog11",
                       "primary_care_provider", "other_health_professional",
                       "surgery_knee", "surgery_knee_years", "surgery_hip",
                       "surgery_hip_years", "surgery_cardiac", "surgery_cardiac_years",
                       "surgery_cataract", "surgery_cataract_years", "surgery_neckspine",
                       "surgery_neckspine_years", "surgery_back", "surgery_back_years",
                       "surgery_other", "surgery_other_type", "operator65",
                       "surgery_other_years", "health_heartattack",
                       "health_heartattack_years", "health_stroke",
                       "health_stroke_years", "health_sleepapnea",
                       "health_sleepapnea_years", "health_dementia",
                       "health_dementia_years", "health_cte", "health_cte_years",
                       "health_parkinsons", "health_parkinsons_years",
                       "health_arthritis", "health_arthritis_years", "health_als",
                       "health_als_years", "health_kidney", "health_kidney_years",
                       "health_cancer", "health_cancer_type", "operator67",
                       "health_cancer_years", "player_position1", "player_position2",
                       "surgery1", "surgery2", "surgery3", "disease1",
                       "disease2", "disease3")

#Save dataset.
save(dataset, file = "q1_processed.rda")
