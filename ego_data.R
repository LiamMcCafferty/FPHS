################################################################################
# PROJECT: Harvard NFL Players Health Study 
# PURPOSE: Import raw data from REDCap NFL survey and clean up ego centric data
# DIR:     "~/Desktop/NPHS Analysis"
# INPUTS:  Fake data created in REDCap ("fakedata_double.csv") 
#          Can be replaced with real data of participants.
# OUTPUTS: A ego_data.rda file that contains data on Ego.
# AUTHORS: Liam McCafferty, Abby Halm, Nuzulul Kurniansyah, Amar Dhand
# CREATED: 10/29/18
# LATEST:  11/6/18
# PSERIES: NA
# NSERIES: NA
# NOTES:   Adapted from Personal Health Survey Clean Data 1 code, found at:
#            https://github.com/AmarDhand/PersonalNetworks
# ##############################################################################

#Empties Global Environment cache
rm(list = ls())

#Set working directory to current file location
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".
setwd("~/Desktop/FPHS Analysis")

#Importing packages. If not yet installed, packages can be installed by going to:
#  Tools -> Install Packages, then enter their exact names from within each
#  library()
library(tidyverse)

#Read in data
#Imports data and assigns it to variable "sample_data"
sample_data <- read.csv("fakedata_long.csv", 
	stringsAsFactors = FALSE)
#Stores "sample_data" as a table data frame for easier reading
base_data <- sample_data <- tbl_df(sample_data)

#LM: I have decided to try to organise this data in the order of our code base,
#    having to do with respondent/ego questions.

##The remaining code sets variable types and assigns levels to categorical
#  variables. We given a detailed annotation of this process for the variable 
#  "sex" below. Subsequent variables follow the same pattern. 

#LM: sex is not used in this dataset, however I have left the original code
#commented out here, for further refernce.

# #Demographics of Central Person (Ego)
# #ego's sex, stored as variable "sex", is made into a factor 
# sample_data$sex <- factor(sample_data$sex, levels = c("0", "1", "2"))
# #assigns levels to variable "sex"
# levels(sample_data$sex) <- c("Female","Male","Other")

###############################################################################
####################### Ego Questions #########################################
###############################################################################

#Study ID######################################################################
#Set study_id variable to category to signify the unique values of each
#study_id. This may not be necessary.
sample_data$study_id <- factor(sample_data$study_id)
#When merging with Q1 data, the factor vs integer designation may cause
#issue, so circle back to reset to integer if necessary.


#Ego's race####################################################################
#Due to multiple choice, code below organizes particpant's choices
#  into race1 and race2. If the participant only chooses 1 race, the value for 
#  "race2" will be NA
r <- sample_data %>% select(study_id, race___1:race___77)
colnames(r) <- c("study_id", "Black or African American", "White",
                 "American Indian/Alaska Native", "Asian",
                 "Native Hawaiian or Other Pacific Islander", "Other")
#creates variable, "race1", that contains the first race a participant chooses
#  if the participant selects multiple races, then "race1" variable represents
#  the race that appears first in the list of choices, and does NOT denote any 
#  ordering assigned by the participant 
race1 <- r %>% gather(race, count, -study_id) %>% filter(count == 1) %>% 
  arrange(study_id) %>% select(-count) %>% group_by(study_id) %>% slice(1) %>% 
  data.frame()
#creates variable, "race2", that contains the second race a participant chooses
#  if the participant selects multiple races, then "race2" variable represents
#  the race that appears second in the list of choices, and does NOT denote any 
#  ordering assigned by the participant 
race2 <- r %>% gather(race, count, -study_id) %>% filter(count == 1) %>% 
  arrange(study_id) %>% select(-count) %>% group_by(study_id) %>% slice(2) %>% 
  data.frame()

#creates a table that combines "race1" and "race2" by study_id
race <- left_join(race1, race2, by = 'study_id')
colnames(race) <- c("study_id", "race1", "race2")
#adds "race" table onto "sample_data", thus adding variables "race1" and "race2"
#  to the original data frame, containing all variables
sample_data <- left_join(sample_data, race, by = "study_id") %>% 
  select(-race___1:-race___88)

#Ego's Ethnicity###############################################################
sample_data$ethnicity <- factor(sample_data$ethnicity, levels = c(0,1,99,88))
levels(sample_data$ethnicity) <- c("Hispanic or Latino",
                                   "NOT Hispanic or Latino", "Unknown",
                                   "Skip Question")

#Education#####################################################################
sample_data$edu <- factor(sample_data$edu, 
	levels = c("1,", "2", "3", "4", "5", "6", "88"))
levels(sample_data$edu) <- c("Some high school or less", "High school grad", 
	"Some college", "Associate degree", "Bachelor's degree", "Graduate degree",
	"Prefer not to answer")

#Ego Zip Code##################################################################
#Zip codes have a habit of being mangled by the read.csv import tool as many
#zips will have 0's at the beginning of their number. Instead we re-import the
#data as characters, and only save the zip
zip_logic <- match("zip" ,colnames(base_data))
col_logic <- c(rep("NULL", times = (zip_logic-1)),"character",
               rep("NULL", times = ncol(base_data) - (zip_logic+1)))
zip_import <- read.csv("fakedata_long.csv", colClasses = col_logic)
sample_data$zip <- zip_import$zip
rm(zip_import,col_logic,zip_logic)
#AD: look at my zip code mapping short code. It allows you to map all participants
#on a map of the US. Helpful quick visual.


#Ego Employment################################################################

#Employment has the problem where the answer can be a checkbox. However we can't
#just lump all multiple selections together as we can with a "mixed race"
#person. Thus we need some way to split up our cohort. Instead I will split the
#employment data by what question may be asked for each variable. Note that NA's
#mean that the Ego does not fall under that question or has no data.

#employed: What is the general employment status of the Ego?
#  (Employed, Student, Retired, Unemployed)
#unemployed: What is the unemployment situation the Ego is in?
#  (Looking for work, Not looking for work)
#emp_nfl: Is the Ego currently employed in football/the nfl?
#  (Football, No Ball)
#stu_work: Is the Ego a working student?
#  (Working Student, Student)

employment <- select(sample_data, employment___1:employment___5)
employment$stu_work <-employment$unemployed <- NA
employment$emp_nfl <- employment$employed <- NA

for(i in 1:nrow(employment)){
  #Iterates through each Ego. Ifelse statements are used to establish a heirarcy
  #of factors. (Employed/Football Employed) > (Student/Retired) >
  #(Unemployed/Looking for Work). With minor caveat that a person who is retired
  #but also looking for work is implied to not want to be retired, thus we have
  #to assume they are unemployed and looking for work. This for loop assigns
  #mutiple variables in each ifelse for efficiency using nested ifelse's.
  if(employment[i,]$employment___1 == 1 | employment[i,]$employment___2 == 1){
    #Employed ifelse
    employment[i,]$employed <- "Employed"
    if(employment[i,]$employment___1 == 1){
      employment[i,]$emp_nfl <- "Football"
    }else if(employment[i,]$employment___2 == 1){
      employment[i,]$emp_nfl <- "No Ball"
    }
    employment[i,]$stu_work <- ifelse(employment[i,]$employment___4 == 1,
                                      "Working Student", NA)
  }else if(employment[i,]$employment___4 == 1){
    #Student ifelse
    employment[i,]$stu_work <- employment[i,]$employed <- "Student"
    
  }else if(employment[i,]$employment___3 == 1 &
           employment[i,]$employment___5 != 1){
    #Retired ifelse, note the requirement to not be looking for work
    employment[i,]$employed <- "Retired"
    
  }else if(employment[i,]$employment___5 == 1 |
           employment[i,]$employment___0 == 1){
    #Unemployed ifelse
    employment[i,]$employed <- "Unemployed"
    
    if(employment[i,]$employment___5 == 1){
      employment[i,]$unemployed <- "Looking for work"
    }else if(employment[i,]$employment___0 == 1){
      employment[i,]$unemployed <- "Not looking for work"
    }
  }
}

#turning each employment categorical variable into a factor.
employment$employed <-   factor(employment$employed,
                                levels = c("Employed", "Student",
                                           "Retired", "Unemployed"))

employment$emp_nfl <-    factor(employment$emp_nfl,
                                levels = c("Football","No Ball"))

employment$unemployed <- factor(employment$unemployed,
                                levels = c("Looking for work",
                                           "Not looking for work"))

employment$stu_work <-   factor(employment$stu_work,
                                levels = c("Student","Working Student"))
#AD: Add relevant columns to sample data. Can check with names(sample_data)

#Ego Occupation################################################################
sample_data$occupation <- factor(sample_data$occupation, levels = c("1", "2", 
	"3", "4", "5", "6", "7", "8", "9", "10", "77"))
#note that for participants who do not select "employment___2" (employed) will
#  have NA's as their value.
levels(sample_data$occupation) <- c("Executive, manager", 
	"Sales or clerical worker", "Mechanic, electrician, skilled worker", 
	"Machine operator, inspector, bus/cab driver", "Service worker", 
	"Professional", "Business owner", "Laborer, unskilled worker", "Farming", 
	"Military", "Other")


#Ego Income####################################################################
sample_data$income <- factor(sample_data$income, levels = c("1", "2", "3", "4",
	"5"))
levels(sample_data$income) <- c("less than $5,000", "$5,000 to $49,000", 
	"$50,000 to $169,000", "$170,000 to $499,000", "more than $500,000")


#Ego Alcohol###################################################################
sample_data$alcohol <- factor(sample_data$alcohol, levels = c(1, 0, 9))
levels(sample_data$alcohol) <- c("Yes", "No", "I do not drink heavily")

#Ego Supplements###############################################################
long_speach <- "I do not use performance enhancing drugs or supplements"
sample_data$supplements <- factor(sample_data$supplements, levels = c(1, 0, 9))
levels(sample_data$supplements) <- c("Yes", "No", long_speach)

#Ego Exercise##################################################################
sample_data$exercise <- factor(sample_data$exercise, levels = c(1,0))
levels(sample_data$exercise) <- c("Yes","No")

#Ego Diet######################################################################
sample_data$diet <- factor(sample_data$diet, levels = c(1,0))
levels(sample_data$diet) <- c("Yes","No")

#Ego Health Problems###########################################################
#Ego Health problems organized into columns
#The code below organizes the Ego's Health Problems (in which the participant
#  can select multiple choices) into columns. 
#same code as for "race" variable
h <- sample_data %>% select(study_id, health___1:health___0)
colnames(h) <- c("study_id", "General Health", "Pain", "Cognitive/Mental Health",
                 "Cardiac", "No Problems")
#creates variable, "health_prob1", that contains the first health problem a 
#  participant chooses if the participant selects multiple health problems, 
#  then "health_prob1" variable represents the health problem that appears first
#  in the list of choices on REDCap, and does NOT denote any ordering 
#  assigned by the participant 
#The same code is then used to create variables for any second, third, or fourth
#  health problems the participant chooses.
health_prob1 <- h %>% gather(health_prob, count, -study_id) %>% 
  filter(count == 1) %>% arrange(study_id) %>% select(-count) %>% 
  group_by(study_id) %>% slice(1) %>% data.frame()
health_prob2 <- h %>% gather(health_prob, count, -study_id) %>% 
  filter(count == 1) %>% arrange(study_id) %>% select(-count) %>% 
  group_by(study_id) %>% slice(2) %>% data.frame()
health_prob3 <- h %>% gather(health_prob, count, -study_id) %>% 
  filter(count == 1) %>% arrange(study_id) %>% select(-count) %>% 
  group_by(study_id) %>% slice(3) %>% data.frame()
health_prob4 <- h %>% gather(health_prob, count, -study_id) %>% 
  filter(count == 1) %>% arrange(study_id) %>% select(-count) %>% 
  group_by(study_id) %>% slice(4) %>% data.frame()
health_problems <- left_join(health_prob1, health_prob2, by = 'study_id')
health_problems <- left_join(health_problems, health_prob3, by = 'study_id')
health_problems <- left_join(health_problems, health_prob4, by = 'study_id')
colnames(health_problems) <- c("study_id", "health_problem1", "health_problem2",
                               "health_problem3", "health_problem4")
sample_data <- left_join(sample_data, health_problems, by = "study_id") %>% 
  select(-health___1:-health___0)

#Network Size and Ties#########################################################

##Calculate total network size. Defined as all unique names entered in name
#generator boxes and extra boxes provided.

calculate_size <- function(x) {
  ##########
  # Function: Creates a network_size variable that takes into account any names 
  #           written in the extra names boxes
  # Inputs: x = Variable that stores the dataset
  # Ouputs: network_size variable for each ID 
    ##########
  #first select all names put in the first 15 columns 
  names_first_15 <- sample_data %>% select(name1, name2, name3, name4, name5, 
  	name6, name7, name8, name9, name10, name11, name12, name13, name14, name15)
  
  #next, select the names for id x
  names_first_15 <- names_first_15[x, ]
  
  #create data frame and transpose it to make it easier to manage 
  names_first_15 <- as.data.frame(t(names_first_15))	
  
  #change the column name
  colnames(names_first_15) <- c("Names")
  	
  #select the keep/remove designation, stored as variables "name_1" to "name_15"
  #  for each of the first 15 names 
  keep_names <- sample_data %>% select(name_1:name_15)
  keep_names <- keep_names[x, ]
  
  #change colnames to numbers 1:15, so that it is easier to do rbind
  colnames(keep_names) <- c(1:15)
  
  #input the data into a data frame and transpose it
  keep_names <- data.frame(t(keep_names))
  
  #change the name of the column to "Value"
  colnames(keep_names) = "Value"
  
  #combine "names_first_15" (the first 15 names entered) and "keep_names" (the 
  #  keep/remove designation for each of the first 15 names) using cbind function
  names_combined <- cbind(names_first_15, keep_names)
  
  #remove any row that contain NA in names_combined
  names_combined <- names_combined[complete.cases(names_combined), ]
  
  #split names_combined into names designated as "keep" (Value = 1) and 
  #  names designated as "remove" (Value = 0)
  names_combined_keep <- split(names_combined, names_combined$Value == 1)
  
  # Select only the names designated as $`TRUE` ("keep")
  names_combined_keep <- names_combined_keep$`TRUE`
  
  #Change all characters into Uppercase
  names_combined_keep <- toupper(names_combined_keep$Names)
  
  #Remove any spaces 
  names_combined_keep <- gsub(" ", "", names_combined_keep)
  
  #Make names_combined_keep into a data frame to make it easier to manage
  names_combined_keep <- data.frame(names_combined_keep)
  colnames(names_combined_keep) <- "Names"
  
  #Now, take all of the names from the 3 extra names boxes 
  #  Strsplit : split names based on coma saparated value and change them into 
  #  characters. 
  names_box1 <- strsplit(as.character(sample_data$more_names_1)[x], 
  	split = ",")
  names_box2 <- strsplit(as.character(sample_data$more_names_2)[x], 
  	split = ",")
  names_box3 <- strsplit(as.character(sample_data$more_names_3)[x], 
  	split = ",")
  
  #Unlist names_box1:names_box3 and create a vector of names for each extra names 
  #  box
  names_box1 <- as.vector(unlist(names_box1, use.names = FALSE))
  names_box2 <- as.vector(unlist(names_box2, use.names = FALSE))
  names_box3 <- as.vector(unlist(names_box3, use.names = FALSE))
  
  #combine the 3 extra names vectors into list so that we can combine 
  #  names_box1:3 into one vector
  names_box <- list(names_box1, names_box2, names_box3)
  
  #make the names_box list into a vector
  names_box <- Reduce(c, names_box)
  
  #remove "NA" in names_box
  names_box <- names_box[!is.na(names_box)]
  
  #Remove Spaces in names_box
  names_box <- gsub(" ", "", names_box)
  
  #Change all character in names_box to uppercase 
  names_box <- toupper(names_box)
  
  #Remove duplicates values in names_box vector
  names_box <- unique(names_box)
  
  #makes names_box into a data frame and change the column name to "Names" 
  #  to make it easier to merge with names_combined_keep
  names_box <- data.frame(names_box)
  colnames(names_box) <- "Names"
  
  # Merge unique names from boxes with unique names of first 15 and 
  #remove duplicates between them 
  #  Keep this order. Placing names_combined_keep first preserves any duplicate 
  #  names that both designated as "keep" by the participant 
  names_network <- merge(names_combined_keep,names_box,by = c("Names"), all = TRUE)
  
  # convert names_network into a vector
  names_network <- as.vector(t(names_network))
  
  #calculate the total network size
  total_size <- length(names_network)
  return(total_size)
}

#apply 'calculate_size' function to all study IDs
network_size <- unlist(lapply(1:nrow(sample_data), calculate_size))

#merge network_size and remove other size variables to reduce confusion
sample_data <- cbind(sample_data, network_size) %>% select(-size, -first)

#Ego's NFL Team################################################################
team_list <- c("49ers", "Bears", "Bengals", "Bills", "Broncos", "Browns",
               "Buccaneers", "Cardinals", "Chargers", "Chiefs", "Colts",
               "Cowboys", "Dolphins", "Eagles", "Falcons", "Giants", "Jaguars",
               "Jets", "Lions", "Packers", "Panthers", "Patriots", "Raiders",
               "Rams", "Ravens", "Redskins", "Saints", "Seahawks", "Steelers",
               "Texans", "Titans", "Vikings")
sample_data$nfl_team <- factor(sample_data$nfl_team, c(1:32))
levels(sample_data$nfl_team) <- team_list




#create ego data file of data frame with all changes made in code
save(sample_data, file = "ego_data.rda")



