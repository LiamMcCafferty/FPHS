#Analysis of Alters

#Removing everything from the global environment
rm(list = ls())

#setting working directory
setwd("~/Desktop/FPHS Analysis")

#attaching important packages
library(tidyverse)

#Loading data made in alter_data.R and ego_data.R. Loaded variables are
#"alter_frame" and "sample_data" respectively
load("alter_data.rda")
load("ego_data.rda")


#Function which counts the number/proportion of each factor per study_id, single answers.
counter_single <- function(x, column, as_prop = TRUE){
  #Function which takes a study_id limited set of alter data and outputs the
  #number/proportion of alters which have each level of the factor.
  
  #Inputs:
  #  x - subset of alter_frame data that only includes data from a single study_id
  #  column - name, in string form, of the column/variable to be checked
  #  as_prop - logical which determines if the output is going to be a
  #    proportion or not.
  
  #Output:
  #  output_vector - a named vector in which each entry is a factor level and the
  #    number/proportion of ties which have that level as a value. Access the levels
  #    of the factor, saves them as a string to be iterated through.
  possible_levels <- levels(unlist(x[column]))
  
  #Blank vector to store each sum.
  output_vector <- c()
  
  #Iterates through each possible value, and the value of NA, and checks if they
  #are present in the indicated column. Then it multiply's this logical by 1 to
  #convert to binary, then sums the number of 1's. Assigning it to the blank
  #vector.
  for(i in c(possible_levels, NA)){
    output_vector[i] <- sum((unlist(x[column]) %in% i) * 1)
  }
  
  #If the funtion is set to output as a proportion, this divides it by the
  #total, giving a proportion.
  if(as_prop){
    output_vector <- output_vector / sum(output_vector)
  }
  
  return(output_vector)
}

#Function which changes the factor counts to per factor from study_id, single answers
counter_single_splitter <- function(x, column, as_prop = TRUE){
  #Function which takes the alter_frame data and converts it to a dataframe
  #which can be cbound to the facts dataframe
  
  #Inputs:
  #  x - alter_frame data
  #  column - name, in string form, of the column/variable to be checked
  #  as_prop - logical which determines if the output is going to be a
  #    proportion or not.
  
  #Output: output_df - a named dataframe organised by study_id with the
  #information entered into.
  
  #Creates blank list to contain output data.
  holder <- list()
  
  #Iterates through each study id in the entered alter_frame. by() kept returning
  #errors, so I used this forloop to do the basic logic of by()
  for(i in levels(x$study_id)){
    #Isolates the rows in the entered alter_frame by the iterated study_id.
    workbench <- subset.data.frame(x, x$study_id == i)
    #runs the counter function and assigns output to holder function.
    holder[[i]] <- counter_single(workbench, column, as_prop)
  }
  
  #Takes the holder list and converts it to a dataframe, where each column is an
  #entrie in the list. The the names in the list are assigned to the df,
  #including NA which requires a seperate line to make the NA into a character.
  #Finally a suffix of the column name is added to each variable to identify
  #them in the larger "facts" dataframe.
  output_df <- as.data.frame(matrix(unlist(holder), nrow = length(holder), byrow = TRUE))
  colnames(output_df) <- names(holder[[1]])
  colnames(output_df)[length(colnames(output_df))] <- "NA"
  colnames(output_df) <- paste(colnames(output_df), "_",column, sep = "")
  
  return(output_df)
}

#Function which counts the number/proportion of each factor per study_id, checkbox answers.
counter_checkbox <- function(x, checkbox_prefix, as_prop = TRUE){
  #Function which takes a study_id limited set of alter data and outputs the
  #number/proportion of alters which have each level of the factor. As
  #checkboxes have multiple possible variables that the data can be contained
  #in, the code checks if the value is present at all for each alter, rather
  #than considering each variable independently.
  
  #Inputs:
  #  x - subset of alter_frame data that only includes data from a single study_id
  #  checkbox_prefix - prefix of the checkbox names which are contained in the data.
  #  as_prop - logical which determines if the output is going to be a
  #    proportion or not.
  
  #Output:
  #  output_vector - a named vector in which each entry is a factor level and the
  #    number/proportion of ties which have that level as a value. Access the levels
  #    of the factor, saves them as a string to be iterated through.
  
  
  #Using greple to access the correct rows that contain the prefix.
  workbench <- x[,grepl(checkbox_prefix, names(x))]
  
  #Accessing study id of the output for later accessing alter count.
  study_id <- unique(x$study_id)
  
  #Storing possible values of the set.
  possible_levels <- levels(unlist(workbench[1]))
  
  #Blank vector to store info.
  output_vector <- c()
  
  #Iterates through each possible value. Ignores NA's as NA's are always present
  #if the alter does not fill all checkbox variables. Apply checks if 
  for(i in possible_levels){
    output_vector[i] <- sum((apply(workbench, 1, function(x){return(i %in% x)})) * 1)
  }
  
  if(as_prop){
    output_vector <- output_vector / (facts[facts$study_id == 1,]$alter_count)
  }
  
  return(output_vector)
}

#Function which changes the factor counts to per factor from study_id, checkbox answers
counter_checkbox_splitter <- function(x, checkbox_prefix, as_prop = TRUE){
  
  
  
  holder <- by(x, x$study_id,counter_checkbox, checkbox_prefix = checkbox_prefix, as_prop = as_prop)
  
  output_list <- list()
  output_list[length(holder[[1]]) + 1] <- "FALSE ELEMENT"
  
  #Iterates through each variable in each study id, entering it into the current
  #variable vectors by appending them.
  for(i in 1:length(holder[[1]])){
    for(j in 1:length(holder)){
      output_list[[i]] <- append(output_list[[i]], holder[[j]][[i]])
    }
  }
  names(output_list) <- names(holder[[1]])
  output_list <- output_list[1:(length(output_list) - 1)]
  
  output_frame <- as.data.frame(matrix(unlist(output_list), ncol = length(output_list)))
  colnames(output_frame) <- paste(names(output_list), "_",checkbox_prefix, sep = "")
  
  return(output_frame)
}


#Making the facts dataframe
facts <- sample_data %>% select(study_id, network_size)

network_size <- sample_data %>% select(study_id, network_size)

facts$alter_count <- unlist(by(alter_frame, alter_frame$study_id, nrow))

#Ensuring the study id in alter_frame is a factor, appearently important in by()
alter_frame$study_id <- factor(alter_frame$study_id, levels = unique(alter_frame$study_id))

single_vars <- c("support", "sex", "neg", "educ", "speak", "length", "fb", "alcohol",
                 "supplement", "exer", "diet", "dist")

for(i in single_vars){
  facts <- cbind(facts, counter_single_splitter(alter_frame, i, as_prop = TRUE))
}

checkbox_prefixes <- c("supp_type","race","relat","fbwith","health")

for(i in checkbox_prefixes){
  facts <- cbind(facts, counter_checkbox_splitter(alter_frame, i, as_prop = TRUE))
}

#####ACTUAL ANALYSIS###########################################################


thing <- quote(+ geom_density(aes(x = unlist(facts["Variable"])), color = "Red"))
thing + thing

mean(unlist(facts["Male_sex"]))
sd(unlist(facts["Male_sex"]))

the_plot <- ggplot(facts) +
  geom_density(aes(x = unlist(facts["Male_sex"])), color = "Red") +
  geom_density(aes(x = unlist(facts["Male_sex"])), color = "Red") +
  geom_density(aes(x = unlist(facts["Female_sex"])), color = "Blue")

used <- facts[,grepl("sex" ,colnames(facts))]

colnames(used)

colors <- c("Red", "Blue", "green4", "deeppink", "Black", "gray49", "chocolate4", "plum3")

the_plot <- ggplot(facts) +
  geom_density(aes(x = unlist(facts["Male_sex"])), color = "Red") +
  geom_density(aes(x = unlist(facts[NULL])), color = NULL) +
  geom_density(aes(x = unlist(facts["Female_sex"])), color = "Blue")




