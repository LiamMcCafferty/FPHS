################################# Generic Set up ##############################
rm(list = ls())

library(plyr)
library(tidyverse)

setwd("~/Desktop/FPHS Analysis/")

dataset <- read.csv("fakedata_long.csv")

name_select <- dataset %>% select(name_1:name_15, name1:name5, name6:name10, name11:name15)

#Four checks being done by the function
#  1: Need to check if user selected keep on everything, easy.
#  2: Need to check if user has selected every name and there are redundant
#    names (can do a weighted/unweighted version).
#  3: Need to check if the user has multiple unique names but has not selected them all.
#  4: Need to check if the user has selected a name which is blank

############################## Functions to use: ##############################
all_selected <- function(data_row){
  #Function returns a binary if the sum of name_# section has all been selected.
  
  #Storing the name_# selection using paste's ability to iterate when using
  #  vectors to select all the names. as.integer is necessary as for some reason
  #  the binaries were coming out as characters.
  workbench <- as.integer(data_row[paste0("name_",c(1:15))])
  
  #Returning a binary 1 the summation is 15, 0 if not.
  return(ifelse(sum(workbench) == 15, 1, 0))
}

dup_selected <- function(data_row){
  #Function returns the set of names which are redundant within a set of names,
  #  checks their locations within name_# section, and returns values if some of
  #  these names were selected. If not, returns an NA.
  
  #Seperating up selections for easier access.
  sel_names  <- unlist(data_row[paste0("name",c(1:15))])
  selections <- unlist(data_row[paste0("name_",c(1:15))])
  
  #Finds duplicate names. Use toupper to correct for any capitalization changes.
  #  If someone accidentally puts a space in here there is no way to find out
  #  otherwise.
  dupes <- duplicated(toupper(sel_names), "")
  
  #Quick ifelse block that first checks if there are any duplicate names. If so,
  #  then it checks locations of the duplicate names and checks if any are 1's. If
  #  so, it returns the values that are 1's. Otherwise it will return NA's.
  if(any(dupes)){
    other_thing <- selections[dupes]
    thing <- as.logical(as.integer(other_thing))
    if(any(thing)){
      return(names(other_thing)[thing])
    }else{return(NA)}
  }else{return(NA)}
}

dup_unselected <- function(data_row){
  #Function returns the set of names which are not redundant within a set of names,
  #  checks their locations within name_# section, and returns values if some of
  #  these names were not selected. If not, returns an NA.
  
  #Seperating up selections for easier access.
  sel_names  <- unlist(data_row[paste0("name",c(1:15))])
  selections <- unlist(data_row[paste0("name_",c(1:15))])
  
  #Finds duplicate names, inverses them with !. Use toupper to correct for any
  #  capitalization changes. If someone accidentally puts a space in here there is
  #  no way to find out otherwise.
  dupes <- ((!duplicated(toupper(sel_names))) & (sel_names != ""))
  
  #Quick ifelse block that first checks if there are any unique names. If so,
  #  then it checks locations of the duplicate names and checks if any are 1's. If
  #  so, it returns the values that are 1's. Otherwise it will return NA's.
  if(any(dupes)){
    other_thing <- selections[dupes]
    thing <- as.logical(as.integer(other_thing))
    if(any(!thing) & sum(as.integer(selections)) <= 10){
      return(names(other_thing)[!thing])
    }else{return(NA)}
  }else{return(NA)}
}

blank_selected <- function(data_row){
  sel_names  <- unlist(data_row[paste0("name",c(1:15))])
  selections <- as.integer(unlist(data_row[paste0("name_",c(1:15))]))
  
  sel_blank <- as.logical(selections) & sel_names == ""
  
  if(any(sel_blank)){
    return(names(sel_names)[sel_blank])
  }else{
    return(NA)
  }
}

list_assigner <- function(error_frame, storage_list, variable_name){
  output_frame <- data.frame(study_id = error_frame$study_id)
  
  for(i in 1:length(storage_list)){
    if(any(is.na(storage_list[[i]]))){
      output_frame[i,paste0(variable_name, 1)] <- storage_list[[i]]
    }else{
      for(j in 1:length(storage_list[[i]])){
        output_frame[i,paste0(variable_name, j)] <- storage_list[[i]][j]
      }
    }
  }
  
  return(output_frame)
}

######################### Putting it all together #############################

#Creating Storage Dataframe

error_frame <- data.frame("study_id" = dataset$study_id, "Error_Present" = NA,
                          "Duplicate_Names" = NA, "Unselected_names" = NA,
                          "Selected_Blanks" = NA)

#Creating Storage Variables

error_all <- apply(name_select, 1, all_selected)

error_dup <- apply(name_select, 1, dup_selected)

error_uns <- apply(name_select, 1, dup_unselected)

error_bla <- apply(name_select, 1, blank_selected)

#Creating dataframes containing info on each variable

error_dataframes_list <- list(
  error_frame = error_frame,
  error_all = list_assigner(error_frame, error_all, "Error_All_"),
  error_dup = list_assigner(error_frame, error_dup, "Duplicate_Names_"),
  error_uns = list_assigner(error_frame, error_uns, "Unselected_Names_"),
  error_bla = list_assigner(error_frame, error_bla, "Selected_Blanks_")
)

error_dataframes_list[[error_all]]$Error_All_1 <-
  factor(error_all$Error_All_1, levels = c(1,0))
levels(error_dataframes_list[[error_all]]$Error_All_1) <-
  c("All Names Selected", "No Error")


#Merging it all together
error_frame <- join_all(error_dataframes_list, by = "study_id")


#Making the error present binary and errors binaries

possible_errors <- c("Duplicate_Names_1", "Unselected_Names_1",
                     "Selected_Blanks_1")

locations <- grep(paste(possible_errors, collapse = '|'), names(error_frame))

error_bins <- as.data.frame(apply(error_frame[,locations], 2, is.na))

bins_names <- c("Duplicate_Names", "Unselected_names", "Selected_Blanks")

colnames(error_bins) <- bins_names

for(i in bins_names){
  error_frame[i] <- error_bins[i]
}


error_frame$Error_Present <- apply(apply(error_frame[,locations], 2, is.na), 1, any)

#Converting all NA's to blanks for readability
error_frame[is.na(error_frame)] <- ""

#Exporting it out
write.csv(error_frame, "error_frame.csv")
