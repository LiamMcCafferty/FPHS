rm(list = ls())

setwd("~/Desktop/q1_test_data")

dataset <- read.csv("q1_test_data.csv")

#Internal Datadictionary:
#
#persnet_id : unique ID attached to each participant. ID is shared between Q1 and persnet
#dob : date of birth of respondent.
#dob_etl : date of birth of respondent correct value
#startplay_age : age of player when they started playing football (not professional)
#startplay_age_etl : age of player when they started playing football (correct)
#numb_season : number of seasons played.
#numb_season_etl : number of seasons played (corrected).
#position___1:position___10 : position player has played (checkbox, not binary)
#  c("Offensive Line", "Defensive Line", "Linebacker", "Defensive Back",
#    "Running Back", "Wide Receiver", "Tight End", "Quarterback",
#    "Kicker/Punter", "Special Teams")
#position___1_etl:position___10_etl : position player has played (checkbox, binary)
##Promis Global Health Questions##
#global1 : Q14 In general, would you say your health is:
#global2 : Q15 In general, would you say your quality of life is:
#global3 : Q16 In general, how would you rate your physical health:
#global4 : Q17 In general how would you rate your mental health, including your mood and ability to think?
#global5 : Q18 In general, how would you rate your satisfaction with your social activities and relationships?
#global6 : Q19 To what extent are you able to carry out everyday activities such as walking, climbing stairs, or carrying groceries?
#global7 : Q20 In the past 7 days, how would you rate your pain on average?
#global8 : Q21 In the past 7 days, how would you rate your fatigue on average?
#global10 : Q22 In the past 7 days, how ofter have you been bothered by emotional problems such as feeling anxious, depressed or irritable.
#Possible answers: 5=Excellent, 4=Very Good, 3=Good, 2=Fair, 1=Poor
#nqcog64 : Q46 In the past 7 days, I had to read something several times to understand it
#nqcog65 : Q47 In the past 7 days, I had trouble keeping track of what I was doing if I was interrupted
#nqcog66 : Q48 In the past 7 days, I had difficulty doing more than one thing at a time
#nqgoc68 : Q49 In the past 7 days, I had trouble remembering new information, like phone numbers or simple instructions
#nqcog72 : Q50 In the past 7 days, I had trouble thinking clearly
#nqcog75 : Q51 In the past 7 days, my thinking was slow
#nqcog77 : Q52 In the past 7 days, I had to work really hard to pay attention or I would make a mistake
#nqcog80 : Q53 In the past 7 days, I had trouble concentration
#nqcog67_editted : Q54 In the past 7 days, I had trouble remembering whether I did things I was supposed to do
#nqcog84 : Q55 In the past 7 days, I had trouble making decisions
#nqcog86 : Q56 In the past 7 days, I had trouble planning out steps of a task.
#Possible answers: 5=Never, 4=Rarely (once), 3=Sometimes (2-3 times), 2=Often (about once a day), 1=Very Often (Several times a day)


ages <- select(dataset, startplay_age:startplay_age_etl)












